source(here::here("R_callum", "globals.R"))

# ==== Setup ====

library(dplyr)
library(here)
library(forcats)
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)

## ---- Load Data ----

# All bird detections across the array
data_all <- readRDS(path_detection_data)

# Receiver deployment metadata (one row per deployment of a device at a site)
recv <- readRDS(path_recv_info)

sql.motus <- DBI::dbConnect(RSQLite::SQLite(), path_motus_database)

# ==== Activity Table ====
#
# The "activity" table from the Motus database records VHF radio activity per
# antenna per hour. Each row represents one hour-bin at one antenna.
#
# Key columns:
#   - hourBin:    hour index (hours since 1970-01-06) — converted to datetime below
#   - pulseCount: number of radio pulses detected (any VHF signal, not just tags)
#   - numTags:    number of unique tags detected in that hour
#   - deviceID:   the Raspberry Pi processor inside the SensorGnome receiver
#
# IMPORTANT: rows only exist when the station is actively listening. If a station
# is offline for an hour, there is simply no row. This means absence of a row =
# station was not operational during that hour.

recv.act <- tbl(sql.motus, "activity")  %>%
  collect() %>%
  as.data.frame() %>%
  rename(deviceID = "motusDeviceID") %>%
  filter(deviceID %in% unique(recv$deviceID)) %>% # keep our deployed antennas only

  # Convert hourBin (hours since 1970-01-06) to datetime
  mutate(date = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC")),
         dateAus = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC"),
                             tz = "Australia/Sydney"))

head(recv.act)

## ---- Diagnostics: pulseCount vs numTags ----

# Check the range of values for numTags, and whether there are any NA values
recv.act$numTags |> summary()

# Check the range of values for pulseCount, and whether there are NA values
recv.act$pulseCount |> summary()

# Cross-tabulate: how many hour-bins have tags but no pulses, or vice versa?
# Rows with numTags > 0 but pulseCount = NA are suspicious — a tag was detected
# but no radio pulses were recorded. This shouldn't normally happen.
recv.act %>%
  mutate(
    condition = case_when(
      numTags == 0 & is.na(pulseCount)  ~ "numTags=0, pulseCount=NA",
      numTags == 0 & pulseCount > 0     ~ "numTags=0, pulseCount>0",
      numTags > 0  & is.na(pulseCount)  ~ "numTags>0, pulseCount=NA",
      numTags > 0  & pulseCount > 0     ~ "numTags>0, pulseCount>0"
    )
  ) %>%
  count(condition)

# Keep a copy for comparison
recv.act.raw <- recv.act

# TODO: figure out why there are rows with tag detections but no radio pulses

# Diagnostic - check the number of hour bins that no pulses were detected (
# i.e., pulseCount = NA), per the number of tags detected during each hour bin.
table(is.na(recv.act$pulseCount), recv.act$numTags)

# is.na(recv.act$pulseCount) produces TRUE/FALSE for each row, where
# TRUE = pulseCount is NA | FALSE = pulseCount is not NA


# ==== Link Activity to Receiver Deployments ====
#
# PROBLEM: The activity table only has deviceID (the physical Raspberry Pi),
# not which station it was deployed at. The same deviceID can be moved between
# stations over time (e.g., swapped for repairs), and a station can have
# different devices over its lifetime. So we need to match each activity row
# to the correct deployment (= which device was at which station, when).
#
# APPROACH: Split receiver deployments into terminated (has an end date) and
# active (no end date), join each group to the activity table separately,
# then recombine. This avoids a naive join that would incorrectly match
# activity rows to deployments that weren't active at that time.
#
# CAVEAT: This split-join approach doesn't actually filter by date range —
# it just separates terminated from active deployments. If a deviceID had
# multiple terminated deployments at different stations, activity could still
# match to the wrong one. A date-range join would be more robust.

# Join activity to TERMINATED deployments (those with an end date)
recv.act.term <- recv.act %>%
  left_join(recv %>%
              filter(!is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Join activity to ACTIVE deployments (still running, no end date)
recv.act.runn <- recv.act %>%
  left_join(recv %>%
              filter(is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Combine both and round to the hour
recv.act <- bind_rows(recv.act.runn, recv.act.term) %>%
  mutate(hour_dt = floor_date(dateAus, "hour"))

## ---- SernoStation and Listening Period ----

# Add SernoStation and listening period bounds to the receiver metadata.
# lisEnd = deployment end date, or now if the station is still running.
recv <- recv %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno),
         lisStart = timeStartAus,
         lisEnd = if_else(
           is.na(timeEndAus), # means the station is still running since the last data downloading
           with_tz(Sys.time(), "Australia/Sydney"),
           with_tz(as_datetime(timeEndAus, tz = "UTC"), "Australia/Sydney")) )

# SernoStation = station name + serial number (e.g. "Ash Island_SG-AB12RPI3CD34").
# It is NOT redundant with deployID:
#   - deployID is a unique numeric ID per deployment (good for DB lookups, opaque)
#   - SernoStation is human-readable and encodes WHICH device was at WHICH site
#   - Later, Station is derived by stripping the serno suffix (line ~136)
# This diagnostic checks whether SernoStation maps 1:1 to deployID in our data:
recv |> group_by(SernoStation) |> summarise(n_deploy = n_distinct(deployID))


# ==== Generate Expected Listening Hours ====
#
# For each receiver deployment (SernoStation), generate one row per hour
# for its full deployment window [lisStart, lisEnd]. This is the set of
# hours the station SHOULD have been listening. We then compare against
# actual activity to determine which hours it truly was operational.

recv_hours <- recv %>%
  select(recvDeployName, deviceID, SernoStation, lisStart, lisEnd) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = floor_date(lisStart, unit = "hour"),
                            to = floor_date(lisEnd, unit = "hour"),
                            by = "hour")) ) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup()

# Mark each hour as operational (TRUE) or not (FALSE) by checking whether
# the activity table has a matching record for that SernoStation + hour.
# No match = station was offline/not recording during that hour.
recv_hours <- recv_hours %>%
  left_join(recv.act %>%
              distinct(SernoStation, hour_dt) %>%
              mutate(operational = TRUE),
            by = c("SernoStation", "hour_dt")) %>%
  mutate(operational = if_else(is.na(operational), FALSE, TRUE))

# Derive Station name by stripping the serno suffix from SernoStation.
# This groups all deployments at the same physical site together, regardless
# of which device was installed there at the time.
recv.act$Station <- sub("_SG-.*", "", recv.act$SernoStation)
recv$Station <- sub("_SG-.*", "", recv$SernoStation)
recv_hours$Station <- sub("_SG-.*", "", recv_hours$SernoStation)


# ==== Survey Effort Summary ====

# Metrics per station:
#   total_hours:        hours the station was deployed (operational or not)
#   operational_hours:  hours the station was actually listening
#   downtime_hours:     hours deployed but not listening
#   uptime_pct:         % of deployment time that was operational
#   cont_listening_eff: this station's share (%) of total operational hours
#                       across the entire array
#   surv_time_cover:    same as uptime_pct (operational / total per station)
uptime_summary <- recv_hours %>%
  group_by(Station) %>%
  summarise(
    total_hours = n(),
    operational_hours = sum(operational),
    downtime_hours = total_hours - operational_hours,
    uptime_pct = round(100 * operational_hours / total_hours), 2)  %>%
  mutate(cont_listening_eff = round(100 * operational_hours / sum(operational_hours), 1),
         surv_time_cover = round(100 * operational_hours / total_hours, 1)) %>%
  arrange(desc(uptime_pct)) %>%
  select(-c("2"))

## ---- Survey Effort Table ----

library(gt)
uptime_summary  %>%
  gt() %>%
  opt_align_table_header(align = "left")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Station)))


# ==== Plot: Array Overview (Hourly) ====
#
# Each black mark = one hour where the station had activity recorded,
# meaning it was operational (recording radio noise or detecting a tag).

motus_survey_h <- ggplot(recv_hours %>%
         filter(operational),
       aes(x = hour_dt, y = factor(Station))) +
  geom_segment(aes(
    x = hour_dt,
    xend = hour_dt + hours(1),
    y = Station,
    yend = Station
  ), color = "black", linewidth = 1) +
  scale_y_discrete(name = "Receiver Station") +
  scale_x_datetime(name = "Time") +
  theme_minimal() +
  ggtitle("Receiver Operational Periods per hours")

motus_survey_h


# ==== Plot: Array Overview (Daily) ====
#
# Consolidates operational hours into continuous "runs" — gaps >24h start
# a new segment. Each segment is drawn as a horizontal bar. Bird detections
# are overlaid as coloured points. Station labels include their contribution
# (%) to total survey effort.

# Identify continuous operational runs per station
recv.status <- recv_hours %>%
  filter(operational) %>%
  arrange(Station, hour_dt) %>%
  group_by(Station) %>%
  # Calculate gap (in hours) between consecutive operational hours
  mutate(gap_hours = as.numeric(difftime(hour_dt, lag(hour_dt), units = "hours")),
         # New run starts if gap > 24h or if first row (NA gap)
         run_group = cumsum(if_else(is.na(gap_hours) | gap_hours > 24, 1, 0))) %>%
  group_by(Station, run_group) %>%
  # Get the start and end datetime per run group
  summarise(start_hour = min(hour_dt),
            end_hour = max(hour_dt) + hours(1), # +1 hour to cover full period
            .groups = "drop") %>%
  left_join(uptime_summary %>% select(Station, cont_listening_eff), "Station") %>%
  mutate(StationP = paste0(Station, " (", round(cont_listening_eff, digits = 1), "%)"))

motus_survey_d <- ggplot(recv.status, aes(y = factor(StationP))) +
  geom_segment(aes(x = start_hour, xend = end_hour,
                   yend = factor(StationP)),
               color = "black", linewidth = 1) +

  scale_y_discrete(name = "") +
  scale_x_datetime(name = "Time",
                   date_breaks = "1 month",
                   date_labels = "%b",
                   sec.axis = dup_axis(breaks = seq(from = floor_date(min(recv.status$start_hour),
                                                                      "year") + months(3),
                                                    to = floor_date(max(recv.status$end_hour),
                                                                    "year") + months(3),
                                                    by = "1 year"),
                                       labels = function(x) format(x, "%Y"),
                                       name = NULL)) +

  theme_minimal() +
  theme(axis.text.y.left = element_text(face = "bold", vjust = 0.5, margin = margin(t = 5)),
        axis.text.x.top = element_text(face = "bold", vjust = 0.5, margin = margin(t = 5)),
        axis.text.x = element_text(size = 9)) +
  ggtitle("Receiver Operational Periods per days (contribution % at the survey effort - listening)")

## ---- Overlay Bird Detections ----

# Join detection data to StationP labels so birds appear on the correct y-axis
data_all_plot <- left_join(data_all %>%
                             select(Band.ID, recv, recvDeployName, timeAus, tideCategory, speciesEN),
                           recv.status %>%
                             rename(recvDeployName = Station) %>%
                             select(recvDeployName, StationP) %>%
                             unique(),
                           "recvDeployName")

motus_survey_d <- motus_survey_d +
  geom_point(
    data = data_all_plot ,
    aes(x = timeAus,
        y = factor(StationP),
        color = speciesEN),
    alpha = 0.7, size = 2) +
  scale_color_manual(name = "Species", values = species_colors)

motus_survey_d


# ==== Plot: Per-station Detail ====
#
# One plot per station. Grey bands = operational periods. Coloured dots =
# individual bird detections, with y-axis showing Band.ID ordered by species.

# Split the detection data by StationP
data_split <- split(data_all_plot, data_all_plot$StationP)

# Make sure your recv.status also has start/end per StationP
recv_split <- split(recv.status, recv.status$StationP)

# Create a named list of plots, one per station
plots_per_station <- imap(data_split, ~ {
  station_data <- .x
  station_name <- .y
  effort_data <- recv_split[[station_name]]

  # Order birds by species then Band.ID so same-species birds are grouped on y-axis
  tag_order <- station_data %>%
    distinct(Band.ID, speciesEN) %>%
    arrange(speciesEN, Band.ID) %>%
    pull(Band.ID)

  station_data <- station_data %>%
    mutate(Band.ID_ordered = factor(Band.ID, levels = tag_order))

  ggplot() +
    # Grey bands show when this station was operational
    geom_rect(data = effort_data,
              aes(xmin = start_hour, xmax = end_hour, ymin = -Inf, ymax = Inf),
              fill = "grey80", alpha = 0.3) +

    # Coloured points for each detection
    geom_point(data = station_data,
               aes(x = timeAus,
                   y = Band.ID_ordered,
                   color = speciesEN),
               alpha = 0.7, linewidth = 2) +
    scale_color_manual(
      values = species_colors,
      name = paste0("Species\n (n = ", n_distinct(station_data$Band.ID_ordered), " indiv. recorded)") ) +
    scale_y_discrete() +
    theme_bw() +
    labs(title = station_name,
         x = "Time (Aus)",
         y = "Band.ID") +
    theme(axis.text.y = element_text(size = 6),
          plot.title = element_text(hjust = 0.5))
})

# Print each station's plot
for (station_data in names(plots_per_station)) {
  cat("\n\n## ", station_data, "\n\n")
  print(plots_per_station[[station_data]])
  flush.console() #force immediate output
}
