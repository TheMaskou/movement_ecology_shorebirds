## ----style legend, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = FALSE, results = 'hide'----
# Source the script into that environment
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_3.qmd"), 
                   output = tempfile(fileext = ".R"),   
                   quiet = TRUE))       


## ----1 packages, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, , results = 'hide', include = FALSE----

library(dplyr)
library(here)
library(forcats) 
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)

# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) 

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 

# Receivers activity
sql.motus <- DBI::dbConnect(RSQLite::SQLite(), here::here("qmd", "chapter_1", "data", "project-294.motus"))
recv.act <- tbl(sql.motus, "activity")  %>% 
  collect() %>% 
  as.data.frame() %>%
  rename(deviceID = "motusDeviceID") %>%
  filter(deviceID %in% unique(recv$deviceID)) %>% # keep our deployed antennas only 
  
  # Set the time properly - IMPORTANT
  mutate(date = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC")),
         dateAus = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC"), 
                             tz = "Australia/Sydney")) 



## ----packages,  message = FALSE, warning = FALSE, eval = FALSE, echo = TRUE----
# 
# library(dplyr)
# library(here)
# library(forcats)
# library(ggplot2)
# library(lubridate)
# library(tidyr)
# library(purrr)


## ----load,  message = FALSE, warning = FALSE, eval = FALSE, echo = TRUE-------
# 
# recv.act <- tbl(sql.motus, "activity")  %>%
#   collect() %>%
#   as.data.frame() %>%
#   rename(deviceID = "motusDeviceID") %>%
#   # keep our deployed antennas only
#   filter(deviceID %in% unique(recv$deviceID)) %>%
#   # Set the time properly - IMPORTANT
#   mutate(date = as_datetime(as.POSIXct(hourBin* 3600,
#                                        origin = "1970-01-06",
#                                        tz = "UTC")),
#          dateAus = as_datetime(as.POSIXct(hourBin* 3600,
#                                           origin = "1970-01-06",
#                                           tz = "UTC"),
#                                tz = "Australia/Sydney"))
# 


## ----2 check the data,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

head(recv.act)



## ----example,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE-----

table(is.na(recv.act$pulseCount), recv.act$numTags) 



## ----clarifying stations,  message = FALSE, warning = FALSE, eval = TRUE------

# Sort the terminated serno (if terminated, ie. one box has been removed from one antenna site, and a date comes along recv$timeEndAus variable)
recv.act.term <- recv.act %>%
  left_join(recv %>% 
              filter(!is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Sort the still currently running serno
recv.act.runn <- recv.act %>%
  left_join(recv %>% 
              filter(is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Merging in one data-set to use Station's name further + pick-up the rounded hours
recv.act <- bind_rows(recv.act.runn, recv.act.term) %>%
  mutate(hour_dt = floor_date(dateAus, "hour"))

# Providing helpful variables
recv <- recv %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno),
         lisStart = timeStartAus,
         lisEnd = if_else(
           is.na(timeEndAus), # means the station is still running since the last data downloading
           with_tz(Sys.time(), "Australia/Sydney"),
           with_tz(as_datetime(timeEndAus, tz = "UTC"), "Australia/Sydney")) )



## ----2 Extracting listening periods,  message = FALSE, warning = FALSE, eval = TRUE----

# Generating hourly sequences per SernoStation from start to end dates of the deviceID at particular sites
recv_hours <- recv %>%
  select(recvDeployName, deviceID, SernoStation, lisStart, lisEnd) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = floor_date(lisStart, unit = "hour"),
                            to = floor_date(lisEnd, unit = "hour"),
                            by = "hour")) ) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup()

# Giving operational and not-operational hours by joining same sequences from recv.act tbl and adding operational = TRUE when existing values
recv_hours <- recv_hours %>%
  left_join(recv.act %>%
              distinct(SernoStation, hour_dt) %>%
              mutate(operational = TRUE),
            by = c("SernoStation", "hour_dt")) %>%
  mutate(operational = if_else(is.na(operational), FALSE, TRUE))

# Relaying on the Station name on its own only (consistent values through SernoStation var)
recv.act$Station <- sub("_SG-.*", "", recv.act$SernoStation)
recv$Station <- sub("_SG-.*", "", recv$SernoStation)
recv_hours$Station <- sub("_SG-.*", "", recv_hours$SernoStation)



## ----table MOTUS array survey effort,  message = FALSE, warning = FALSE, eval = TRUE----

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



## ----table survey effort,  message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----
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



## ----2 hour MOTUS array survey effort,  message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

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



## ----motus_survey_h, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "100%"----
motus_survey_h


## ----2 day MOTUS array survey effort,  message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

# Plot (day detailed)
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

# Adding Birds 
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



## ----motus_survey_d, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "100%"----
motus_survey_d


## ----2 station MOTUS array survey effort plot, echo = FALSE, eval = TRUE, results = 'asis'----

# Split the detection data by StationP
data_split <- split(data_all_plot, data_all_plot$StationP)

# Make sure your recv.status also has start/end per StationP
recv_split <- split(recv.status, recv.status$StationP)

# Create a named list of plots, one per station
plots_per_station <- imap(data_split, ~ {
  station_data <- .x
  station_name <- .y
  effort_data <- recv_split[[station_name]]
  
tag_order <- station_data %>%
    distinct(Band.ID, speciesEN) %>%
    arrange(speciesEN, Band.ID) %>%
    pull(Band.ID)
  
# Create a factor with levels ordered by speciesEN grouping
station_data <- station_data %>%
    mutate(Band.ID_ordered = factor(Band.ID, levels = tag_order))

ggplot() +
  # Vertical bands for survey effort periods
  geom_rect(data = effort_data, 
            aes(xmin = start_hour, xmax = end_hour, ymin = -Inf, ymax = Inf),
            fill = "grey80", alpha = 0.3) +
  
    # Points for detections with ordered Band.ID
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

# Now you can print each plot in your Quarto chunk by looping over the list
  for (station_data in names(plots_per_station)) {
    cat("\n\n## ", station_data, "\n\n") 
    print(plots_per_station[[station_data]]) 
    flush.console() #force immediate output
  }


