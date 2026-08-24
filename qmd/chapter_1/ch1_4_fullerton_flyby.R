# ============================================================
# Fullerton Entrance - Flyby vs Use, Direction & Mudflat Visit Duration
# Extracted R code (chunks only) from ch1_4_fullerton_flyby.md
# Assumes visit_duration, tide_data, data_all, recv, shorebird_class
# already exist in the environment (source ch1_3.qmd first).
# ============================================================

# Source the previous chapter script into this environment
# (this must produce, among others: visit_duration, tide_data, data_all, recv, shorebird_class)
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_12.qmd"),
                   output = tempfile(fileext = ".R"),
                   quiet = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(mixtools)
library(lubridate)
library(gt)


# 
# 
# 
# # Birds
# data_all <- readRDS(
#   tail(sort(list.files(
#     here::here("qmd", "chapter_1", "data", "motus"),
#     pattern = "-data\\.rds$", full.names = TRUE
#   )), 1)) 
# 
# # Receivers info
# recv <- readRDS(
#   tail(sort(list.files(
#     here::here("qmd", "chapter_1", "data", "motus"),
#     pattern = "-recv-info\\.rds$", full.names = TRUE
#   )), 1)) 
# 
# # Receivers activity
# sql.motus <- DBI::dbConnect(RSQLite::SQLite(), here::here("qmd", "chapter_1", "data", "project-294.motus"))
# recv.act <- tbl(sql.motus, "activity")  %>% 
#   collect() %>% 
#   as.data.frame() %>%
#   rename(deviceID = "motusDeviceID") %>%
#   filter(deviceID %in% unique(recv$deviceID)) %>% # keep our deployed antennas only 
#   
#   # Set the time properly - IMPORTANT
#   mutate(date = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC")),
#          dateAus = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC"), 
#                                tz = "Australia/Sydney")) 
# 
# # Tide
# tide_data <- readRDS(here("qmd", "chapter_1", "data", "tides", "tideData.rds"))
# 









focal_station    <- "Fullerton Entrance"   # <- directional antenna; rename here if needed
flyby_window_min <- 30                     # minutes before/after Fullerton detection to look for
                                            # a detection at another station
tide_lookahead_h <- 2                      # hours ahead of an "unknown" flyby used to infer
                                            # direction from tide state

stopifnot(focal_station %in% unique(visit_duration$recvDeployName))

fc_visits <- visit_duration %>%
  filter(recvDeployName == focal_station) %>%
  mutate(duration_min = duration_h * 60)

# order components by mean (component 1 = short/flyby, component 2 = long/actual use)
set.seed(123)
mix_fc <- normalmixEM(
  log(fc_visits$duration_min + 1),   # +1 to avoid log(0)
  k = 2, 
  maxit = 5000, 
  epsilon = 1e-08)
comp_order_fc <- order(mix_fc$mu)
post_short    <- mix_fc$posterior[, comp_order_fc[1]]
cut_df <- data.frame(duration_h = fc_visits$duration_h,
                     p_short = post_short) %>%
  arrange(duration_h)

flyby_cutpoint_h <- cut_df$duration_h[which(cut_df$p_short < 0.5)[1]]
cat("Flyby / actual-use cutpoint at", focal_station, ":",
    round(flyby_cutpoint_h, 2), "hours (~", round(flyby_cutpoint_h * 60, 1), "min)\n")

fc_visits <- fc_visits %>%
  mutate(visit_type = if_else(duration_h < flyby_cutpoint_h, "flyby", "actual_use"))

ggplot(fc_visits, aes(x = duration_h, fill = visit_type)) +
  geom_histogram(binwidth = 0.1, color = "white", boundary = 0) +
  geom_vline(xintercept = flyby_cutpoint_h, color = "red", linetype = "dashed", linewidth = 0.7) +
  scale_fill_manual(values = c(flyby = "#E7298A", actual_use = "#66A61E")) +
  coord_cartesian(xlim = c(0, 5)) +
  labs(x = "Visit duration (hours)", y = "Count", fill = "Visit type",
       title = paste("Flyby vs actual-use classification -", focal_station),
       subtitle = paste0("Cut-point = ", round(flyby_cutpoint_h * 60, 1), " min (mixture model)")) +
  theme_minimal(base_size = 13)

flyby_visits <- fc_visits %>% filter(visit_type == "flyby")

all_visits_dt   <- as.data.table(visit_duration)
other_visits_dt <- all_visits_dt[recvDeployName != focal_station]
flyby_dt        <- as.data.table(flyby_visits)

## --- BEFORE match: closest other-station visit ending <= window before flyby start ---
before_cand <- other_visits_dt[flyby_dt, 
                               on = "Band.ID", 
                               allow.cartesian = TRUE,
  .(Band.ID, visitID = i.visitID, 
    fc_start = i.visitStart,
    other_station = x.recvDeployName, 
    other_end = x.visitEnd)]

before_cand <- before_cand[!is.na(other_end) & 
                             other_end <= fc_start &
                             as.numeric(difftime(fc_start, other_end, units = "mins")) <= flyby_window_min]

before_flag <- before_cand[order(Band.ID, 
                                 visitID, 
                                 -as.numeric(other_end))][
  , .SD[1], by = .(Band.ID, visitID)][
  , .(Band.ID, 
      visitID, 
      before_station = other_station, 
      before_end = other_end)]



## --- Fly by directions ---
flyby_direction <- flyby_visits %>%
  left_join(as.data.frame(before_flag), by = c("Band.ID", "visitID")) %>%
  left_join(as.data.frame(after_flag),  by = c("Band.ID", "visitID")) %>%
  mutate(
    has_before = !is.na(before_end),
    has_after  = !is.na(after_start),
    direction = case_when(
      has_before & has_after  ~ "return_flight",
      has_before & !has_after ~ "to_mudflat",
      !has_before & has_after ~ "from_mudflat",
      TRUE                    ~ "unknown"))

all_visits_sorted <- visit_duration %>% arrange(Band.ID, visitStart)
get_next_visit_start <- function(band, after_time) {
  cand <- all_visits_sorted %>% filter(Band.ID == band, visitStart > after_time)
  if (nrow(cand) == 0) return(as.POSIXct(NA))
  min(cand$visitStart)
}
get_prev_visit_end <- function(band, before_time) {
  cand <- all_visits_sorted %>% filter(Band.ID == band, visitEnd < before_time)
  if (nrow(cand) == 0) return(as.POSIXct(NA))
  max(cand$visitEnd)
}
# NOTE: rowwise lookups below are O(n) per row and fine for exploratory work; for large
# datasets replace with a data.table rolling join (roll = -Inf / roll = Inf) - see Suggestions.

flyby_direction <- flyby_direction %>%
  rowwise() %>%
  mutate(
    mudflat_duration_min = case_when(
      direction == "return_flight" ~ as.numeric(difftime(after_start, before_end, units = "mins")),
      direction == "to_mudflat"    ~ as.numeric(difftime(get_next_visit_start(Band.ID, visitEnd), visitEnd, units = "mins")),
      direction == "from_mudflat"  ~ as.numeric(difftime(visitStart, get_prev_visit_end(Band.ID, visitStart), units = "mins")),
      TRUE ~ NA_real_),
    duration_censored = direction %in% c("to_mudflat", "from_mudflat")) %>%
  ungroup()

tide_dt <- as.data.table(tide_data)[, .(tideDateTimeAus, tideCategory)]
setkey(tide_dt, tideDateTimeAus)

infer_tide_direction <- function(start_time, lookahead_h = tide_lookahead_h) {
  hrs    <- seq(from = start_time, by = "hour", length.out = lookahead_h + 1)
  hrs_dt <- data.table(hour_dt = hrs)
  matched <- tide_dt[hrs_dt, on = .(tideDateTimeAus = hour_dt), roll = "nearest"]
  n_high <- sum(grepl("High", matched$tideCategory))
  n_low  <- sum(grepl("Low",  matched$tideCategory))
  if (n_high > n_low) return("from_mudflat_tide")
  if (n_low > n_high) return("to_mudflat_tide")
  "ambiguous_tide"
}

unknown_flyby <- flyby_direction %>%
  filter(direction == "unknown") %>%
  rowwise() %>%
  mutate(inferred_direction = infer_tide_direction(visitStart)) %>%
  ungroup()

flyby_direction <- flyby_direction %>%
  left_join(unknown_flyby %>% select(Band.ID, visitID, inferred_direction),
            by = c("Band.ID", "visitID")) %>%
  mutate(direction_final = if_else(direction == "unknown" & !is.na(inferred_direction),
                                    inferred_direction, direction))

fc_classified <- fc_visits %>%
  left_join(
    flyby_direction %>%
      select(Band.ID, visitID, direction, direction_final,
             mudflat_duration_min, duration_censored),
    by = c("Band.ID", "visitID")) %>%
  mutate(direction_final = if_else(visit_type == "actual_use", "site_use", direction_final))

ggplot(flyby_direction, aes(x = direction_final, fill = direction_final)) +
  geom_bar() +
  labs(x = "Direction", y = "Number of flyby events",
       title = paste("Direction of flyby detections at", focal_station)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))

# ggplot(flyby_direction %>% filter(!is.na(mudflat_duration_min), mudflat_duration_min >= 0),
#        aes(x = direction, y = mudflat_duration_min / 60, fill = duration_censored)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(width = 0.15, alpha = 0.4, size = 1) +
#   scale_fill_manual(values = c(`FALSE` = "#66A61E", `TRUE` = "grey70"),
#                      labels = c("bounded both sides", "bounded one side only (censored)")) +
#   labs(x = "Direction", y = "Estimated mudflat visit duration (hours)",
#        fill = "",
#        title = "Estimated mudflat visit duration by flight direction") +
#   theme_minimal(base_size = 13) +
#   theme(legend.position = "bottom")


flyby_direction <- flyby_direction %>%
  mutate(
    direction_class = case_when(
      direction_final %in% c("from_mudflat", "from_mudflat_tide") ~ "from",
      direction_final %in% c("to_mudflat", "to_mudflat_tide")     ~ "to",
      direction_final == "return_flight"                          ~ "return",
      TRUE                                                        ~ "other"
    ),
    direction_class = factor(direction_class, levels = c("from", "to", "return", "other"))
  )

ggplot(flyby_direction, aes(x = direction_class, fill = direction_class)) +
  geom_bar() +
  facet_wrap(~ speciesEN) +
  labs(
    x = "Direction class",
    y = "Number of flyby events",
    title = paste("Direction of flyby detections at", focal_station)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )

unknown_flyby %>%
  mutate(hour_of_day = hour(visitStart) + minute(visitStart) / 60) %>%
  ggplot(aes(x = hour_of_day, fill = inferred_direction)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 0) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  labs(x = "Hour of day (local)", y = "Count",
       fill = "Tide-inferred direction",
       title = "Tide-inferred direction of unresolved flybys, by time of day") +
  theme_minimal(base_size = 13)

fc_classified %>%
  mutate(direction_final = replace_na(direction_final, "NA")) %>%
  group_by(visit_type, direction_final) %>%
  summarise(
    n_events        = n(),
    n_birds         = n_distinct(Band.ID),
    median_duration_h = round(median(duration_h), 2),
    .groups = "drop") %>%
  arrange(visit_type, direction_final) %>%
  gt() %>%
  tab_header(title = paste("Summary -", focal_station)) %>%
  opt_align_table_header(align = "left") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())
