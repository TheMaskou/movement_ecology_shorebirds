## ----style legend, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = FALSE, results = 'hide'----
# TODO: Replace this with import from .R file to avoid unnecessary knitting
# Source the script into that environment
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_3.qmd"), 
                   output = tempfile(fileext = ".R"),  
                   quiet = TRUE))     


## ----packages,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----
library(motus)
library(dplyr)
library(here)
library(forcats) 
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(readr)
library(bioRad)
library(hms)
library(dplyr)
library(ggplot2)
library(scales)
library(gt)
library(DBI)
library(RSQLite)


## -----------------------------------------------------------------------------
load(here("data", "motus_data.RData"))
sql.motus <- dbConnect(SQLite(), here("qmd", "chapter_1", "data", "motus", "project-294.motus"))


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


## ----my data, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, results = 'hide', include = FALSE----
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

# Tide
tide_data <- readRDS(here("qmd", "chapter_1", "data", "tides", "tideData.rds"))

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



## ----tide, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----
tide_data <- tide_data %>%
  
  arrange(tideDateTimeAus) %>%
  mutate(prev_time = lag(tideDateTimeAus),
         next_time = lead(tideDateTimeAus) ) %>%
  filter(!is.na(prev_time) & !is.na(next_time)) %>%
  
  # Get the duration of the tide centered around Peak 2 (P2), with start halfway between P1 and P2, and with end btw P2-P3
  mutate(duration_h = as.numeric(difftime(tideDateTimeAus, prev_time, units = "hours")/2 + 
                                    difftime(next_time, tideDateTimeAus, units = "hours")/2)) %>%
  
  # Rename for consistency
  rename(tideHighLow = high_low,
         timeAus = tideDateTimeAus,
         tideDiel = day_night) %>%
  select(timeAus, tideCategory, tideHighLow, tideDiel, duration_h, sunriseNewc, sunsetNewc) %>%
  
  # Factorise
  mutate(tideCategory = as_factor(tideCategory))


## ----Available vs Used RECV AVAILABLE, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

# Sort the terminated serno (if terminated, ie. one box removed from one antenna site, a date comes along)
# but still needed for accessing survey effort as the station is currently running with another serno 
recv.act.term <- recv.act %>%
  left_join(recv %>% 
              filter(!is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Sort the currently running serno
recv.act.runn <- recv.act %>%
  left_join(recv %>% 
              filter(is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Merging in one data-set to use Station's name further + pick-up the rounded hours
recv.act <- bind_rows(recv.act.runn, recv.act.term) %>%
  mutate(hour_dt = round_date(dateAus, "hour"))

# Providing helpful variables
recv <- recv %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno),
         lisStart = timeStartAus,
         lisEnd = if_else(
           is.na(timeEndAus), # means the station is still running since the last data downloading
           with_tz(Sys.time(), "Australia/Sydney"),
           with_tz(as_datetime(timeEndAus, tz = "UTC"), "Australia/Sydney")) )

# Generating hourly sequences per SernoStation from start to end dates of the deviceID at particular sites
recv_hours <- recv %>%
  select(recvDeployName, deviceID, SernoStation, lisStart, lisEnd) %>%
  group_by(SernoStation) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = round_date(lisStart, unit = "hour"),
                            to = round_date(lisEnd, unit = "hour"),
                            by = "hour")) ) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup()

# Simplify station variables (recvDeployName)
recv <- recv %>% 
  select(!recvDeployName) 
recv$recvDeployName <- sub("_SG-.*", "", recv$SernoStation)

recv_hours <- recv_hours %>% 
  select(!recvDeployName) 
recv_hours$recvDeployName <- sub("_SG-.*", "", recv_hours$SernoStation)



## ----Available vs Used RECV OFF PERIODS, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----


# Distinguish station from mixed recv + Giving operational variable (= TRUE when existing values from act table)
recv_hours <- recv_hours %>%
  left_join(recv.act %>%
              distinct(recvDeployName, hour_dt) %>%
              mutate(operational = TRUE),
            by = c("recvDeployName", "hour_dt")) %>%
  mutate(operational = if_else(is.na(operational), FALSE, TRUE))

# /!\ Due to unknown error? Have to set this manually
recv_hours <- recv_hours %>%
  mutate(operational = case_when(
    recvDeployName == "Fullerton Entrance" & 
      hour_dt > as.POSIXct("2023-04-02") & 
      hour_dt < as.POSIXct("2023-04-05") ~ FALSE,
    TRUE ~ operational))

# Summary table                                                                     
off_runs <- recv_hours %>% 
  arrange(recvDeployName, hour_dt) %>%
  group_by(recvDeployName) %>%
  mutate(off_run_id = consecutive_id(operational == FALSE)) %>%
  ungroup() %>%
  
  filter(operational == FALSE) %>%
  
  group_by(recvDeployName, off_run_id) %>%
  summarise(
    start_off = min(hour_dt),
    end_off = max(hour_dt),
    tot_off_hours = n(),
    .groups = "drop") %>%
  
  filter(tot_off_hours > 24)

# Unique recvDeployNames from off_runs
recv_names <- unique(recv_hours$recvDeployName)

# Split tide_data into a named list with one element per recvDeployName
tide_data_list <- setNames(vector("list", length(recv_names)), recv_names)

for(name in recv_names) {
  # Get off intervals for this recvDeployName
  intervals <- off_runs %>%
    filter(recvDeployName == name) %>%
    select(start_off, end_off)
  
  # Get deployment start and end dates for this recvDeployName
  deploy <- recv_hours %>%
    filter(recvDeployName == name) %>%
    summarise(
      lisStart = min(lisStart, na.rm = TRUE),
      lisEnd = max(lisEnd, na.rm = TRUE)
    )
  
  # Filter tide_data by deployment period
  td <- tide_data %>%
    filter(timeAus >= deploy$lisStart & timeAus <= deploy$lisEnd) %>%
    mutate(recvDeployName = name) 
  
  if(nrow(intervals) > 0) {
    # Vectorized exclusion of off intervals
    is_in_off <- sapply(td$timeAus, function(t) {
      any(t >= intervals$start_off & t <= intervals$end_off)
    })
    td <- td[!is_in_off, ]
  }
  tide_data_list[[name]] <- td
}

tide_data_df <- bind_rows(tide_data_list) %>%
  mutate(hour_dt = round_date(timeAus, unit = "hour"))            # AVAILABLE TIME (tide categories covering same time as recv survey effort) 

# Finalise data set for receiver ON with tidal data, per hour
total_recv_tide_data <- tide_data_df %>%
  group_by(recvDeployName) %>%
  
  # Expand per hour bin
  summarise(hour_seq = list(seq(min(hour_dt), max(hour_dt), by = "hour")), .groups = "drop") %>% 
  unnest(hour_seq) %>%
  rename(hour_dt = hour_seq) %>%
  
  # Match tide data to hourly grid
  # Hours WITHOUT tide data get NA values across all columns but we'll deal with this later
  left_join(tide_data_df, by = c("recvDeployName", "hour_dt")) 



## ----Available vs Used BIRD AVAILABLE TIME, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----


# Get the monitored period of each bird
period_sp <- data_all  %>%
  group_by(Band.ID) %>%
  reframe(DateAUS.Trap = first(DateAUS.Trap), 
          last_dateAus = max(dateAus),
          speciesEN = speciesEN) %>%
  unique()

# Expand one row per hours to each individual across its whole period (this is the available time)
bird_hours <- period_sp %>%
  group_by(Band.ID) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = as.POSIXct(ymd(DateAUS.Trap), tz = "UTC"),
                            to = as.POSIXct(last_dateAus, tz = "UTC"),
                            by = "hour"))) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup() 

# Duplicate in as many list as many stations
recv_names <- unique(recv_hours$recvDeployName)
bird_data_list <- setNames(vector("list", length(recv_names)), recv_names)

for(name in recv_names) {
  
  valid_hours_recv <- total_recv_tide_data %>%
    filter(recvDeployName == name) %>%
    pull(hour_dt)
  
  valid_hours_bird_recv <- bird_hours %>%
    filter(hour_dt %in% valid_hours_recv)
  
  valid_hours_bird_recv$name <- name
  
    
  bird_data_list[[name]] <- valid_hours_bird_recv
} 

# ADD TIDE TO AVAILABLE TIME
# The NA cols left over before from recv table, now also filtered out based on available time coming from bird periods table
get.tideIndex <- function(time){return(which.min(abs(tide_data$timeAus - time)))}

available_bird_recv_time <- bind_rows(bird_data_list) %>%
  mutate(hour_dt = force_tz(hour_dt, "Australia/Sydney")) %>%
  # Remove rows with ANY NA
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(tideIndex = map_dbl(hour_dt, get.tideIndex))

# Extract tide categories thanks to tide index (row order in available_bird_recv_time)
tide_values <- tide_data[available_bird_recv_time$tideIndex, c("tideCategory")]

# Merge tide categories to available_bird_recv_time and factorise the variables
available_bird_recv_time <- available_bird_recv_time %>%
  mutate(tideCategory = as_factor(tide_values$tideCategory))

# FINALISE AVAILABLE TIME 
available_bird_recv_time <- available_bird_recv_time %>%
  rename(recvDeployName = name) %>%
  group_by(Band.ID, speciesEN, recvDeployName, tideCategory)  %>%
  summarise(duration_h = n()) %>%
  mutate(tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
         tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) %>%
  select(Band.ID, speciesEN, recvDeployName, tideCategory, tideDiel, tideHighLow, duration_h)




## ----Available vs Used BIRD USED TIME, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

# USED TIME (amount of time each bird spent during each category of tide and at each station)
# Provide the burst interval value depending Lotek-nano tag model (scd)
data_bird <- data_all %>%
  mutate(burst_inter = ifelse(tagModel == "NTQB2-6-2", dseconds(7.1), dseconds(13.1))) %>%
  select(timeAus, sunriseNewc, sunsetNewc, tideCategory, tideHighLow, tideDiel, 
         speciesEN, tagModel, recvDeployName, recv,speciesSci, Band.ID, burst_inter)

used_bird_recv_time <- data_bird %>%
  group_by(Band.ID, speciesEN, recvDeployName, tideCategory)  %>%
  summarise(duration_sec = sum(burst_inter)) %>%
  mutate(duration_h = round(duration_sec / 3600, 0),
         tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
         tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) %>%
  select(Band.ID, speciesEN, recvDeployName, tideCategory, tideDiel, tideHighLow, duration_h)



## ----potential error, message = FALSE, warning = FALSE, echo = FALSE, eval = FALSE----
# table((used_bird_recv_time  %>%
#          filter(recvDeployName == "Curlew Point", speciesEN == "Far Eastern Curlew"))$tideCategory)
# used_bird_recv_time %>%
#   filter(recvDeployName == "Curlew Point", speciesEN == "Far Eastern Curlew") %>%
#   select(tideCategory, duration_h)
# 
# 
# table((available_bird_recv_time  %>%
#          filter(recvDeployName == "Curlew Point", speciesEN == "Far Eastern Curlew"))$tideCategory)
# available_bird_recv_time %>%
#   filter(recvDeployName == "Curlew Point", speciesEN == "Far Eastern Curlew") %>%
#   select(tideCategory, duration_h)


## ----above hundred, message = FALSE, warning = FALSE, echo = FALSE, eval = TRUE----

figure_plot <- left_join(available_bird_recv_time %>% 
                           group_by(recvDeployName, Band.ID, speciesEN) %>%
                           rename(available_t = "duration_h"),
                         used_bird_recv_time %>%
                           rename(used_t = "duration_h")) %>%
  mutate(rate_use = used_t*100/available_t)




above <- figure_plot %>%
  filter(rate_use > 100)
above  %>%
 ungroup() %>% 
  gt() %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) 

above100 <- above %>%
  filter(rate_use > 100 & rate_use < 140)
above140 <- above %>%
  filter(rate_use >= 140)



## ----Time Used Rate, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

figure_plot <- figure_plot %>%
  
  mutate(rate_use = ifelse(rate_use > 100 & rate_use < 140, 100, rate_use)) %>% # force 100-140 rate values to equal 100 %
  mutate(rate_use = ifelse(rate_use >= 140, NA, rate_use)) %>%                  # remove rates above 140 %
  
  mutate(speciesType = factor(shorebird_class[speciesEN], 
                               levels = c("migratory", "resident"))) %>%
  
  filter(!speciesEN %in% c("Masked Lapwing")) 



## ----tool, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----
# Track size sample
counts <- used_bird_recv_time %>%
      group_by(speciesEN) %>%
      summarise(n = n_distinct(Band.ID)) %>%
      mutate(label = paste0(speciesEN, " (n = ", n, ")"))
label_vec <- setNames(counts$label, counts$speciesEN)


## ----4 Used rate PLOT, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

# Tide and species groupings
tide_levels <- c("Low", "High")
species_types <- unique(shorebird_class)  

# Simplified function - no min_n_nonzero check
make_plot <- function(tide_level, species_type) {
  data_sub <- figure_plot %>%
    filter(tideHighLow == tide_level) %>%
    mutate(species_class = shorebird_class[speciesEN]) %>%
    filter(species_class == species_type)
  
  p <- ggplot(data_sub,
              aes(x    = factor(recvDeployName, levels = sort(unique(recvDeployName))),
                  y    = rate_use,
                  fill = tideDiel))
  
  # Always add boxplot for all available data
  p <- p + 
    geom_boxplot(outlier.shape = NA,
                 varwidth = FALSE,
                 position = position_dodge(width = 0.8, preserve = "single")) +
    
    # Always show individual points
    geom_point(aes(shape = tideDiel), 
               position = position_dodge(width = 0.8),
               alpha = 1, size = 1.5,
               show.legend = FALSE) +
    scale_shape_manual(values = c("Diurnal" = 21, "Nocturnal" = 16)) +
    
    facet_wrap(~ speciesEN,
               labeller = labeller(speciesEN = label_vec)) +
    labs(x     = "Receiver Deployment",
         y     = "Rate of Use (%)",
         title = paste(ifelse(species_type == "migratory",
                              "Migratory species", "Resident species"),
                       "during", tide_level, "tide")) +
    coord_cartesian(ylim = c(0, 100)) +
    theme_minimal() +
    scale_fill_manual(values = c("Diurnal" = "white", "Nocturnal" = "darkgrey")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Generate all plots
plots_used_rate <- cross2(tide_levels, species_types) %>%
  purrr::map(~ make_plot(.x[[1]], .x[[2]]))



## ----stats for plots, message = FALSE,  warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

# General function to provide one table with stats values
make_summary_table <- function(tide_level, species_type) {
  data_sub <- figure_plot %>%
    filter(tideHighLow == tide_level) %>%
    mutate(species_class = shorebird_class[speciesEN]) %>%
    filter(species_class == species_type)
  
  summary_table <- data_sub %>%
    group_by(speciesEN, recvDeployName, tideDiel) %>%
    summarise(
      n_individual = n(),
      min = round(min(rate_use, na.rm = TRUE), 1),
      q1 = round(quantile(rate_use, 0.25, na.rm = TRUE),1),
      median = round(median(rate_use, na.rm = TRUE),1),
      q3 = round(quantile(rate_use, 0.75, na.rm = TRUE),1),
      max = round(max(rate_use, na.rm = TRUE),1),
      mean = round(mean(rate_use, na.rm = TRUE),1),
      sd = round(sd(rate_use, na.rm = TRUE),1),
      .groups = 'drop'
    ) %>%
    arrange(speciesEN, recvDeployName, tideDiel) %>%
    rename(Species = "speciesEN", Station = "recvDeployName", condition = "tideDiel")
  
  return(summary_table)
}

# One table per conditions
high_migratory <- make_summary_table("High", "migratory")
high_resident <- make_summary_table("High", "resident")
low_migratory <- make_summary_table("Low", "migratory")
low_resident <- make_summary_table("Low", "resident")


## ----4 Used rate 1, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
plots_used_rate[[4]]


## ----4 Used rate table, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
high_resident  %>%
  gt() %>%
  tab_header(title = "Statistics - High Tide Resident Species") %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Species))) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2)),
    locations = cells_body(
      rows = which(!duplicated(high_resident$Species))[-1]))


## ----4 Used rate 2, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_used_rate[[3]]


## ----4 Used rate table 2, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
low_resident  %>%
  gt() %>%
  tab_header(title = "Statistics - Low Tide Resident Species") %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Species))) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2)),
    locations = cells_body(
      rows = which(!duplicated(high_resident$Species))[-1]))


## ----4 Used rate 3, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
plots_used_rate[[2]]


## ----4 Used rate table 3, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
high_migratory  %>%
  gt() %>%
  tab_header(title = "Statistics - High Tide Migratory Species") %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Species))) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2)),
    locations = cells_body(
      rows = which(!duplicated(high_resident$Species))[-1]))


## ----4 Used rate 4, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_used_rate[[1]]


## ----4 Used rate table 5, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
low_migratory  %>%
  gt() %>%
  tab_header(title = "Statistics - Low Tide Migratory Species") %>%
  opt_align_table_header(align = "left") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Species))) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "darkgrey",
      weight = px(2)),
    locations = cells_body(
      rows = which(!duplicated(high_resident$Species))[-1]))


## ----Available vs Used PLOT, message = FALSE, warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

# Combine dataset
used_bird_recv_time$type <- "used"
available_bird_recv_time$type <- "available"
combined_data <- bind_rows(used_bird_recv_time, available_bird_recv_time) %>%
  mutate(fill_color = ifelse(type == "available", "available", speciesEN),
         station_type = interaction(recvDeployName, type, lex.order = TRUE))

# Plot function available vs. used
plot_by_tide <- function(tide_cat) {
  data_subset <- combined_data %>% filter(tideCategory == tide_cat)
  
  ggplot(data_subset, 
         aes(x = recvDeployName, y = duration_h, 
             fill = fill_color)) +
    
    geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = species_colors, name = "Type") +
    
    facet_wrap(~ speciesEN, scales = "free_y", 
               labeller = labeller(speciesEN = label_vec)) +

    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 8),
          strip.text.x = element_text(face = "plain", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "in"))  +
    
    coord_cartesian(ylim = c(0, NA)) +     
    labs(x = "Motus Stations",
         y = "Detection duration (hours)",
         title = paste("Tide Category:", tide_cat),
         fill = "Type",
         caption = "Used (colors) vs. Available (grey)")
  }

# Get unique tideCategory levels
tide_categories <- combined_data %>%
  filter(!is.na(tideCategory)) %>%
  pull(tideCategory) %>%
  unique()

# Generate a list of plots for all tide categories
plots_list <- purrr::map(tide_categories, plot_by_tide)



## ----4 1, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
plots_list[[1]]


## ----4 2, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list[[2]]


## ----4 3, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list[[3]]


## ----4 4, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list[[4]]


## ----4 Used PLOT, message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE, results = 'asis'----

# Plot function used ONLY
plot_used <- function(tide_cat) {
  data_subset <- used_bird_recv_time %>% filter(tideCategory == tide_cat)
  
  ggplot(data_subset, 
         aes(x = recvDeployName, y = duration_h, fill = speciesEN)) +
    
    geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = species_colors, name = "Species") +
    
    facet_wrap(~ speciesEN, scales = "free_y", 
               labeller = labeller(speciesEN = label_vec)) +
    
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.margin = margin(0, 0, 0, 0, "in"))  +
    
    coord_cartesian(ylim = c(0, NA), expand = FALSE) + 
    labs(x = "Motus stations",
         y = "Detection duration (hours)",
         title = paste("Tide Category:", tide_cat),
         fill = "Species")
  }

# Get unique tideCategory levels
tide_categories <- combined_data %>%
  filter(!is.na(tideCategory)) %>%
  pull(tideCategory) %>%
  unique()

# Generate a list of plots for all tide categories
plots_list_used <- purrr::map(tide_categories, plot_used)



## ----4 used 1, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
plots_list_used[[4]]


## ----4 used 2, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list_used[[1]]


## ----4 used 3, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list_used[[3]]


## ----4 used 4, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

plots_list_used[[2]]


## ----global, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----

# Bar plot that balances the data we got across tideCategory for each species
balance_table <- figure_plot %>%
  group_by(Band.ID, speciesEN, tideCategory) %>%
  summarise(total_used_t = sum(used_t, na.rm = TRUE),
            total_available_t = sum(available_t, na.rm = TRUE),
            total_rate_use = total_used_t*100/total_available_t) %>%
  ungroup() %>%
  
  # Choose order in plot
  mutate(tideCategory = factor(tideCategory, 
                               levels = c("Diurnal_High",
                                          "Diurnal_Low",
                                          "Nocturnal_High", 
                                          "Nocturnal_Low")))

  # Choose colors in plot
  custom_colors <- c("Nocturnal_Low" = "darkgrey",
                     "Nocturnal_High" = "darkgrey",
                     "Diurnal_Low" = "white",
                     "Diurnal_High" = "white")


## ----global rou, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
ggplot(balance_table, aes(x = tideCategory, y = total_rate_use, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 30)) + #300 if total_used_t
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(title = "Rate of Use - Total acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Rate of Use (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----global hours, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
# All species and tide categories 
ggplot(balance_table, aes(x = tideCategory, y = total_used_t, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 300)) +
  labs(title = "Total used - Total acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Total used (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = custom_colors, guide = "none")


## ----sp rou, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
ggplot(balance_table, aes(x = tideCategory, y = total_rate_use, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~speciesEN) +
  coord_cartesian(ylim = c(0, 30)) + #300 if total_used_t
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(title = "Rate of Use - Total acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Rate of Use (%)") +
  # labs(title = "Total used - Acquired data for tagged population depending Tide and Time",
  #      x = "Tide Category",
  #      y = "Total used (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----sp hours, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----
explore <- ggplot(balance_table, aes(x = tideCategory, y = total_used_t, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~speciesEN) +
  coord_cartesian(ylim = c(0, 300)) + 
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(title = "Total used - Total acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Total used (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
explore


## ----summary table sp lvl, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE, results = 'asis'----


# ══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 1 — Summaries au niveau receiver
# ══════════════════════════════════════════════════════════════════════════════

recv_summary_available <- available_bird_recv_time %>%
  group_by(speciesEN, recvDeployName) %>%
  summarise(
    n_birds              = n_distinct(Band.ID),
    total_hours          = sum(duration_h, na.rm = TRUE),
    hours_Nocturnal_Low  = sum(duration_h[tideCategory == "Nocturnal_Low"],  na.rm = TRUE),
    hours_Nocturnal_High = sum(duration_h[tideCategory == "Nocturnal_High"], na.rm = TRUE),
    hours_Diurnal_Low    = sum(duration_h[tideCategory == "Diurnal_Low"],    na.rm = TRUE),
    hours_Diurnal_High   = sum(duration_h[tideCategory == "Diurnal_High"],   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_Nocturnal_Low  = hours_Nocturnal_Low  / total_hours * 100,
    pct_Nocturnal_High = hours_Nocturnal_High / total_hours * 100,
    pct_Diurnal_Low    = hours_Diurnal_Low    / total_hours * 100,
    pct_Diurnal_High   = hours_Diurnal_High   / total_hours * 100
  )

recv_summary_used <- used_bird_recv_time %>%
  group_by(speciesEN, recvDeployName) %>%
  summarise(
    n_birds              = n_distinct(Band.ID),
    total_hours          = sum(duration_h, na.rm = TRUE),
    hours_Nocturnal_Low  = sum(duration_h[tideCategory == "Nocturnal_Low"],  na.rm = TRUE),
    hours_Nocturnal_High = sum(duration_h[tideCategory == "Nocturnal_High"], na.rm = TRUE),
    hours_Diurnal_Low    = sum(duration_h[tideCategory == "Diurnal_Low"],    na.rm = TRUE),
    hours_Diurnal_High   = sum(duration_h[tideCategory == "Diurnal_High"],   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_Nocturnal_Low  = hours_Nocturnal_Low  / total_hours * 100,
    pct_Nocturnal_High = hours_Nocturnal_High / total_hours * 100,
    pct_Diurnal_Low    = hours_Diurnal_Low    / total_hours * 100,
    pct_Diurnal_High   = hours_Diurnal_High   / total_hours * 100
  )

# ══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 2 — JOIN receiver-level (colonnes brutes _used/_avail conservées)
# ══════════════════════════════════════════════════════════════════════════════

recv_combined <- recv_summary_used %>%
  left_join(recv_summary_available, by = c("speciesEN", "recvDeployName"),
            suffix = c("_used", "_avail")) %>%
  mutate(
    # Colonnes formatées pour affichage
    total_hours          = paste0(round(total_hours_used, 1), " / ", round(total_hours_avail, 1), " (",round((total_hours_used * 100)/total_hours_avail, 1), " %)"),

    hours_Nocturnal_Low  = paste0(round(hours_Nocturnal_Low_used,  1), " / ", round(hours_Nocturnal_Low_avail,  1),
                                  " (", round(hours_Nocturnal_Low_used  * 100 / hours_Nocturnal_Low_avail,  1), " %)"),
    hours_Nocturnal_High = paste0(round(hours_Nocturnal_High_used, 1), " / ", round(hours_Nocturnal_High_avail, 1),
                                  " (", round(hours_Nocturnal_High_used * 100 / hours_Nocturnal_High_avail, 1), " %)"),
    hours_Diurnal_Low    = paste0(round(hours_Diurnal_Low_used,    1), " / ", round(hours_Diurnal_Low_avail,    1),
                                  " (", round(hours_Diurnal_Low_used    * 100 / hours_Diurnal_Low_avail,    1), " %)"),
    hours_Diurnal_High   = paste0(round(hours_Diurnal_High_used,   1), " / ", round(hours_Diurnal_High_avail,   1),
                                  " (", round(hours_Diurnal_High_used   * 100 / hours_Diurnal_High_avail,   1), " %)"),

    pct_Nocturnal_Low    = paste0(round(pct_Nocturnal_Low_used,  1)),
    pct_Nocturnal_High   = paste0(round(pct_Nocturnal_High_used, 1)),
    pct_Diurnal_Low      = paste0(round(pct_Diurnal_Low_used,    1)),
    pct_Diurnal_High     = paste0(round(pct_Diurnal_High_used,   1))
  ) %>%
  select(speciesEN, recvDeployName, n_birds_used, total_hours, 
         hours_Nocturnal_Low, hours_Nocturnal_High, hours_Diurnal_Low, hours_Diurnal_High,
         pct_Nocturnal_Low, pct_Nocturnal_High, pct_Diurnal_Low, pct_Diurnal_High,
         # Colonnes brutes pour seuils gt
         pct_Nocturnal_Low_used, pct_Nocturnal_High_used,
         pct_Diurnal_Low_used,   pct_Diurnal_High_used,
         # Colonnes brutes pour recalcul species total
         total_hours_used, total_hours_avail,
         hours_Nocturnal_Low_used,  hours_Nocturnal_Low_avail,
         hours_Nocturnal_High_used, hours_Nocturnal_High_avail,
         hours_Diurnal_Low_used,    hours_Diurnal_Low_avail,
         hours_Diurnal_High_used,   hours_Diurnal_High_avail)

# ══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 3 — Species TOTAL recalculé DEPUIS recv_combined (receivers filtrés)
# ══════════════════════════════════════════════════════════════════════════════

n_birds_species <- used_bird_recv_time %>%
  group_by(speciesEN) %>%
  summarise(n_birds_used = n_distinct(Band.ID), .groups = "drop")

species_combined <- recv_combined %>%
  group_by(speciesEN) %>%
  summarise(
    total_hours_used_sum  = sum(total_hours_used,          na.rm = TRUE),
    total_hours_avail_sum = sum(total_hours_avail,         na.rm = TRUE),
    hours_NL_used  = sum(hours_Nocturnal_Low_used,         na.rm = TRUE),
    hours_NL_avail = sum(hours_Nocturnal_Low_avail,        na.rm = TRUE),
    hours_NH_used  = sum(hours_Nocturnal_High_used,        na.rm = TRUE),
    hours_NH_avail = sum(hours_Nocturnal_High_avail,       na.rm = TRUE),
    hours_DL_used  = sum(hours_Diurnal_Low_used,           na.rm = TRUE),
    hours_DL_avail = sum(hours_Diurnal_Low_avail,          na.rm = TRUE),
    hours_DH_used  = sum(hours_Diurnal_High_used,          na.rm = TRUE),
    hours_DH_avail = sum(hours_Diurnal_High_avail,         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  left_join(n_birds_species, by = "speciesEN") %>%

  mutate(
    recvDeployName = "TOTAL",

    total_hours          = paste0(round(total_hours_used_sum, 1), " / ", round(total_hours_avail_sum, 1), " (", round((total_hours_used_sum * 100)/total_hours_avail_sum, 1), " %)"),

    hours_Nocturnal_Low  = paste0(round(hours_NL_used, 1), " / ", round(hours_NL_avail, 1),
                                  " (", round(hours_NL_used * 100 / hours_NL_avail, 1), " %)"),
    hours_Nocturnal_High = paste0(round(hours_NH_used, 1), " / ", round(hours_NH_avail, 1),
                                  " (", round(hours_NH_used * 100 / hours_NH_avail, 1), " %)"),
    hours_Diurnal_Low    = paste0(round(hours_DL_used, 1), " / ", round(hours_DL_avail, 1),
                                  " (", round(hours_DL_used * 100 / hours_DL_avail, 1), " %)"),
    hours_Diurnal_High   = paste0(round(hours_DH_used, 1), " / ", round(hours_DH_avail, 1),
                                  " (", round(hours_DH_used * 100 / hours_DH_avail, 1), " %)"),

    pct_Nocturnal_Low    = paste0(round(hours_NL_used / total_hours_used_sum * 100, 1)),
    pct_Nocturnal_High   = paste0(round(hours_NH_used / total_hours_used_sum * 100, 1)),
    pct_Diurnal_Low      = paste0(round(hours_DL_used / total_hours_used_sum * 100, 1)),
    pct_Diurnal_High     = paste0(round(hours_DH_used / total_hours_used_sum * 100, 1)),

    # Brutes pour seuils gt
    pct_Nocturnal_Low_used  = hours_NL_used / total_hours_used_sum * 100,
    pct_Nocturnal_High_used = hours_NH_used / total_hours_used_sum * 100,
    pct_Diurnal_Low_used    = hours_DL_used / total_hours_used_sum * 100,
    pct_Diurnal_High_used   = hours_DH_used / total_hours_used_sum * 100
  ) %>%
  select(speciesEN, recvDeployName, n_birds_used, total_hours,
         hours_Nocturnal_Low, hours_Nocturnal_High, hours_Diurnal_Low, hours_Diurnal_High,
         pct_Nocturnal_Low, pct_Nocturnal_High, pct_Diurnal_Low, pct_Diurnal_High,
         pct_Nocturnal_Low_used, pct_Nocturnal_High_used,
         pct_Diurnal_Low_used,   pct_Diurnal_High_used)

# ══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 4 — BIND + arrange
# ══════════════════════════════════════════════════════════════════════════════

# Réduire recv_combined aux colonnes finales seulement avant le bind
recv_combined_final <- recv_combined %>%
  select(speciesEN, recvDeployName, n_birds_used, total_hours, 
         hours_Nocturnal_Low, hours_Nocturnal_High, hours_Diurnal_Low, hours_Diurnal_High,
         pct_Nocturnal_Low, pct_Nocturnal_High, pct_Diurnal_Low, pct_Diurnal_High,
         pct_Nocturnal_Low_used, pct_Nocturnal_High_used,
         pct_Diurnal_Low_used,   pct_Diurnal_High_used)

final_table <- bind_rows(species_combined, recv_combined_final) %>%
  arrange(speciesEN, desc(recvDeployName == "TOTAL")) %>%
  mutate(across(
    c(hours_Nocturnal_Low, hours_Nocturnal_High, hours_Diurnal_Low, hours_Diurnal_High),
    ~ sub("^0(\\.0)? / ", "<  1 / ", .x)
  ))

# ══════════════════════════════════════════════════════════════════════════════
# ÉTAPE 5 — GT TABLE
# ══════════════════════════════════════════════════════════════════════════════

final_table %>%
  mutate(
    speciesEN      = toupper(speciesEN),
    recvDeployName = ifelse(recvDeployName == "TOTAL", "Total", recvDeployName)
  ) %>%
  gt(groupname_col = "speciesEN", rowname_col = "recvDeployName") %>%

  tab_header(
    title    = md("**Species Site Use Summary**"),
    subtitle = md("*Detected / Survey effort (Hours) per Tide Categories*")
  ) %>%

  tab_spanner(label = "Hours per Tide Category",
              columns = c(hours_Nocturnal_Low, hours_Nocturnal_High,
                          hours_Diurnal_Low, hours_Diurnal_High)) %>%
  tab_spanner(label = "Detection per Tide Category (%)",
              columns = c(pct_Nocturnal_Low, pct_Nocturnal_High,
                          pct_Diurnal_Low, pct_Diurnal_High)) %>%

  cols_label(
    n_birds_used         = md("**N Birds**"),
    total_hours          = md("**Total Hours**<br><span style='font-weight:normal;font-size:10px;'>Detected / Survey effort</span>"),
    hours_Nocturnal_Low  = "Noct. Low",
    hours_Nocturnal_High = "Noct. High",
    hours_Diurnal_Low    = "Diur. Low",
    hours_Diurnal_High   = "Diur. High",
    pct_Nocturnal_Low    = "Noct. Low",
    pct_Nocturnal_High   = "Noct. High",
    pct_Diurnal_Low      = "Diur. Low",
    pct_Diurnal_High     = "Diur. High"
  ) %>%

  cols_hide(columns = c(pct_Nocturnal_Low_used, pct_Nocturnal_High_used,
                        pct_Diurnal_Low_used,   pct_Diurnal_High_used)) %>%

  cols_align(align = "right",
             columns = c(n_birds_used, total_hours, 
                         hours_Nocturnal_Low, hours_Nocturnal_High,
                         hours_Diurnal_Low, hours_Diurnal_High,
                         pct_Nocturnal_Low, pct_Nocturnal_High,
                         pct_Diurnal_Low, pct_Diurnal_High)) %>%

  # ── TOTAL rows: grey + bold ───────────────────────────────────────────────
  tab_style(
    style     = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold")),
    locations = cells_body(rows = recvDeployName == "Total")
  ) %>%
  tab_style(
    style     = list(cell_fill(color = "#E8E8E8"), cell_text(weight = "bold")),
    locations = cells_stub(rows = recvDeployName == "Total")
  ) %>%

  # ── pct >= 20%: bold blue ─────────────────────────────────────────────────
  tab_style(
    style     = cell_text(weight = "bold", color = "#1A6FA3"),
    locations = cells_body(columns = pct_Nocturnal_Low,  rows = pct_Nocturnal_Low_used  >= 20)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold", color = "#1A6FA3"),
    locations = cells_body(columns = pct_Nocturnal_High, rows = pct_Nocturnal_High_used >= 20)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold", color = "#1A6FA3"),
    locations = cells_body(columns = pct_Diurnal_Low,    rows = pct_Diurnal_Low_used    >= 20)
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold", color = "#1A6FA3"),
    locations = cells_body(columns = pct_Diurnal_High,   rows = pct_Diurnal_High_used   >= 20)
  ) %>%

  # ── N Birds / Total Hours / Rate Use: fond distingué ─────────────────────
  tab_style(
    style     = list(cell_fill(color = "#DDE3EA"),
                     cell_text(weight = "bold", size = px(11), color = "#2C3E50")),
    locations = cells_column_labels(columns = c(n_birds_used, total_hours))
  ) %>%

  # ── Group headers ─────────────────────────────────────────────────────────
  tab_style(
    style     = list(cell_fill(color = "#2C3E50"),
                     cell_text(color = "white", weight = "bold", size = px(13))),
    locations = cells_row_groups()
  ) %>%

  # ── Spanners ──────────────────────────────────────────────────────────────
  tab_style(
    style     = cell_text(weight = "bold", color = "#2C3E50"),
    locations = cells_column_spanners()
  ) %>%

  # ── Column labels ─────────────────────────────────────────────────────────
  tab_style(
    style     = list(cell_fill(color = "#F0F4F8"),
                     cell_text(weight = "bold", size = px(11))),
    locations = cells_column_labels()
  ) %>%

  # ── Stub ──────────────────────────────────────────────────────────────────
  tab_style(
    style     = cell_text(color = "#555555", size = px(11)),
    locations = cells_stub()
  ) %>%

  tab_options(
    table.font.names                  = "Source Sans Pro, Arial, sans-serif",
    table.font.size                   = px(12),
    table.border.top.color            = "#2C3E50",
    table.border.top.width            = px(2),
    table.border.bottom.color         = "#2C3E50",
    table.border.bottom.width         = px(2),
    heading.background.color          = "#FAFAFA",
    heading.border.bottom.color       = "#2C3E50",
    column_labels.border.top.color    = "transparent",
    column_labels.border.bottom.color = "#2C3E50",
    column_labels.border.bottom.width = px(2),
    row_group.border.top.color        = "#2C3E50",
    row_group.border.bottom.color     = "#CCCCCC",
    stub.border.color                 = "#DDDDDD",
    row.striping.include_table_body   = TRUE,
    row.striping.background_color     = "#FAFAFA",
    data_row.padding                  = px(5),
    table.width                       = pct(100)
  ) %>%

  opt_row_striping() %>%
  opt_table_font(font = google_font("Source Sans Pro"))


