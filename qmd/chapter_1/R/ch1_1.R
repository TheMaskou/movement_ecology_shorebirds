# ch1_1.R — Load, clean, and enrich Motus detection data
#
# DEPENDENCIES:
#   - qmd/chapter_1/R/globals.R (constants: station_rename, tag lists, project number,
#     species colours, names, classifications)
#   - data/motus/project-294.motus (SQLite database)
#   - data/motus/detection_data.rds (previous output from previous run, if exists)
#   - data/tides/TideDataNewcastle.csv
#   - data/spreadsheet/SHOREBIRD NUMBER TRACKING(Birds caught).csv
# The above is stored in the shorebird group SharePoint, to be downloaded manually
# as a .csv whenever new birds are caught / tagged.
# PRODUCES:
#   - data/motus/detection_data.rds (df.alltags)
#   - data/motus/recv-info.rds (df.recvDeps)
#   - data/tides/tideData.rds
#   - data/motus/backups/{date}-detection_data.rds (dated backup)
#   - data/spreadsheet/spreadsheet_data.rds

# ==== Setup ====

library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats)
library(lubridate)
library(bioRad)
library(purrr)
library(ggplot2)
library(tictoc)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

# IMPORTANT NOTE:
# Output file paths are defined in globals.R; this way, they can easily be
# imported into other files without remembering the path.

# ==== SQLite Connection ====

sql.motus <- dbConnect(SQLite(), path_motus_database)

# Test connection by listing tables
dbListTables(sql.motus)

# ==== Load Previous Output ====
# If there is no previous output (i.e., this is the first time the script has been run on a machine or the data file has been deleted), will return NULL
df.alltags.past <- if (file.exists(path_detection_data)) readRDS(path_detection_data) else NULL

# ==== Get New Detections from SQLite ====

# Resolve ambiguous detections before querying
clarify(sql.motus)

tic("Flattening df.new using collect() and as.data.frame(): ")
if (!is.null(df.alltags.past)) {
  # Only pull rows not already processed (hitID is unique per detection)
  past_ids <- df.alltags.past$hitID
  df.new <- tbl(sql.motus, "alltags") %>%
    #head(1000) |> # Uncomment this line to test with a small number of data points
    filter(!hitID %in% past_ids) %>%
    collect() %>%
    as.data.frame()
} else {
  df.new <- tbl(sql.motus, "alltags") %>%
    collect() %>%
    as.data.frame()
}
toc(log = T)
# 2026-05-29: Entire (fresh) dataset only took 33 seconds to collect. Epic.

# ==== Process New Rows ====
if (nrow(df.new) == 0) {
  message("No new detections found. Using existing detection_data.rds.")
  df.alltags <- df.alltags.past

} else {
  message(nrow(df.new), " new detections to process.")

  ## 1. Datetime columns ----
  tic("1. Datetime columns")
  df.new <- df.new %>%
    mutate(time = as_datetime(ts),
           timeAus = as_datetime(ts, tz = "Australia/Sydney"),
           dateAus = as_date(timeAus),
           year = year(time),
           day = yday(time))
  toc(log = TRUE)

  ## 2. Filter tags (test, undeployed, pre-tagging detections) ----
  tic("2. Filter tags")
  df.new <- df.new %>%
    filter(
      !(motusTagID %in% motus_tags_test),
      !(motusTagID %in% motus_tags_undeployed),
      !(motusTagID == "81134" & time < dmy("23-11-2024")),
      !(motusTagID == "60575" & time < dmy("25-10-2023"))
    )
  toc(log = TRUE)

  ## 3. Fix NA species names for known tags ----
  tic("3. Fix NA species names")
  df.new <- df.new %>%
    mutate(speciesEN = case_when(
      is.na(speciesEN) & motusTagID %in% c("60470", "81121") ~ "Red-necked Avocet",
      is.na(speciesEN) & motusTagID %in% c("81118") ~ "Red-necked Avocet",
      TRUE ~ speciesEN
    )) %>%
    mutate(motusTagID = as.character(motusTagID))
  toc(log = TRUE)

  ## 4. Filter receivers (bug fix: use %in% instead of != c(...)) ----
  tic("4. Filter receivers")
  df.new <- df.new %>%
    filter(
      !is.na(recvDeployLat),
      recvDeployName != "Throsby Creek Test Site",
      !(recv %in% c("SG-C621RPI3E17F", "SG-62A5RPI36710"))
    ) %>%
    mutate(recvDeployName = ifelse(
      is.na(recvDeployName) & recv == "SG-D5BBRPI3E2F7",
      "Windeyers",
      recvDeployName
    ))
  toc(log = TRUE)

  ## 5. Snapshot before motusFilter removal (for diagnostic plots) ----
  tic("5. Snapshot before motusFilter removal")
  df.new.prefilter <- df.new
  toc(log = TRUE)

  ## 6. MotusFilter: keep only valid detections (build diagnostic plots first) ----
  tic("6. MotusFilter")
  
  # FIXME: These plots are massive and use too much memory (plot_filter_in and 
  # plot_filter_perc 1.5GB each). Would be simpler to just save the current 
  # state of the dataset (e.g. df.new.prefilter), and generate the diagnostic 
  # plots in a separate script.
  
  # plot_filter_in <- ggplot(df.new.prefilter %>% filter(motusFilter == 1),
  #        aes(x = recvDeployName)) +
  #   geom_bar(fill = "steelblue") + theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   labs(x = "Motus Station", y = "Nb of motusFilter = 1 (good)")
  # 
  # plot_filter_out <- ggplot(df.new.prefilter %>% filter(motusFilter == 0),
  #        aes(x = recvDeployName)) +
  #   geom_bar(fill = "orange") + theme_minimal() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #   labs(x = "Motus Station", y = "Nb of motusFilter = 0 (filtered out)")
  # 
  # plot_filter_perc <- ggplot(
  #        df.new.prefilter %>% filter(motusFilter %in% c(0, 1)),
  #        aes(x = recvDeployName, fill = factor(motusFilter))) +
  #   geom_bar(position = "fill") +
  #   scale_fill_manual(values = c("0" = "orange", "1" = "steelblue"),
  #                     labels = c("0 (filtered out)", "1 (good)"),
  #                     name = "motusFilter") +
  #   theme_minimal() +
  #   labs(x = "Motus Station", y = "Proportion") +
  #   scale_y_continuous(labels = scales::percent) +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

  df.new <- df.new %>%
    filter(motusFilter == 1,
           runLen >= 3)
  toc(log = TRUE)

  ## 7. Station rename ----
  tic("7. Station rename")
  df.new <- df.new %>%
    mutate(recvDeployName = recode(recvDeployName, !!!station_rename))
  toc(log = TRUE)

  ## 8. Spreadsheet join (Band.ID) ----
  tic("8. Spreadsheet join")
  # Import spreadsheet from file (ensure is current)
  spreadsheet <- read.csv(path_shorebird_number_spreadsheet) |> 
    # Keep only the tagged ones 
    filter(Radio.tag. == "Y") %>%      
    
    # Variable names
    rename(DateAUS.Trap = "Date", 
           motusTagID = "Motus.tag.ID", 
           speciesEN = "Species") %>%
    
    # Value names
    mutate(speciesEN = case_when(
      speciesEN == "Eastern Curlew" ~ "Far Eastern Curlew",
      speciesEN == "Black-winged Stilt" ~ "Pied Stilt",
      speciesEN == "Pacific Golden Plover" ~ "Pacific Golden-Plover",
      speciesEN == "Whimbrel" ~ "Eurasian Whimbrel",
      TRUE ~ speciesEN )) %>% 
    
    # Format
    mutate(motusTagID = as.factor(motusTagID),
           DateAUS.Trap = as.Date(DateAUS.Trap),
           Band.ID = as.factor(Band.ID)) %>%
    select(Band.ID, motusTagID, speciesEN, DateAUS.Trap, everything())  

  # Ensure motusTagID is also a factor in df.new (otherwise merge won't work)
  df.new <- df.new %>%
    mutate(motusTagID = as.factor(motusTagID))

  # Join Band ID to new detections, using motusTagID as the join key
  df.new <- left_join(df.new,
                      spreadsheet %>%
                        filter(is.na(Euthanised.)) %>%
                        select(motusTagID, DateAUS.Trap, Band.ID, Bander),
                      by = "motusTagID")
  toc(log = TRUE)

  ## 9. Tide data ----
  tic("9. Tide data")
  tideData <- read.csv(here("data", "tides", "TideDataNewcastle.csv"))

  tideData <- tideData %>% mutate(
    date = dmy(date, tz = "Australia/Sydney"),
    tideDateTimeAus = dmy_hm(tideDateTimeAus, tz = "Australia/Sydney")
  )

  tideData <- tideData %>% mutate(
    sunriseNewc = sunrise(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
    sunsetNewc = sunset(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
    sunriseNewcTime = strftime(sunriseNewc, format = "%H:%M:%S", tz = "Australia/Sydney"),
    sunsetNewcTime = strftime(sunsetNewc, format = "%H:%M:%S", tz = "Australia/Sydney")
  )

  tideData <- tideData %>% mutate(
    day_night = case_when(
      tideDateTimeAus >= sunriseNewc & tideDateTimeAus <= sunsetNewc ~ "Diurnal",
      TRUE ~ "Nocturnal"
    )
  )

  tideData <- tideData %>% mutate(
    tideCategory = case_when(
      high_low == "Low" & day_night == "Diurnal" ~ "Diurnal_Low",
      high_low == "Low" & day_night == "Nocturnal" ~ "Nocturnal_Low",
      high_low == "High" & day_night == "Diurnal" ~ "Diurnal_High",
      high_low == "High" & day_night == "Nocturnal" ~ "Nocturnal_High") %>%
      as_factor())

  tideData <- tideData %>%
    group_by(tideCategory) %>%
    mutate(tideID = paste0(tideCategory, "_", row_number())) %>%
    ungroup()

  tidalCurveFunc <- splinefun(tideData$tideDateTimeAus, tideData$tideHeight, method = "natural")
  get.tideIndex <- function(time) { return(which.min(abs(tideData$tideDateTimeAus - time))) }

  # Add tide + sunrise/sunset variables to new rows
  df.new <- df.new %>%
    sunRiseSet(lat = "recvDeployLat",
               lon = "recvDeployLon",
               ts = "ts") %>%
    mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
           sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
    mutate(tideHeight = tidalCurveFunc(timeAus),
           tideIndex = map_dbl(timeAus, get.tideIndex))

  tide_values <- tideData[df.new$tideIndex,
                          c("tideDateTimeAus", "high_low", "day_night",
                            "tideCategory", "tideID", "tideHeight")]

  df.new <- df.new %>%
    mutate(tideDateTimeAus = tide_values$tideDateTimeAus,
           tideHighLow = as_factor(tide_values$high_low),
           tideDiel = as_factor(tide_values$day_night),
           tideCategory = as_factor(tide_values$tideCategory),
           tideCategoryHeight = tide_values$tideHeight,
           tideID = as_factor(tide_values$tideID),
           tideTimeDiff = abs(difftime(timeAus, tideDateTimeAus, units = "hours")))

  df.new <- df.new %>%
    mutate(Band.ID = as.factor(Band.ID))
  toc(log = TRUE)

  # ---- Combine with Past ----

  if (!is.null(df.alltags.past)) {
    df.alltags <- bind_rows(df.alltags.past, df.new)
  } else {
    df.alltags <- df.new
  }

  # ---- Full-dataset Recalculations ----

  df.alltags <- df.alltags %>%
    mutate(sigPositive = sig + abs(min(sig)))

  # ---- Final Cleaning ----

  df.alltags <- df.alltags %>%
    filter(!is.na(speciesEN))
}

# ==== Receiver Deployments ====
# TODO: would it be simpler to specify the stations we ARE interested in, rather
# than filtering out specific stations?
df.recvDeps <- tbl(sql.motus, "recvDeps") %>%
  collect() %>%
  as.data.frame() %>%
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"),
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney"))

# Filter to our project (otherwise includes ALL motus stations)
df.recvDeps <- df.recvDeps |> 
  filter(projectID == motus_proj_num)

# Apply station name corrections
df.recvDeps <- df.recvDeps %>%
  mutate(name = recode(name, !!!station_rename)) %>%
  rename(recvDeployName = "name")

# Filter test stations (bug fix: use %in% instead of != c(...))
df.recvDeps <- df.recvDeps %>%
  filter(!is.na(latitude),
         recvDeployName != "Throsby Creek Test Site",
         !(serno %in% c("SG-C621RPI3E17F", "SG-62A5RPI36710")))

# Filter to study period
df.recvDeps <- df.recvDeps %>%
  filter(timeStartAus > "2023-01-31 00:00:00 AEDT")

df.recvDeps |> glimpse()

# ==== Save ====

# Overwrite output files
saveRDS(df.alltags,  path_detection_data)
saveRDS(df.recvDeps, path_recv_info)
saveRDS(tideData,    path_tideData)
saveRDS(spreadsheet, path_spreadsheet_data)

# Dated backups
backup_dir <- file.path(dir_motus, "backups")
dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
file.copy(path_detection_data, file.path(backup_dir, paste0("data-", Sys.Date(), ".rds")))
file.copy(path_recv_info, file.path(backup_dir, paste0("recv-info-", Sys.Date(), ".rds")))

message("Done. Saved to ", path_detection_data)
