library(dplyr)
library(here)
library(openxlsx2)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

message("Reminder to run qmd/chapter_1/R/_sync_sharepoint_receiver_log.R if there have been any changes to either the historic or survey123 log on SharePoint!")

# ==== Import Survey123 Log ====

# Specify Survey123 column types, for currently empty columns.
# Types need to be the same between both logs to join them together.
# The issue is that there are some columns (like technician_other) that do not
# currently have any values, and they default to type = numeric. Shouldn't
# need this in future.

# NOTE: The reason I did not simply go into the Survey123 log and manually
# update the column types in Excel, is because this would then need to be done
# every time the Survey123 log is re-downloaded from Survey123.

type_cols_survey123 <- c(
  technician_other = "character",
  repair_description = "character",
  receiver_replacement = "character",
  receiver_replacement_id = "character",
  tag_test_perf_success_dep = "character",
  tag_test_notes_dep = "character",
  visit_date = "character"
)

# NOTE: At the time of writing, there was only a single worksheet in the 
# spreadsheet. If more are added, should specify the sheet = "" argument.
# But it defaults to using the first sheet anyway, so should be fine.

log_survey123 <- wb_to_df(
  file = path_motus_receiver_log_survey123,
  types = type_cols_survey123
)

# ==== Import Historic Log ====
type_cols_historic <- c(
  deploy_mast = "character",
  deploy_solar = "character",
  deploy_omni = "character",
  deploy_dir = "character",
  deploy_receiver = "character",
  deploy_shelf = "character",
  deploy_battery = "character",
  sg_lon = "character",
  sg_lat = "character",
  sg_alt = "character"
)


log_historic <- wb_to_df(
  file = path_motus_receiver_log_historic,
  sheet = "log",
  types = type_cols_historic
)

# ==== Column Comparison ====
cols_only_in_historic <- setdiff(names(log_historic), names(log_survey123))

# Define which columns we expect to only be in the historic log (columns used
# for manual verification / record-keeping etc.)
valid_historic_only <- c("Row Status", "Row Note", "Historic Row Source")

cols_invalid_historic <- setdiff(cols_only_in_historic, valid_historic_only)

if (length(cols_invalid_historic > 0)) {
  warning("There are columns in the historic log that do not map to the Survey123 log: ",
          toString(cols_invalid_historic))
} else{
  message("All columns in historic log are valid, yippee")
}

cols_only_in_survey123 <- setdiff(names(log_survey123), names(log_historic))

# Define which columns we expect to only be in the survey123 log. This is mainly
# the Survey123 generated things like survey timestamp, objectid, etc. Also 
# includes some deprecated columns (which unfortunately seem to remain in the 
# Survey123 output even after the question is removed)

valid_survey123_only <- c("objectid", "globalid", "start", "end", "today",
                          "sg_id_calc", "sg_version_calc", "action", 
                          "sg_id_other", "station_status", "visit_category",
                          "CreationDate", "Creator", "EditDate", "Editor",
                          "sg_lon_calc", "sg_lat_calc", "sg_alt_calc", 
                          "station_status_arrival", "station_status_left")

cols_invalid_survey123 <- setdiff(cols_only_in_survey123, valid_survey123_only)

if (length(cols_invalid_survey123 > 0)) {
  warning("There are columns in the Survey123 log that do not map to the historic log: ",
          toString(cols_invalid_survey123))
} else{
  message("All columns in Survey123 are valid, yippee")
}

# ==== Type Mismatch Check ====
common_cols <- intersect(names(log_historic), names(log_survey123))

type_comparison <- tibble::tibble(
  column      = common_cols,
  type_historic  = sapply(common_cols, \(col) class(log_historic[[col]])[1]),
  type_survey123 = sapply(common_cols, \(col) class(log_survey123[[col]])[1])
) |>
  dplyr::filter(type_historic != type_survey123)

if (nrow(type_comparison) > 0) {
  warning("Type mismatches found between historic and Survey123 logs:", immediate. = TRUE)
  print(type_comparison)
} else {
  message("No type mismatches found, yippee")
}

# ==== Combine Les Tableaux ====
log_complete <- bind_rows(log_historic, log_survey123)

# ==== Fix Corries Island Naming Error ====
# Survey123 entries submitted before the form was corrected used "Corries Island"
# instead of "Corrie Island". Historic log already has the correct name.
log_complete <- log_complete |>
  mutate(station_id = if_else(station_id == "Corries Island", "Corrie Island", station_id))

# ==== Parse Dates ====
# Retain raw character values for debugging (as visit_date_str)
log_complete <- log_complete |>
  mutate(visit_date_str = visit_date) |> 
  mutate(visit_date = as.Date(as.numeric(visit_date), origin = "1899-12-30"))

# Check for NA dates
log_complete |> 
  filter(is.na(visit_date)) |> 
  select(station_id, visit_date_str, visit_date)

# ==== Exclude Test Entries ====
log_complete <- log_complete |>
  filter(station_id != "TEST")

# ==== Exclude TBC Entries ====
log_tbc <- log_complete |>
  filter(station_id == "TBC")

log_complete <- log_complete |>
  filter(station_id != "TBC")

message(nrow(log_tbc), " entries with station_id = 'TBC' excluded from log_complete (retained in log_tbc)")

# ==== Export ====
openxlsx2::write_xlsx(
  log_complete,
  file = path_maintenance_log_excel
)

saveRDS(log_complete, file = path_maintenance_log)
