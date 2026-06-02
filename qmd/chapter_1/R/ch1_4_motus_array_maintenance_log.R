library(dplyr)
library(here)
library(openxlsx2)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

log_historic <- wb_to_df(
  file = path_motus_receiver_log_historic,
  sheet = "log"
)

# NOTE: At the time of writing, there was only a single worksheet in the 
# spreadsheet. If more are added, should specify the sheet = "" argument.
# But it defaults to using the first sheet anyway, so should be fine.
log_survey123 <- wb_to_df(
  file = path_motus_receiver_log_survey123,
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
  warning("There are columns in the Survey123 log that do not map to the historc log: ",
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
