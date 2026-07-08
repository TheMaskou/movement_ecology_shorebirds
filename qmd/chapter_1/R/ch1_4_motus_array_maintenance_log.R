library(dplyr)
library(here)
library(openxlsx2)
library(lubridate)

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
# Survey123's "visit_date" question is typed as `datetime` with a default of
# now(), so every Survey123-sourced value carries a real time-of-day, not just
# a calendar date (confirmed 2026-07-08 by inspecting the raw serials - e.g.
# Davies Point 2026-04-29 is stored as 46141.1, not 46141). Historic log
# entries are plain whole-day serials with no time component. Parsing via
# as.Date(as.numeric(x), ...) alone does NOT drop that fractional part - it
# only gets floored for display - so it silently breaks any exact `==`
# comparison against a clean Date downstream. Parse the full datetime first,
# then derive visit_date from it in the Sydney timezone so the calendar date
# reflects the local day of the visit rather than the UTC day.
log_complete <- log_complete |>
  mutate(visit_date_str = visit_date) |>
  mutate(visit_datetime = as.POSIXct(as.numeric(visit_date) * 86400,
                                      origin = "1899-12-30", tz = "UTC")) |>
  mutate(visit_date = as.Date(visit_datetime, tz = "Australia/Sydney"))

# Check for NA dates
log_complete |> 
  filter(is.na(visit_date)) |> 
  select(station_id, visit_date_str, visit_date)

# ==== Exclude Test Entries ====
log_complete <- log_complete |>
  filter(station_id != "TEST")

## ---- Remove Test Entries without Test Label ----

# The 2026-04-29 Davies Point entry was submitted as a test, before "TEST" was
# a station_id option. Confirmed with Callum 2026-07-08 - drop it.
n_before <- nrow(log_complete)
log_complete <- log_complete |>
  filter(!((station_id == "Davies Point") & (visit_date == ymd("2026-04-29"))))
message(n_before - nrow(log_complete), " Davies Point test entry/entries removed")

# ==== Exclude TBC Entries ====
log_tbc <- log_complete |>
  filter(station_id == "TBC")

log_complete <- log_complete |>
  filter(station_id != "TBC")

message(nrow(log_tbc), " entries with station_id = 'TBC' excluded from log_complete (retained in log_tbc)")

# ==== Fix Entries With Deprecated Field Names ====
# The issue is that some of the Survey123 entries were completed before
# finalising the survey structure.
# Specifically, "station_status_left" / "station_status_arrival" were used for
# whether the station was functioning on departure/arrival - these have now
# been replaced by wifi_dep/station_power_dep and wifi_arrival/
# station_power_arrival, meaning that for those entries, the new fields had an
# NA value. Fixed below by deriving the new fields from the deprecated status
# values (confirmed with Callum 2026-07-08). This is a one-off cleanup - the
# Survey123 form has been corrected, so no future entries will need it.
#
# Field vocabulary: wifi_* in {yes, no, unknown};
#                    station_power_* in {on, off, removed, unknown}.
#
# Departure (feeds the map): station_status_left = "online" -> power on/wifi
# yes; "removed" -> power removed/wifi no. No rows have "unknown" or "offline"
# for status_left, so those are not handled here.
#
# Arrival: station_status_arrival = "unknown" -> power/wifi unknown; "online"
# -> power on/wifi yes, EXCEPT Hexham, which was online but had wifi off.
# "offline" is ambiguous (doesn't say whether power or wifi was the problem),
# so there is no generic rule for it - Swan Pond is the sole offline row and is
# hardcoded instead (power on, wifi no, per Callum's notes).
#
# Matching is value-based (not station-name-based, except the two named
# exceptions above) so it is robust to station_id spelling variants - note
# station_rename (globals.R) is not applied in this script.

log_complete <- log_complete |>
  mutate(
    .ssl = tolower(trimws(as.character(station_status_left))),
    .ssa = tolower(trimws(as.character(station_status_arrival))),
    .sid = tolower(trimws(as.character(station_id))),

    ## Departure (feeds the map)
    station_power_dep = case_when(
      !is.na(station_power_dep) ~ station_power_dep,
      .ssl == "online"          ~ "on",
      .ssl == "removed"         ~ "removed",
      TRUE                      ~ station_power_dep
    ),
    wifi_dep = case_when(
      !is.na(wifi_dep) ~ wifi_dep,
      .ssl == "online"  ~ "yes",
      .ssl == "removed" ~ "no",
      TRUE              ~ wifi_dep
    ),

    ## Arrival
    station_power_arrival = case_when(
      !is.na(station_power_arrival) ~ station_power_arrival,
      .ssa == "unknown" ~ "unknown",
      .ssa == "offline" ~ "on",    # Swan Pond (sole offline row) - hardcoded
      .ssa == "online"  ~ "on",
      TRUE              ~ station_power_arrival
    ),
    wifi_arrival = case_when(
      !is.na(wifi_arrival) ~ wifi_arrival,
      is.na(.ssa)          ~ wifi_arrival,   # not a deprecated row -> leave as-is
      .ssa == "unknown"    ~ "unknown",
      .ssa == "offline"    ~ "no",           # Swan Pond (sole offline row) - hardcoded
      .sid == "hexham swamp" ~ "no",         # online, but wifi off per comments
      .ssa == "online"     ~ "yes",
      TRUE                 ~ wifi_arrival
    )
  ) |>
  select(-.ssl, -.ssa, -.sid)

# Inspect the (now-fixed) entries
log_deprecated_fields <- log_complete |>
  filter(is.na(station_status_left)==F | is.na(station_status_arrival) == F)

nrow(log_deprecated_fields)

log_deprecated_fields |> select(
  station_id,
  visit_date,
  technician,
  data_downloaded,
  data_notes,
  issue_presence,
  issue_found,
  issue_category,
  issue_description,
  repair_done,
  repair_description,
  comments,
  station_status_arrival,
  station_status_left,
  wifi_arrival,
  wifi_dep,
  station_power_arrival,
  station_power_dep
) |> datatable()

# Verify that all entries using the deprecated station status fields, now have
# a value for wifi_dep and station_power_dep (feeds the map, so most important).
n_dep_unfilled <- log_deprecated_fields |>
  filter(!is.na(station_status_left) &
         (is.na(wifi_dep) | is.na(station_power_dep))) |>
  nrow()

if (n_dep_unfilled > 0) {
  warning(n_dep_unfilled,
          " deprecated-field rows still have NA wifi_dep/station_power_dep after fix")
} else {
  message("All deprecated-field departure statuses filled, yippee")
}

# ==== Export ====
openxlsx2::write_xlsx(
  log_complete,
  file = path_maintenance_log_excel
)

saveRDS(log_complete, file = path_maintenance_log)
