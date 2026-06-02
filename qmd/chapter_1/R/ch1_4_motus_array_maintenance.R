# Load packages
library(dplyr)
library(leaflet)
library(htmltools)
library(readr)
library(here)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

# ==== Load Data ====
maintenance_log <- readRDS(path_maintenance_log)

# ==== Load and Attach Station Coordinates ====
# Coordinates are sourced from receivers_temp.csv for every row (both the
# historic and Survey123 entries), replacing the per-row sg_lon / sg_lat
# columns that were only populated for Survey123 entries.
#
# The format in the CSV is e.g. "151.681010702398E" / "32.846728923395S".
# Conversion: extract the leading number with sub(), then negate if W or S.

station_coords <- read_csv(
  here::here("data", "motus", "receivers_temp.csv"),
  show_col_types = FALSE
) |>
  filter(station_id != "TEST") |>
  mutate(
    sg_lon = as.numeric(sub("([0-9.]+).*", "\\1", sg_lon)) *
               ifelse(grepl("W", sg_lon), -1, 1),
    sg_lat = as.numeric(sub("([0-9.]+).*", "\\1", sg_lat)) *
               ifelse(grepl("S", sg_lat), -1, 1)
  ) |>
  select(station_id, sg_lon, sg_lat)

maintenance_log <- maintenance_log |>
  select(-sg_lon, -sg_lat) |>
  left_join(station_coords, by = "station_id")

## ---- Coordinate Guard ----
# Warn if any station_id in the log has no match in receivers_temp.csv, as
# those stations will be silently excluded from the map.
missing_coords <- maintenance_log |>
  filter(is.na(sg_lon) | is.na(sg_lat)) |>
  distinct(station_id) |>
  pull(station_id)

if (length(missing_coords) > 0) {
  warning(
    "These station_id values have no match in receivers_temp.csv and will be ",
    "excluded from the map: ", toString(missing_coords)
  )
}

# ==== Build Popup HTML ====

## ---- Helper: Format a Single Value for HTML ----
# Returns an em dash for NA or blank values; otherwise HTML-escapes the value.
.fmt <- function(x) {
  ifelse(
    is.na(x) | trimws(as.character(x)) == "",
    "—",
    htmlEscape(as.character(x))
  )
}

## ---- Build Popup for One Station ----
# Takes all visit rows for a single station and returns a self-contained HTML
# string with a summary header and a scrollable full visit history table.
build_popup <- function(rows) {
  # Use only dated rows to determine the "latest" visit; undated historic rows
  # sort last with arrange(desc(visit_date)) which is why we exclude them here.
  rows_dated <- rows |> filter(!is.na(visit_date))
  rows_all   <- rows |> arrange(desc(visit_date))  # NA dates sort last

  latest <- if (nrow(rows_dated) > 0) {
    rows_dated |> slice_max(visit_date, n = 1, with_ties = FALSE)
  } else {
    rows_all[1, ]
  }

  # Last data download (most recent visit where data_downloaded == "yes")
  last_dl_rows <- rows_dated |> filter(tolower(data_downloaded) == "yes")
  last_dl_str  <- if (nrow(last_dl_rows) > 0) {
    r <- last_dl_rows |> slice_max(visit_date, n = 1, with_ties = FALSE)
    paste0(.fmt(r$visit_date), " (", .fmt(r$technician), ")")
  } else {
    "—"
  }

  # Summary block (latest-status snapshot)
  summary_html <- paste0(
    "<div style='font-family:sans-serif;font-size:13px;",
                  "min-width:300px;max-width:400px;padding:2px'>",
    "<b style='font-size:15px'>", .fmt(latest$station_id), "</b><br>",
    "<span style='color:#666;font-size:12px'>Receiver: ",
    .fmt(latest$sg_id), "</span><br><br>",
    "<table style='border-collapse:collapse;width:100%;font-size:13px'>",
    "<tr><td style='color:#666;padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last visit</td>",
      "<td>", .fmt(latest$visit_date), " (", .fmt(latest$technician), ")</td></tr>",
    "<tr><td style='color:#666;padding:2px 10px 2px 0;white-space:nowrap'>",
      "Status on departure</td>",
      "<td>", .fmt(latest$station_status_left), "</td></tr>",
    "<tr><td style='color:#666;padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last data download</td>",
      "<td>", last_dl_str, "</td></tr>",
    "<tr><td style='color:#666;padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last issue</td>",
      "<td>", .fmt(latest$issue_category), "</td></tr>",
    "</table>"
  )

  # Scrollable visit history table (all visits, newest first)
  history_rows_html <- paste0(
    vapply(seq_len(nrow(rows_all)), function(i) {
      r <- rows_all[i, ]
      paste0(
        "<tr style='border-top:1px solid #eee'>",
        "<td style='padding:3px 6px;white-space:nowrap'>", .fmt(r$visit_date), "</td>",
        "<td style='padding:3px 6px'>",                    .fmt(r$technician), "</td>",
        "<td style='padding:3px 6px;text-align:center'>",  .fmt(r$data_downloaded), "</td>",
        "<td style='padding:3px 6px'>",                    .fmt(r$issue_category), "</td>",
        "<td style='padding:3px 6px'>",                    .fmt(r$repair_description), "</td>",
        "</tr>"
      )
    }, character(1)),
    collapse = ""
  )

  history_html <- paste0(
    "<hr style='margin:8px 0;border:none;border-top:1px solid #ddd'>",
    "<span style='font-size:12px;color:#666'>",
      "Visit history (", nrow(rows_all), " visit",
      ifelse(nrow(rows_all) == 1, "", "s"), ")</span>",
    "<div style='max-height:180px;overflow-y:auto;margin-top:4px'>",
    "<table style='border-collapse:collapse;width:100%;font-size:12px'>",
    "<thead>",
    "<tr style='background:#f5f5f5;color:#444;font-size:11px;position:sticky;top:0'>",
    "<th style='padding:4px 6px;text-align:left'>Date</th>",
    "<th style='padding:4px 6px;text-align:left'>Technician</th>",
    "<th style='padding:4px 6px;text-align:center'>Data DL</th>",
    "<th style='padding:4px 6px;text-align:left'>Issue</th>",
    "<th style='padding:4px 6px;text-align:left'>Repair</th>",
    "</tr>",
    "</thead>",
    "<tbody>", history_rows_html, "</tbody>",
    "</table>",
    "</div>",
    "</div>"  # close outer div
  )

  paste0(summary_html, history_html)
}

# ==== Build Per-station Data for Mapping ====
# One row per station: coordinates + popup HTML built from all visits for that
# station. Uses group_split() so each group's full data frame is passed to
# build_popup(), which derives the latest visit and history table internally.
station_groups <- maintenance_log |>
  filter(!is.na(sg_lon), !is.na(sg_lat)) |>
  group_by(station_id) |>
  group_split()

popup_data <- tibble::tibble(
  station_id = sapply(station_groups, \(grp) grp$station_id[1]),
  sg_lon     = sapply(station_groups, \(grp) grp$sg_lon[1]),
  sg_lat     = sapply(station_groups, \(grp) grp$sg_lat[1]),
  popup      = sapply(station_groups, build_popup)
)

# ==== Map ====
map_maintenance <- leaflet(popup_data, height = 520) |>
  addProviderTiles("CartoDB.Positron",  group = "Map") |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
  addCircleMarkers(
    lng          = ~sg_lon,
    lat          = ~sg_lat,
    radius       = 7,
    color        = "white",
    weight       = 2,
    fillColor    = "#1b6ca8",
    fillOpacity  = 0.9,
    popup        = ~popup,
    popupOptions = popupOptions(maxWidth = 420, minWidth = 320)
  ) |>
  addLayersControl(
    baseGroups = c("Map", "Satellite"),
    options    = layersControlOptions(collapsed = FALSE)
  ) |>
  fitBounds(
    lng1 = min(popup_data$sg_lon) - 0.05,
    lat1 = min(popup_data$sg_lat) - 0.05,
    lng2 = max(popup_data$sg_lon) + 0.05,
    lat2 = max(popup_data$sg_lat) + 0.05
  ) |>
  addScaleBar(position = "bottomleft") |>
  addMiniMap()

map_maintenance
