# Load packages
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(readr)
library(here)
library(DT)
library(openxlsx2)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

# ==== Map Display Settings ====
# Edit these variables to change how the map looks. All visual properties of
# the map, markers, and popups are controlled from here — no need to search
# through the rest of the script.

## ---- Map ----
map_height         <- NULL    # NULL = fills & resizes the viewer pane;
                               # set a number (e.g. 520) for a fixed pixel height
map_bounds_padding <- 0.05    # degrees of padding around stations on initial zoom
map_zoom_snap      <- 0.1    # zoom level granularity (1 = integer steps only,
                               # 0.1 = tenth steps; controls where zoom stops)
map_wheel_px_per_zoom <- 120  # scroll pixels required to move one zoom level
                               # (default 60 = ~2 levels per wheel tick, coarse;
                               # 120 = ~1 level per tick; 240 = ~0.5 levels per tick)

## ---- Markers ----
marker_radius        <- 8
marker_fill_opacity  <- 0.9
marker_stroke_weight <- 2

## ---- Marker & Status Colours ----
# Colours used both for popup status text and for circle markers. Edit freely.
status_ok_color  <- "#2ca02c"   # green — used for "on" status text
status_bad_color <- "#d62728"   # red   — used for any other status text

# Circle fill palette
marker_fill_green   <- "#2ca02c"
marker_fill_yellow  <- "#f0c000"
marker_fill_red     <- "#d62728"
marker_fill_default <- "#9e9e9e"   # grey: matches none of the rules below

marker_stroke_darken <- 0.7   # outline = fill colour darkened by this factor (0-1)

# Circle fill rule: first matching rule wins, else marker_fill_default.
# To add or restyle a category, edit this function only — nothing else needs to change.
# Expects wifi already converted to display terms (on/off) via wifi_display().
marker_fill_for <- function(power, wifi) {
  power <- tolower(trimws(as.character(power)))
  wifi  <- tolower(trimws(as.character(wifi)))
  if (isTRUE(power == "on" && wifi == "on"))  return(marker_fill_green)
  if (isTRUE(power == "on" && wifi == "off")) return(marker_fill_yellow)
  if (isTRUE(power %in% c("off", "removed"))) return(marker_fill_red)
  marker_fill_default
}

## ---- WiFi Display Mapping ----
# WiFi is stored as yes/no/unknown but shown on the map as on/off (yes = on,
# no = off). Edit this named vector to change the displayed terms — nothing
# else needs to change.
wifi_display_map <- c(yes = "on", no = "off", unknown = "unknown")

# Map a stored WiFi value to its display term; unmapped/NA values pass through
# unchanged (so na_placeholder / .status_token still handle them normally).
wifi_display <- function(x) {
  out <- unname(wifi_display_map[tolower(trimws(as.character(x)))])
  ifelse(is.na(out), as.character(x), out)
}

# Darken a colour (hex or named) for the marker outline, for contrast with the fill.
darken_color <- function(col, factor = marker_stroke_darken) {
  v <- grDevices::col2rgb(col)
  grDevices::rgb(t(pmax(0, v * factor)), maxColorValue = 255)
}

## ---- Popups ----
popup_max_width  <- 440   # pixels (widened for the 19-column history table)
popup_min_width  <- 320   # pixels
popup_font_family <- "sans-serif"
na_placeholder   <- "—"   # text shown when a value is NA or blank

## ---- Popup Summary Block ----
summary_title_size  <- "15px"
summary_font_size   <- "13px"
summary_label_color <- "#666"

## ---- Popup History Table ----
history_max_height   <- "180px"
history_font_size    <- "12px"
history_header_bg    <- "#f5f5f5"
history_header_color <- "#444"
history_row_border   <- "1px solid #eee"

# Field -> column header for the visit-history table. Edit / reorder / remove
# entries here to change the table — column order follows this vector's order.
history_fields <- c(
  visit_date                = "Date",
  technician                = "Technician",
  data_downloaded           = "Data DL",
  station_power_dep         = "Power (dep)",
  wifi_dep                  = "WiFi (dep)",
  tag_test_perf_dep         = "Tag Tested",
  tag_test_perf_success_dep = "Test Tag Detected",
  sg_id                     = "Receiver",
  sg_version                = "Version",
  issue_presence            = "Issue?",
  issue_category            = "Issue cat.",
  #issue_description         = "Issue desc.",
  repair_done               = "Repair?",
  #repair_description        = "Repair desc.",
  station_power_arrival     = "Power (arr)",
  wifi_arrival              = "WiFi (arr)"
  #comments                  = "Comments",
  #data_notes                = "Data notes",
  #tag_test_notes_dep        = "Tag test notes"
)

# Per-column width bounds (long free-text wraps within these; the table
# scrolls horizontally once total column width exceeds the popup width).
history_cell_min_width <- "80px"
history_cell_max_width <- "220px"

## ---- Maintenance Table & Downloads ----
# The on-page interactive table (dt_maintenance) and the downloadable files are
# built from a cleaned copy of the log. Columns listed here are dropped from the
# TABLE and the .xlsx download (they are Survey123 system/calc/debug fields that
# add noise). The .rds download keeps the FULL log for R users, so nothing is
# lost there. Edit this vector to show/hide columns.
table_drop_cols <- c(
  "globalid", "start", "end", "today",
  "sg_id_calc", "sg_version_calc", "sg_lon_calc", "sg_lat_calc", "sg_alt_calc",
  "action", "sg_id_other", "objectid",
  "CreationDate", "Creator", "EditDate", "Editor",
  "entry_source", "visit_datetime", "visit_date_str",
  "Historic Row Source", "technician_other"
)

table_page_length <- 15   # rows shown per page in the interactive table

# Human-readable column headers. Format: "Label" = "raw_column_name".
# Columns not listed here keep their raw name. Edit / extend freely.
# (To see the full list of visible columns and finish the map, run
#  names(table_data) after the table_data block below has run.)
col_labels <- c(
  "Station"            = "station_id",
  "Visit date"         = "visit_date",
  "Technician"         = "technician",
  "Data downloaded"    = "data_downloaded",
  "Power (dep)"        = "station_power_dep",
  "WiFi (dep)"         = "wifi_dep",
  "Power (arr)"        = "station_power_arrival",
  "WiFi (arr)"         = "wifi_arrival",
  "Tag tested"         = "tag_test_perf_dep",
  "Test tag detected"  = "tag_test_perf_success_dep",
  "Receiver"           = "sg_id",
  "Version"            = "sg_version",
  "Issue?"             = "issue_presence",
  "Issue category"     = "issue_category",
  "Issue description"  = "issue_description",
  "Repair?"            = "repair_done",
  "Repair description" = "repair_description",
  "Comments"           = "comments",
  "Data notes"         = "data_notes"
)

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
  here::here("qmd", "chapter_1", "data", "motus", "receivers_temp.csv"),
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
# Returns na_placeholder for NA or blank values; otherwise HTML-escapes the value.
.fmt <- function(x) {
  ifelse(
    is.na(x) | trimws(as.character(x)) == "",
    na_placeholder,
    htmlEscape(as.character(x))
  )
}

## ---- Helper: Short SensorGnome Version Token ----
# e.g. "SensorGnome v1" -> "v1"; returns the raw (trimmed) value if no "vN"
# pattern is found (e.g. "NONE").
sg_version_short <- function(x) {
  raw <- trimws(as.character(x))
  ifelse(grepl("v[0-9]+", raw, ignore.case = TRUE),
         sub(".*?(v[0-9]+).*", "\\1", raw, ignore.case = TRUE), raw)
}

## ---- Helper: Format a History-table Cell ----
# Applies field-specific display conversions (WiFi yes/no -> on/off, short
# SensorGnome version) on top of the generic .fmt() NA/blank handling.
fmt_cell <- function(field, value) {
  if (field %in% c("wifi_dep", "wifi_arrival")) return(.fmt(wifi_display(value)))
  if (field == "sg_version")                    return(.fmt(sg_version_short(value)))
  .fmt(value)
}

## ---- Helper: Colour-coded Status Token ----
# Green if the value is "on" (any case); red for everything else, including NA/blank.
.status_token <- function(x) {
  raw   <- trimws(as.character(x))
  is_on <- !is.na(x) && tolower(raw) == "on"
  label <- if (is.na(x) || raw == "") "NA" else toupper(raw)
  color <- if (is_on) status_ok_color else status_bad_color
  paste0("<span style='color:", color, ";font-weight:bold'>", htmlEscape(label), "</span>")
}

## ---- Helper: Latest Visit Row ----
# Picks the most recent dated visit; if none are dated, falls back to the last row
# after sorting with NA dates last (matches the ordering used for the history table).
latest_visit <- function(rows) {
  rows_dated <- rows |> filter(!is.na(visit_date))
  if (nrow(rows_dated) > 0) {
    rows_dated |> slice_max(visit_date, n = 1, with_ties = FALSE)
  } else {
    rows |> arrange(desc(visit_date)) |> slice(1)
  }
}

## ---- Build Popup for One Station ----
# Takes all visit rows for a single station and returns a self-contained HTML
# string with a summary header and a scrollable full visit history table.
# Uses the display-settings variables defined at the top of the script.
build_popup <- function(rows) {
  # NA dates sort last, so the history table still shows undated rows.
  rows_all <- rows |> arrange(desc(visit_date))
  latest   <- latest_visit(rows)

  # Last data download (most recent visit where data_downloaded == "yes")
  last_dl_rows <- rows |> filter(!is.na(visit_date)) |> filter(tolower(data_downloaded) == "yes")
  last_dl_str  <- if (nrow(last_dl_rows) > 0) {
    r <- last_dl_rows |> slice_max(visit_date, n = 1, with_ties = FALSE)
    paste0(.fmt(r$visit_date), " (", .fmt(r$technician), ")")
  } else {
    na_placeholder
  }

  # Receiver line: append the short SensorGnome version, e.g. "SG-... (v1)".
  # Omit the "(vN)" suffix when there's no real version (NA / blank / "NONE").
  sg_version_raw <- trimws(as.character(latest$sg_version))
  receiver_str <- if (is.na(latest$sg_version) || sg_version_raw %in% c("", "NONE")) {
    .fmt(latest$sg_id)
  } else {
    paste0(.fmt(latest$sg_id), " (", htmlEscape(sg_version_short(latest$sg_version)), ")")
  }

  # Summary block (latest-status snapshot)
  summary_html <- paste0(
    "<div style='font-family:", popup_font_family, ";font-size:", summary_font_size, ";padding:2px'>",
    "<b style='font-size:", summary_title_size, "'>", .fmt(latest$station_id), "</b><br>",
    "<span style='color:", summary_label_color, ";font-size:12px'>Receiver: ",
    receiver_str, "</span><br><br>",
    "<table style='border-collapse:collapse;width:100%;font-size:", summary_font_size, "'>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last visit</td>",
      "<td>", .fmt(latest$visit_date), " (", .fmt(latest$technician), ")</td></tr>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Status on departure</td>",
      "<td>Power: ", .status_token(latest$station_power_dep),
      "  |  WiFi: ", .status_token(wifi_display(latest$wifi_dep)), "</td></tr>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last data download</td>",
      "<td>", last_dl_str, "</td></tr>",
    "</table>"
  )

  # Scrollable visit history table (all visits, newest first). Columns are
  # driven entirely by history_fields (settings, top of script) — edit that
  # vector to add/remove/reorder columns.
  cell_style <- paste0(
    "padding:3px 6px;vertical-align:top;min-width:", history_cell_min_width,
    ";max-width:", history_cell_max_width, ";white-space:normal;word-wrap:break-word"
  )

  history_rows_html <- paste0(
    vapply(seq_len(nrow(rows_all)), function(i) {
      r <- rows_all[i, ]
      cells <- vapply(names(history_fields), function(field) {
        paste0("<td style='", cell_style, "'>", fmt_cell(field, r[[field]]), "</td>")
      }, character(1))
      paste0("<tr style='border-top:", history_row_border, "'>",
             paste(cells, collapse = ""), "</tr>")
    }, character(1)),
    collapse = ""
  )

  header_cells <- paste0(
    vapply(history_fields, function(label) {
      paste0("<th style='padding:4px 6px;text-align:left'>", htmlEscape(label), "</th>")
    }, character(1)),
    collapse = ""
  )

  history_html <- paste0(
    "<hr style='margin:8px 0;border:none;border-top:1px solid #ddd'>",
    "<span style='font-size:12px;color:", summary_label_color, "'>",
      "Visit history (", nrow(rows_all), " visit",
      ifelse(nrow(rows_all) == 1, "", "s"), ")</span>",
    "<div style='max-height:", history_max_height, ";overflow-y:auto;overflow-x:auto;margin-top:4px'>",
    "<table style='border-collapse:collapse;width:100%;font-size:", history_font_size, "'>",
    "<thead>",
    "<tr style='background:", history_header_bg, ";color:", history_header_color,
    ";font-size:11px;position:sticky;top:0'>",
    header_cells,
    "</tr>",
    "</thead>",
    "<tbody>", history_rows_html, "</tbody>",
    "</table>",
    "</div>",
    "</div>"   # close outer div
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
  popup      = sapply(station_groups, build_popup),
  fill_color = sapply(station_groups, \(grp) {
    lv <- latest_visit(grp)
    marker_fill_for(lv$station_power_dep, wifi_display(lv$wifi_dep))
  })
) |>
  mutate(stroke_color = darken_color(fill_color))

# ==== Map ====
map_maintenance <- leaflet(popup_data, height = map_height,
                          options = leafletOptions(zoomSnap = map_zoom_snap,
                                        wheelPxPerZoomLevel = map_wheel_px_per_zoom)) |>
  addProviderTiles("CartoDB.Positron",  group = "Map") |>
  addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
  addProviderTiles("OpenStreetMap",     group = "Street (OSM)") |>
  addCircleMarkers(
    lng          = ~sg_lon,
    lat          = ~sg_lat,
    radius       = marker_radius,
    color        = ~stroke_color,
    weight       = marker_stroke_weight,
    fillColor    = ~fill_color,
    fillOpacity  = marker_fill_opacity,
    popup        = ~popup,
    popupOptions = popupOptions(maxWidth = popup_max_width, minWidth = popup_min_width)
  ) |>
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Street (OSM)"),
    options    = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(
    position = "bottomright",
    colors   = c(marker_fill_green, marker_fill_yellow, marker_fill_red, marker_fill_default),
    labels   = c("Power on &amp; WiFi on", "Power on, WiFi off",
                  "Power off / removed", "Unknown / no data"),
    title    = "Departure Status",
    opacity  = marker_fill_opacity
  ) |>
  fitBounds(
    lng1 = min(popup_data$sg_lon) - map_bounds_padding,
    lat1 = min(popup_data$sg_lat) - map_bounds_padding,
    lng2 = max(popup_data$sg_lon) + map_bounds_padding,
    lat2 = max(popup_data$sg_lat) + map_bounds_padding
  ) |>
  addScaleBar(position = "bottomleft") |>
  addMiniMap() |>
  addFullscreenControl() |>
  htmlwidgets::onRender("
    function(el, x) {
      var base = el.querySelector('.leaflet-control-layers-base');
      if (base) {
        var h = document.createElement('div');
        h.style.cssText = 'padding:2px 4px 6px 2px;font-weight:bold;font-size:11px;color:#555;text-transform:uppercase;letter-spacing:0.05em';
        h.textContent = 'Basemap';
        base.parentNode.insertBefore(h, base);
      }
    }
  ")

map_maintenance

# ==== Maintenance Table ====
# Cleaned, one-row-per-visit table for on-page browsing. station_id + visit_date
# are moved to the front; the noise columns in table_drop_cols are removed. This
# same cleaned frame feeds the .xlsx download below.
# Sort the data itself so the table AND both download files (.xlsx from
# table_data, .rds from maintenance_log) share the same order. NA dates sort
# last by default. The map above is already built, so it is unaffected.
maintenance_log <- maintenance_log |>
  arrange(desc(visit_date), station_id)

table_data <- maintenance_log |>
  select(-any_of(table_drop_cols)) |>
  relocate(station_id, visit_date, .before = 1)

# Plain interactive table: per-column filters, readable headers, horizontal
# scroll (there are many columns), and the arranged newest-first order.
dt_maintenance <- datatable(
  table_data,
  filter   = "top",
  rownames = FALSE,
  colnames = col_labels[col_labels %in% names(table_data)],
  options  = list(
    pageLength = table_page_length,
    scrollX    = TRUE,     # many columns — scroll rather than overflow the page
    order      = list()    # keep the arranged (newest-first) order on load
  )
)

# ==== Downloadable Receiver Log ====
# Generate the download files into qmd/chapter_1/downloads/ so they land in the
# published site (docs/) and can be linked from the .qmd. Unlike the browser's
# client-side DataTables export, these preserve data types:
#   - .xlsx (openxlsx2): dates -> Excel dates, numbers -> numbers, etc.
#   - .rds : full-fidelity, full log (all columns) for anyone continuing in R.
dir_downloads <- here::here("qmd", "chapter_1", "downloads")
if (!dir.exists(dir_downloads)) dir.create(dir_downloads, recursive = TRUE)

openxlsx2::write_xlsx(
  table_data,
  file = file.path(dir_downloads, "receiver_log_complete.xlsx")
)

saveRDS(
  maintenance_log,
  file = file.path(dir_downloads, "receiver_log_complete.rds")
)
