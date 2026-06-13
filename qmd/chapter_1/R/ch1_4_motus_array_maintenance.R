# Load packages
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(readr)
library(here)

source(here::here("qmd", "chapter_1", "R", "globals.R"))

# TODO: Add more columns to popup (probably issue notes, comments, power status)

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
marker_fill_for <- function(power, wifi) {
  power <- tolower(trimws(as.character(power)))
  wifi  <- tolower(trimws(as.character(wifi)))
  if (isTRUE(power == "on" && wifi == "on"))  return(marker_fill_green)
  if (isTRUE(power == "on" && wifi == "off")) return(marker_fill_yellow)
  if (isTRUE(power %in% c("off", "removed"))) return(marker_fill_red)
  marker_fill_default
}

# Darken a colour (hex or named) for the marker outline, for contrast with the fill.
darken_color <- function(col, factor = marker_stroke_darken) {
  v <- grDevices::col2rgb(col)
  grDevices::rgb(t(pmax(0, v * factor)), maxColorValue = 255)
}

## ---- Popups ----
popup_max_width  <- 420   # pixels
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
# Returns na_placeholder for NA or blank values; otherwise HTML-escapes the value.
.fmt <- function(x) {
  ifelse(
    is.na(x) | trimws(as.character(x)) == "",
    na_placeholder,
    htmlEscape(as.character(x))
  )
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

  # Summary block (latest-status snapshot)
  summary_html <- paste0(
    "<div style='font-family:", popup_font_family, ";font-size:", summary_font_size, ";padding:2px'>",
    "<b style='font-size:", summary_title_size, "'>", .fmt(latest$station_id), "</b><br>",
    "<span style='color:", summary_label_color, ";font-size:12px'>Receiver: ",
    .fmt(latest$sg_id), "</span><br><br>",
    "<table style='border-collapse:collapse;width:100%;font-size:", summary_font_size, "'>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last visit</td>",
      "<td>", .fmt(latest$visit_date), " (", .fmt(latest$technician), ")</td></tr>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Status on departure</td>",
      "<td>Power: ", .status_token(latest$station_power_dep),
      "  |  WiFi: ", .status_token(latest$wifi_dep), "</td></tr>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last data download</td>",
      "<td>", last_dl_str, "</td></tr>",
    "<tr><td style='color:", summary_label_color, ";padding:2px 10px 2px 0;white-space:nowrap'>",
      "Last issue</td>",
      "<td>", .fmt(latest$issue_category), "</td></tr>",
    "</table>"
  )

  # Scrollable visit history table (all visits, newest first)
  history_rows_html <- paste0(
    vapply(seq_len(nrow(rows_all)), function(i) {
      r <- rows_all[i, ]
      paste0(
        "<tr style='border-top:", history_row_border, "'>",
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
    "<span style='font-size:12px;color:", summary_label_color, "'>",
      "Visit history (", nrow(rows_all), " visit",
      ifelse(nrow(rows_all) == 1, "", "s"), ")</span>",
    "<div style='max-height:", history_max_height, ";overflow-y:auto;margin-top:4px'>",
    "<table style='border-collapse:collapse;width:100%;font-size:", history_font_size, "'>",
    "<thead>",
    "<tr style='background:", history_header_bg, ";color:", history_header_color,
    ";font-size:11px;position:sticky;top:0'>",
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
    marker_fill_for(lv$station_power_dep, lv$wifi_dep)
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
