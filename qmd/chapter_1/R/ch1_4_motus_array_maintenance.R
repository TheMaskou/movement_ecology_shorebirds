# Load packages
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(readr)
library(here)
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
  station_power_departure   = "Power (dep)",
  wifi_departure            = "WiFi (dep)",
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

## ---- Popup Antenna Table ----
antenna_section_open <- FALSE    # TRUE = section expanded when the popup opens
antenna_font_size    <- "12px"
antenna_max_height   <- "160px"  # scrolls beyond this (no station currently needs it)

# Field -> column header for the antenna table. Edit / reorder / remove entries
# here to change the table — column order follows this vector's order.
# Available fields: port, antenna_type, magnetic_bearing, true_bearing,
# height_m, mount_type, install_date.
antenna_fields <- c(
  port         = "Port",
  antenna_type = "Type",
  true_bearing = "Bearing (°T)"
)

## ---- Antenna Range Overlay ----
# VERY APPROXIMATE shapes showing the rough form of each antenna type's
# pattern (round vs. one-way beam vs. two-way beam). These are NOT modelled
# detection ranges - do not read distances off them.

show_antenna_ranges         <- TRUE   # FALSE = none of the overlay code below runs
antenna_ranges_shown_on_load <- TRUE  # FALSE = layer starts unticked in the control
antenna_range_group         <- "Antenna range (approx.)"   # label on the layer toggle

antenna_range_fill_color     <- "#1f77b4"
antenna_range_fill_opacity   <- 0.15
antenna_range_border_color   <- "#1f77b4"
antenna_range_border_weight  <- 1
antenna_range_border_opacity <- 0.6

# One row per antenna type, matching the antenna log's "Type" column exactly.
# A type present in the log but missing here is skipped with a warning rather
# than erroring, so a new/renamed type doesn't silently break the map.
#   shape         "omni" = circle around the station; "lobe" = directional teardrop
#   range_m       how far the shape reaches from the station, in metres
#   beamwidth_deg angular width of the lobe (ignored for "omni"); smaller = narrower
#   n_lobes       1 = one lobe on the recorded bearing; 2 = plus a mirrored
#                 lobe 180 degrees opposite (e.g. a bidirectional H antenna)
antenna_range_spec <- tibble::tribble(
  ~antenna_type,    ~shape,  ~range_m, ~beamwidth_deg, ~n_lobes,
  "monopole",       "omni",      1000,             NA,       NA,
  "H antenna",      "lobe",      2500,             90,        2,
  "3-element Yagi", "lobe",      4000,             70,        1,
  "6-element Yagi", "lobe",      6000,             45,        1
)

# ==== Load Data ====
maintenance_log <- readRDS(path_maintenance_log)

# ==== Load and Tidy Antenna Log ====
# Manual per-antenna record (Port#, Type, bearings, ...), one row per antenna.
# Not yet used anywhere else in the pipeline - loaded here purely to feed the
# popup's antenna section below.
#
# The sheet has two columns literally named "Type" (antenna type, then mount
# type e.g. Tower/Existing); wb_to_df() returns both as "Type", so
# make.unique() disambiguates the second to "Type.1" before selecting.

antenna_raw <- wb_to_df(path_antenna_log, sheet = "antennae")
names(antenna_raw) <- make.unique(names(antenna_raw))

# target_name = source_column_name. Edit here (and antenna_fields, settings
# above) if the SharePoint sheet's columns are ever renamed/restructured.
antenna_cols <- c(
  site             = "Site",
  install_date     = "Install_Date",
  removal_date     = "Removal_Date",
  port             = "Port#",
  antenna_type     = "Type",
  magnetic_bearing = "Magnetic_Bearing",
  true_bearing     = "True_Bearing",
  height_m         = "Height_m",
  mount_type       = "Type.1"
)

missing_antenna_cols <- setdiff(unname(antenna_cols), names(antenna_raw))
if (length(missing_antenna_cols) > 0) {
  stop(
    "antenna_log_motus_294.xlsx ('antennae' sheet) is missing expected ",
    "column(s): ", toString(missing_antenna_cols), ". Update antenna_cols ",
    "in ch1_4_motus_array_maintenance.R to match the sheet's current headers."
  )
}

## ---- Helper: Canonical Station Name ----
# The antenna log records site names as they appear on the Motus website,
# which differ from the maintenance log's station_id for three sites.
# station_rename (globals.R) already covers those - but the antenna log uses
# a typographic apostrophe (U+2019) in "Milham’s Pond", so normalise that
# first to match station_rename's straight-apostrophe key.
canonical_station <- function(x) {
  x       <- trimws(gsub("’", "'", as.character(x)))
  renamed <- unname(unlist(station_rename)[x])
  ifelse(is.na(renamed), x, renamed)
}

antenna_log <- antenna_raw |>
  select(all_of(antenna_cols)) |>
  mutate(
    station_id = canonical_station(site),
    port       = as.integer(port)
  )

## ---- Coordinate Guard (Antenna Log) ----
# Warn if any antenna-log site has no match in the maintenance log, as its
# antennae would otherwise be silently dropped from the popup.
unmatched_antenna_sites <- setdiff(
  unique(antenna_log$station_id), unique(maintenance_log$station_id)
)
if (length(unmatched_antenna_sites) > 0) {
  warning(
    "These antenna log site(s) have no matching station_id in the ",
    "maintenance log and will be excluded from popups: ",
    toString(unmatched_antenna_sites)
  )
}

# Antennae currently in the field (no removal_date) vs. historically removed
# (removal_date is empty for every row today, but the column exists for when
# that changes).
antenna_active <- antenna_log |>
  filter(is.na(removal_date)) |>
  arrange(station_id, port)

antenna_removed_counts <- antenna_log |>
  filter(!is.na(removal_date)) |>
  count(station_id, name = "n_removed")

# ==== Load and Attach Station Coordinates ====
# Coordinates are sourced from receivers.csv for every row (both the
# historic and Survey123 entries), replacing the per-row sg_lon / sg_lat
# columns that were only populated for Survey123 entries.
#
# Note that this receivers.csv is sourced from SharePoint, and it must be
# manually updated in SharePoint when any changes are made (e.g., new receiver
# deployed at a site).
#
# The format in the CSV is e.g. "151.681010702398E" / "32.846728923395S".
# Conversion: extract the leading number with sub(), then negate if W or S.

station_coords <- read_csv(
  here::here("data", "motus", "receivers.csv"),
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
# Warn if any station_id in the log has no match in receivers.csv, as
# those stations will be silently excluded from the map.
missing_coords <- maintenance_log |>
  filter(is.na(sg_lon) | is.na(sg_lat)) |>
  distinct(station_id) |>
  pull(station_id)

if (length(missing_coords) > 0) {
  warning(
    "These station_id values have no match in receivers.csv and will be ",
    "excluded from the map: ", toString(missing_coords)
  )
}

# ==== Antenna Range Geometry ====
# VERY APPROXIMATE overlay shapes only - see the settings block above for what
# each variable controls. Nothing in this section runs unless
# show_antenna_ranges is TRUE, so the rest of the map is unaffected when it's
# FALSE.
if (isTRUE(show_antenna_ranges)) {

  antenna_range_pane <- "antennaRangePane"

  ## ---- Helper: Metres Offset to Lon/Lat ----
  # Flat-earth approximation (good to well under a metre at these ranges and
  # this latitude) - avoids pulling in sf/geosphere for a shape that's
  # explicitly not meant to be read precisely.
  metres_to_lonlat <- function(lon, lat, dx_m, dy_m) {
    metres_per_deg_lat <- 111320
    dlat <- dy_m / metres_per_deg_lat
    dlon <- dx_m / (metres_per_deg_lat * cos(lat * pi / 180))
    list(lon = lon + dlon, lat = lat + dlat)
  }

  ## ---- Helper: Directional Lobe Polygon ----
  # A teardrop pointing along bearing_deg (clockwise from north), reaching
  # range_m at the bearing and tapering to 0 at +-180 degrees so it closes
  # cleanly back on the station. beamwidth_deg sets the taper: radius is half
  # of range_m at +-(beamwidth_deg / 2) off the bearing.
  lobe_polygon <- function(lon, lat, bearing_deg, range_m, beamwidth_deg, n_points = 90) {
    delta_deg      <- seq(-180, 180, length.out = n_points)
    half_width_rad <- beamwidth_deg * pi / 720
    taper          <- log(0.5) / log(cos(half_width_rad))
    r              <- range_m * pmax(cos(delta_deg * pi / 360), 0) ^ taper
    bearing_rad    <- (bearing_deg + delta_deg) * pi / 180
    offset         <- metres_to_lonlat(lon, lat, r * sin(bearing_rad), r * cos(bearing_rad))
    data.frame(lon = offset$lon, lat = offset$lat)
  }

  ## ---- Helper: Combine Separate Polygons for a Single addPolygons() Call ----
  # leaflet's addPolygons() draws multiple disjoint shapes from one pair of
  # lng/lat vectors using the same convention as base R's polygon() - an NA
  # row marks the break between shapes.
  combine_polygons <- function(polys) {
    lng <- unlist(lapply(polys, function(p) c(p$lon, NA)))
    lat <- unlist(lapply(polys, function(p) c(p$lat, NA)))
    n   <- length(lng)
    list(lng = lng[-n], lat = lat[-n])
  }

  ## ---- Antenna Type -> Range Spec Guard ----
  unmatched_antenna_range_types <- setdiff(
    unique(antenna_active$antenna_type), antenna_range_spec$antenna_type
  )
  if (length(unmatched_antenna_range_types) > 0) {
    warning(
      "These antenna type(s) have no entry in antenna_range_spec and will ",
      "be excluded from the antenna range overlay: ",
      toString(unmatched_antenna_range_types)
    )
  }

  # Active antennae with known coordinates and a matching spec row. Uses
  # station_coords (from receivers.csv) rather than the antenna log's own
  # Latitude/Longitude columns, so shapes originate exactly at the station
  # marker.
  antenna_range_base <- antenna_active |>
    inner_join(antenna_range_spec, by = "antenna_type") |>
    left_join(station_coords, by = "station_id") |>
    filter(!is.na(sg_lon), !is.na(sg_lat))

  ## ---- Omnidirectional Antennae ----
  antenna_omni <- antenna_range_base |>
    filter(shape == "omni") |>
    mutate(label = paste0(station_id, " — port ", port, ", ", antenna_type))

  ## ---- Directional Antennae ----
  # Bearings are parsed defensively (as fmt_antenna_cell() does for the same
  # column) because a blank cell can read back as "TODO" rather than NA - see
  # Tomago ports 2-3 in the antenna log.
  antenna_lobe_rows <- antenna_range_base |>
    filter(shape == "lobe") |>
    mutate(bearing_num = suppressWarnings(as.numeric(true_bearing)))

  missing_bearing_antennae <- antenna_lobe_rows |> filter(is.na(bearing_num))
  if (nrow(missing_bearing_antennae) > 0) {
    warning(
      "These antenna(e) have no usable true_bearing and will be excluded ",
      "from the antenna range overlay: ",
      toString(paste0(
        missing_bearing_antennae$station_id, " (port ",
        missing_bearing_antennae$port, ")"
      ))
    )
  }
  antenna_lobe_rows <- antenna_lobe_rows |> filter(!is.na(bearing_num))

  # Antennae with n_lobes == 2 (e.g. H antenna) get a second, mirrored lobe
  # 180 degrees opposite the recorded bearing.
  antenna_lobe_specs <- bind_rows(
    antenna_lobe_rows |> mutate(lobe_bearing = bearing_num),
    antenna_lobe_rows |> filter(n_lobes == 2) |>
      mutate(lobe_bearing = (bearing_num + 180) %% 360)
  ) |>
    mutate(label = paste0(
      station_id, " — port ", port, ", ", antenna_type,
      " (", round(lobe_bearing), "°)"
    ))

  antenna_lobe_polys <- lapply(seq_len(nrow(antenna_lobe_specs)), function(i) {
    row <- antenna_lobe_specs[i, ]
    lobe_polygon(row$sg_lon, row$sg_lat, row$lobe_bearing, row$range_m, row$beamwidth_deg)
  })
  antenna_lobe_coords <- combine_polygons(antenna_lobe_polys)
  antenna_lobe_labels <- antenna_lobe_specs$label
}

## ---- Add Antenna Range Layers to a Map ----
# No-op when show_antenna_ranges is FALSE - returns the map unchanged, so
# nothing below this point needs to know the overlay exists.
add_antenna_ranges <- function(map) {
  if (!isTRUE(show_antenna_ranges)) return(map)

  map <- map |> addMapPane(antenna_range_pane, zIndex = 350)

  if (nrow(antenna_omni) > 0) {
    map <- map |> addCircles(
      data        = antenna_omni,
      lng         = ~sg_lon,
      lat         = ~sg_lat,
      radius      = ~range_m,
      stroke      = TRUE,
      color       = antenna_range_border_color,
      weight      = antenna_range_border_weight,
      opacity     = antenna_range_border_opacity,
      fill        = TRUE,
      fillColor   = antenna_range_fill_color,
      fillOpacity = antenna_range_fill_opacity,
      label       = ~label,
      group       = antenna_range_group,
      options     = pathOptions(pane = antenna_range_pane)
    )
  }

  if (length(antenna_lobe_coords$lng) > 0) {
    map <- map |> addPolygons(
      lng         = antenna_lobe_coords$lng,
      lat         = antenna_lobe_coords$lat,
      stroke      = TRUE,
      color       = antenna_range_border_color,
      weight      = antenna_range_border_weight,
      opacity     = antenna_range_border_opacity,
      fill        = TRUE,
      fillColor   = antenna_range_fill_color,
      fillOpacity = antenna_range_fill_opacity,
      label       = antenna_lobe_labels,
      group       = antenna_range_group,
      options     = pathOptions(pane = antenna_range_pane)
    )
  }

  map
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
  if (field %in% c("wifi_departure", "wifi_arrival")) return(.fmt(wifi_display(value)))
  if (field == "sg_version")                    return(.fmt(sg_version_short(value)))
  .fmt(value)
}

## ---- Helper: Format an Antenna-table Cell ----
# Bearing fields: blank / NA / literal "NA" -> na_placeholder; numeric ->
# value with a degree sign; anything else (e.g. a "TODO" placeholder still in
# the spreadsheet) is passed through as typed, so it stays visible rather
# than silently disappearing. Everything else falls through to .fmt().
fmt_antenna_cell <- function(field, value) {
  if (field %in% c("magnetic_bearing", "true_bearing")) {
    raw <- trimws(as.character(value))
    if (is.na(value) || raw == "" || toupper(raw) == "NA") return(na_placeholder)
    num <- suppressWarnings(as.numeric(raw))
    if (!is.na(num)) return(paste0(htmlEscape(format(num, trim = TRUE)), "°"))
    return(htmlEscape(raw))
  }
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

## ---- Build Antenna Section for One Station ----
# Collapsible <details> block listing that station's active antennae, driven
# by antenna_fields (settings, top of script). Native HTML disclosure - no JS
# library needed, and it degrades to "always open" if unsupported.
antenna_section_html <- function(sid) {
  rows      <- antenna_active |> filter(station_id == sid)
  n_removed <- antenna_removed_counts |> filter(station_id == sid) |> pull(n_removed)
  n_removed <- if (length(n_removed) == 0) 0 else n_removed

  if (nrow(rows) == 0) {
    return(paste0(
      "<div style='font-size:12px;color:", summary_label_color, ";margin:6px 0'>",
      "Antennae — none recorded</div>"
    ))
  }

  removed_line <- if (n_removed > 0) {
    paste0(
      "<div style='font-size:11px;color:", summary_label_color, ";margin-top:4px'>",
      n_removed, " antenna", ifelse(n_removed == 1, "", "e"), " removed — see antenna log</div>"
    )
  } else ""

  cell_style <- "padding:3px 6px;vertical-align:top;white-space:nowrap"

  header_cells <- paste0(
    vapply(antenna_fields, function(label) {
      paste0("<th style='padding:4px 6px;text-align:left'>", htmlEscape(label), "</th>")
    }, character(1)),
    collapse = ""
  )

  body_rows <- paste0(
    vapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      cells <- vapply(names(antenna_fields), function(field) {
        paste0("<td style='", cell_style, "'>", fmt_antenna_cell(field, r[[field]]), "</td>")
      }, character(1))
      paste0("<tr style='border-top:", history_row_border, "'>",
             paste(cells, collapse = ""), "</tr>")
    }, character(1)),
    collapse = ""
  )

  paste0(
    "<details", if (antenna_section_open) " open" else "",
    " style='margin:6px 0;font-size:", antenna_font_size, "'>",
    "<summary style='cursor:pointer;color:", summary_label_color, ";font-size:12px'>",
    "Antennae (", nrow(rows), ")</summary>",
    "<div style='max-height:", antenna_max_height, ";overflow-y:auto;overflow-x:auto;margin-top:4px'>",
    "<table style='border-collapse:collapse;width:100%;font-size:", antenna_font_size, "'>",
    "<thead><tr style='background:", history_header_bg, ";color:", history_header_color,
    ";font-size:11px'>", header_cells, "</tr></thead>",
    "<tbody>", body_rows, "</tbody></table></div>",
    removed_line,
    "</details>"
  )
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
      "<td>Power: ", .status_token(latest$station_power_departure),
      "  |  WiFi: ", .status_token(wifi_display(latest$wifi_departure)), "</td></tr>",
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

  paste0(summary_html, antenna_section_html(latest$station_id), history_html)
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
    marker_fill_for(lv$station_power_departure, wifi_display(lv$wifi_departure))
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
  add_antenna_ranges() |>
  addLayersControl(
    baseGroups    = c("Map", "Satellite", "Street (OSM)"),
    overlayGroups = if (isTRUE(show_antenna_ranges)) antenna_range_group else character(0),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  (\(m) if (isTRUE(show_antenna_ranges) && !antenna_ranges_shown_on_load) {
     hideGroup(m, antenna_range_group)
   } else {
     m
   })() |>
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
