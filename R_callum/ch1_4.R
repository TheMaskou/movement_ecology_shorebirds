# Load packages
library(readxl)
library(dplyr)
library(leaflet)
library(DT)
library(htmltools)

# Overview on our Motus stations part of our local array located in the Hunter estuary, Newcastle (NSW).

# Read data
motus_data <- read_excel(path_maintenance_log)

# 1. Convert lat/lon columns to numeric (E/W, N/S)
motus_data <- motus_data %>%
  mutate(
    sg_lon = as.numeric(sub("([0-9.]+).*", "\\1", sg_lon)) * ifelse(grepl("W", sg_lon), -1, 1),
    sg_lat = as.numeric(sub("([0-9.]+).*", "\\1", sg_lat)) * ifelse(grepl("S", sg_lat), -1, 1)
  )

# 2. Get last data download per station_id (where data_downloaded == "yes")
last_dl <- motus_data %>%
  filter(tolower(data_downloaded) == "yes") %>%
  arrange(station_id, visit_date) %>%
  group_by(station_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(
    station_id,
    last_data_download = visit_date,
    last_tech_download = technician
  )

# 3. Join back to main table
motus_data2 <- motus_data %>%
  left_join(last_dl, by = "station_id")

# 4. Photo directory and file list
photo_dir <- "C:/Users/marin/OneDrive - The University Of Newcastle/Documents/6-PHD_UON/11-PhD project/Chapters/Chapter 1 - Motus/Array maintenance/reports/log_photo"

photo_files <- list.files(photo_dir, full.names = FALSE)

# 5. Match filename by objectid and build relative URL
motus_data2 <- motus_data2 %>%
  rowwise() %>%
  mutate(
    photo_file = {
      oid <- as.character(objectid)
      p <- grep(paste0("^", oid, "_"), photo_files, value = TRUE)
      if (length(p) > 0) p[1] else NA_character_
    }
  ) %>%
  ungroup() %>%
  mutate(
    photo_url = ifelse(
      is.na(photo_file),
      NA_character_,
      paste0("log_photo/", photo_file)   # relative path for map + table
    )
  )


# Find below a dynamic map with the last info concerning the status of each stations part of the local Motus array

# Select only needed fields for popups
popup_data <- motus_data2 %>%
  mutate(
    popup = paste0(
      "<b>Station ID:</b> ", station_id, "<br>",
      "<b>Receiver ID:</b> ", sg_id, "<br>",
      "<b>Station status:</b> ", station_status_left, "<br>",
      "<b>Last visit:</b> ", visit_date, " (", technician, ")", "<br>",
      "<b>Last data download:</b> ", last_data_download, " (", last_tech_download, ")", "<br>",
      "<b>Last issues reported:</b> ", issue_category, "<br>",
      ifelse(
        is.na(photo_url),
        "",
        paste0("<br><a href='", photo_url, "' target='_blank'>📷 View photo</a>")
        
      )
    )
  )



# Create the map
leaflet(popup_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~sg_lon,
    lat = ~sg_lat,
    radius = 6,
    color = "#1b6ca8",
    fillOpacity = 0.8,
    popup = ~popup
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap()

# And retrieve the tables corresponding to each stations with its complete maintenance history.
drop_cols <- c(
  "globalid", "start", "end", "today",
  "sg_id_calc", "sg_version_calc",
  "sg_lon_calc", "sg_lat_calc"
)

table_data <- motus_data2 %>%
  select(-any_of(drop_cols)) %>%
  relocate(station_id, visit_date, .before = 1)

# 0-based indices for DT
hidden_cols <- which(names(table_data) %in% c(
  "objectid", "CreationDate", "Creator",
  "EditDate", "Editor", "photo_file"
)) - 1

datatable(
  table_data,
  filter   = "top",
  rownames = FALSE,
  class    = "stripe hover row-border order-column compact",
  extensions = c("Buttons", "FixedColumns"),
  options  = list(
    pageLength = 15,
    scrollX    = TRUE,
    autoWidth  = TRUE,
    dom        = "Bfrtip",
    buttons    = c("copy", "csv", "excel"),
    columnDefs = list(
      list(visible = FALSE, targets = hidden_cols)
    ),
    fixedColumns = list(leftColumns = 2)  # station_id + visit_date
  )
)