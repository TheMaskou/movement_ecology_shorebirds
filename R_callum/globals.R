motus_proj_num <- 294

# ==== Directories and File Paths ====
dir_motus <- here::here("qmd", "chapter_1", "data", "motus")
dir_tides <- here::here("qmd", "chapter_1", "data", "tides")

path_shorebird_number_spreadsheet <- here::here("qmd", "chapter_1", "data", "spreadsheet", "SHOREBIRD NUMBER TRACKING(Birds caught).csv")
path_detection_data <- here("qmd", "chapter_1", "data", "motus", "data.rds")
path_recv_info <- here("qmd", "chapter_1", "data", "motus", "recv-info.rds")
path_tideData <- here("qmd", "chapter_1", "data", "tides", "tideData.rds")
path_spreadsheet_data <- here("qmd", "chapter_1", "data", "spreadsheet", "spreadsheet_data.rds")

# ==== Motus Tags ====
motus_tags_test <- c(
  "43291"
)

motus_tags_undeployed <- c(
  "43288", "43291", "43297", "43299",
  "43307", "43424", "43425", "60470", 
  "60579", "81123", "81136", "81137"
)

# ==== Receivers / Stations ====
station_rename <- list("Barry_Fullerton_cove"  = "Fullerton Entrance",
                       "Fullerton entrance"  = "Fullerton Entrance",
                       "North Swann Pond" = "Swan Pond" ,
                       "Ramsar Road Floodgate" = "Ramsar Road",    
                       "Milham's Pond" = "Milhams Pond")  
