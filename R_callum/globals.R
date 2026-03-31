motus_proj_num <- 294

# ==== Directories and File Paths ====
dir_motus <- here::here("qmd", "chapter_1", "data", "motus")
dir_tides <- here::here("qmd", "chapter_1", "data", "tides")

# Inputs / raw data
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

# ==== Species Constants ====
species <- tibble::tribble(
  ~code, ~english,                 ~scientific,                       ~class,      ~color,
  "BTG", "Bar-tailed Godwit",     "Limosa lapponica",                "migratory", "#FF1493",
  "FEC", "Far Eastern Curlew",    "Numenius madagascariensis",       "migratory", "#FF8C00",
  "ML",  "Masked Lapwing",        "Vanellus miles",                  "resident",  "#32CD32",
  "PGP", "Pacific Golden-Plover", "Pluvialis fulva",                 "migratory", "#FFD700",
  "PS",  "Pied Stilt",            "Himantopus leucocephalus",        "resident",  "#00CED1",
  "CS",  "Curlew Sandpiper",      "Calidris ferruginea",             "migratory", "#9370DB",
  "RNA", "Red-necked Avocet",     "Recurvirostra novaehollandiae",   "resident",  "#FF4500",
  "EW",  "Eurasian Whimbrel",     "Numenius phaeopus",               "migratory", "#1E90FF"
)

## ---- Derived Named Vectors ----
species_colors      <- setNames(species$color, species$english)
species_init_colors <- setNames(species$color, species$code)
shorebird_class     <- setNames(species$class, species$english)
