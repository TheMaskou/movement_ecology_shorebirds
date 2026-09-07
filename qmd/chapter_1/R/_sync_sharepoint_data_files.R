# ==== Description ====
# This script automatically copies data files from the shorebird SharePoint to
# the local project data folder.
# 
# Files copied include:
# - historic receiver log
# - Survey123 receiver log
# - tide data
# - shorebird catching / band numbers
# - receiver list
#
# To use this, you must first define the path to the SharePoint on your computer
# as a project-level environment variable. Basically, this allows anyone to run
# this script (even on different computers with different SharePoint locations).

# Only works because .Renviron has been added to .gitignore.

# ==== How To Set Up ====
# This only need to be done once (per computer).

# 1. run the following command in console to open your project-level .Renviron
# "usethis::edit_r_environ("project")

# 2. Add a new environment variable called SHOREBIRD_SHAREPOINT_PATH, 
# assigned to the full path to the SharePoint on your computer. 
# For example, for me (Lily), the full line is:

# SHOREBIRD_SHAREPOINT_PATH = "/home/lily-work/SharePoint-Shorebirds/General"

# 3. Restart R session so the Renviron is reloaded

# ==== Load Packages ====
source(here::here("qmd", "chapter_1", "R", "globals.R"))

# ==== Define File Paths ====

# !!!! DEFINE THE FILE NAMES OF THE SPREADSHEETS AS THEY ARE IN SHAREPOINT
# HERE !!!!

sharepoint_receiver_log_historic <- "motus_receiver_log_historic_callum.xlsx"
sharepoint_receiver_log_survey123 <- "arcgis-123survey_output_table_raw-from-app-maxime.xlsx"
sharepoint_receiver_list <- "receivers.csv"
sharepoint_shorebird_numbers <- "SHOREBIRD NUMBER TRACKING.xlsx"
sharepoint_tide_data <- "TideDataNewcastle.csv"
sharepoint_antenna_log <- "antenna_log_motus_294.xlsx"

path_sharepoint_receiver_log_historic <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_receiver_log_historic)
path_sharepoint_receiver_log_survey123 <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_receiver_log_survey123)
path_sharepoint_receiver_list <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_receiver_list)
path_sharepoint_shorebird_numbers <- here::here(dir_sharepoint, sharepoint_shorebird_numbers)
path_sharepoint_tide_data <- here::here(dir_sharepoint, "Motus_R", "data", "tides", sharepoint_tide_data)
path_sharepoint_antenna_log <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_antenna_log)

# Destinations folder are gitignored, so it may not exist on a fresh clone.
# Create directories in case (but having .gitkeep files should make this redundant)
dir.create(dirname(path_motus_receiver_log_historic), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_shorebird_numbers), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(path_tide_data_spreadsheet), recursive = TRUE, showWarnings = FALSE)

# ==== Copy Files From SharePoint ====

# Copy the files - note that regardless of the name in the sharepoint, they
# will be renamed to whatever is defined in globals.R.
file.copy(path_sharepoint_receiver_log_historic, path_motus_receiver_log_historic, overwrite = TRUE)
file.copy(path_sharepoint_receiver_log_survey123, path_motus_receiver_log_survey123, overwrite = TRUE)
file.copy(path_sharepoint_receiver_list, path_motus_receiver_list, overwrite = TRUE)
file.copy(path_sharepoint_shorebird_numbers, path_shorebird_numbers, overwrite = TRUE)
file.copy(path_sharepoint_tide_data, path_tide_data_spreadsheet, overwrite = TRUE)
file.copy(path_sharepoint_antenna_log, path_antenna_log, overwrite = TRUE)

# TODO: Implement useful checks to help user
# if (is.na(dir_sharepoint) || !nzchar(dir_sharepoint)) {
#   stop(
#     'SHOREBIRD_SHAREPOINT_PATH is not set. Run usethis::edit_r_environ("user"), add:\n',
#     '  SHOREBIRD_SHAREPOINT_PATH="C:/Users/you/OneDrive - .../Motus Stuff"\n',
#     "then restart R.", call. = FALSE
#   )
# }
# if (!dir.exists(dir_sharepoint)) {
#   warning("SHOREBIRD_SHAREPOINT_PATH is set but not found (OneDrive not synced?): ", dir_sharepoint)
# }

