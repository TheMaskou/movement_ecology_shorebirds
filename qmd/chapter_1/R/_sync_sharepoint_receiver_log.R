# ==== Description ====
# This script automatically copies the receiver log spreadsheets (historic + 
# Survey123) from the SharePoint, into the project data folder.

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
# For example, for me (Callum), the full line is:

# SHOREBIRD_SHAREPOINT_PATH = "C:\Users\callu\OneDrive - Student\The University of Newcastle\StudentGroupPhD - Louise Williams and Mattea Taylor - General"

# 3. Restart R session so the Renviron is reloaded

# ==== Load Packages ====
source(here::here("qmd", "chapter_1", "R", "globals.R"))

# ==== Define File Paths ====

# !!!! DEFINE THE FILE NAMES OF THE SPREADSHEETS AS THEY ARE IN SHAREPOINT
# HERE !!!!

sharepoint_receiver_log_historic <- "motus_receiver_log_historic_callum.xlsx"
sharepoint_receiver_log_survey123 <- "arcgis-123survey_output_table_raw-manually-updated-callum.xlsx"

path_sharepoint_receiver_log_historic <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_receiver_log_historic)
path_sharepoint_receiver_log_survey123 <- here::here(dir_sharepoint, "Motus_array_maintenance", sharepoint_receiver_log_survey123)

# ==== Copy Files From SharePoint ====

# Copy the files - note that regardless of the name in the sharepoint, they
# will be renamed to whatever is defined in globals.R.
file.copy(path_sharepoint_receiver_log_historic, path_motus_receiver_log_historic, overwrite = TRUE)
file.copy(path_sharepoint_receiver_log_survey123, path_motus_receiver_log_survey123, overwrite = TRUE)

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

