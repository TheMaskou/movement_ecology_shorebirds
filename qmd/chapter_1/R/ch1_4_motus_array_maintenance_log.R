source(here::here("qmd", "chapter_1", "R", "globals.R"))

library(dplyr)
library(here)
library(openxlsx2)

log_historic <- wb_to_df(
  file = path_motus_receiver_log_historic,
  sheet = "log"
)

# NOTE: At the time of writing, there was only a single worksheet in the 
# spreadsheet. If more are added, should specify the sheet = "" argument.
# But it defaults to using the first sheet anyway, so should be fine.
log_survey123 <- wb_to_df(
  file = path_motus_receiver_log_survey123,
)
