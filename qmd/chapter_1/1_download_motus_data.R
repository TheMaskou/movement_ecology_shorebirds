## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Fetch detection data from Motus server
## Created:  2025 July 

## PACKAGE
library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 

## RETRIEVE DATA
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
Sys.setenv(TZ="UTC") 
proj.num <- 294       
motusLogout()
sql.motus <- tagme(projRecv = proj.num,
                   new = FALSE, # FALSE overwrites existing file, TRUE creates a new file
                   update = TRUE,
                   dir = here("qmd", "chapter_1", "data", "motus"))
metadata(sql.motus, proj.num)

# Below = testing / not needed
## QUICK CHECK FOR LAST DATA (43288 is test tag)

# Below commented out as I would suggest this belongs in a separate cleaning script

# df.alltags <- tbl(sql.motus, "alltags") %>%
#   dplyr::collect() %>%
#   as.data.frame() %>%
#   mutate(time = as_datetime(ts),
#          timeAus = as_datetime(ts, tz = "Australia/Sydney"),
#          dateAus = as_date(timeAus),
#          year = year(time), 
#          day = yday(time)) 
# 
# tail(df.alltags %>% 
#        arrange(timeAus) %>%
#        select(timeAus, speciesEN, motusTagID, tagModel, pulseLen, recvDeployName, recv))
