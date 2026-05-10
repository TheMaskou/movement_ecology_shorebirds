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
                   new = FALSE, # TRUE overwrites existing file
                   update = TRUE,
                   dir = here("qmd", "chapter_1","data"))
metadata(sql.motus, proj.num)

## QUICK CHECK FOR LAST DATA (43288 is test tag)
df.alltags <- tbl(sql.motus, "alltags") %>%
  dplyr::collect() %>%
  as.data.frame() %>%
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time), 
         doy = yday(time)) 

tail(df.alltags %>% 
       arrange(timeAus) %>%
       select(timeAus, speciesEN, motusTagID, tagModel, pulseLen, recvDeployName, recv))

# ## Particular checking
# sql.motus <- dbConnect(SQLite(), here::here("qmd", "chapter_1", "data", "project-294.motus"))
# 
# df.alltags <- tbl(sql.motus, "alltags") %>%
#   dplyr::collect() %>%
#   as.data.frame() %>%
#   mutate(time = as_datetime(ts),
#          timeAus = as_datetime(ts, tz = "Australia/Sydney"),
#          dateAus = as_date(timeAus),
#          year = year(time), 
#          doy = yday(time)) 
# 
# df <- df.alltags %>%
#        arrange(timeAus) %>%
#        select(timeAus, speciesEN, motusTagID, tagModel, pulseLen, recvDeployName, recv) %>%
# 
#   filter(
#     # motusTagID != "43288", # test tag
#     # recv != c("SG-CE0DRPI43C5E", "SG-CE0DRPI43C5E"), # andrea, andrew
#     recv == "SG-E3E4RPI4CED5"
#     )


