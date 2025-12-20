## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Fetch detection data from Motus server
## Created:  2025 July 


library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
Sys.setenv(TZ="UTC") 
proj.num <- 294       
motusLogout()
sql.motus <- tagme(projRecv = proj.num,
                   new = FALSE, # TRUE overwrites existing file
                   update = TRUE,
                   dir = here("qmd", "chapter_1","data"))
metadata(sql.motus, proj.num)

