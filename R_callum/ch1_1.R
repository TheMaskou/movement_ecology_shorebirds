## ----style legend, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = FALSE, results = 'hide'----
# Source the .qmd that set styles and codex into that environment
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_3.qmd"), 
                   output = tempfile(fileext = ".R"), 
                   quiet = TRUE))


## ----1 packages, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# install.packages("motus", 
#                  repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                            CRAN = 'https://cloud.r-project.org'))
library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 
library(ggplot2) 

# Code for downloading Motus data redundant to include here. Should be left in
# a separate script. (Could be sourced here)

# Load Data
sql.motus <- DBI::dbConnect(SQLite(), here("data", ))


## ----1 data quick view, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

tail(df.alltags %>% 
       arrange(timeAus) %>%
       select(timeAus, speciesEN, motusTagID, tagModel, pulseLen, recvDeployName, recv)) 


## ----1 filter the data, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Cleaning and correcting tags metadata
df.alltags <- df.alltags %>% 
   filter(
    # test tags
     motusTagID != c("43291"),
    # pending, unconfirmed or undeployed tags
    !motusTagID %in% c("43288", "43291", "43297", "43299",
                       "43307", "43424", "43425", "60470", 
                       "60579", "81123", "81136", "81137"),
    # used for test/validation before tagging bird (remove time before the tagging)
    !(motusTagID == "81134" & time < dmy("23-11-2024")),
    !(motusTagID == "60575" & time < dmy("25-10-2023")) ) %>% 
    # NA species
     mutate(speciesEN = case_when(
       is.na(speciesEN) & motusTagID %in% c("60470", "81121") ~ "Red-necked Avocet",
       is.na(speciesEN) & motusTagID %in% c("81118") ~ "Red-necked Avocet",
       TRUE ~ speciesEN)) %>%
  
  # motusTagID as factor
  mutate(motusTagID = as.character(motusTagID))

  
# Cleaning and correcting receiver metadata
df.alltags <- df.alltags %>% 
  filter(
    # NA
     !is.na(recvDeployLat),
    # site not any longer used
      recvDeployName != c("Throsby Creek Test Site"),
    # test sensor gnome
      recv != c("SG-C621RPI3E17F",       
                "SG-62A5RPI36710") ) %>% 
    # Windeyers
  mutate(recvDeployName = ifelse(is.na(recvDeployName) & recv == "SG-D5BBRPI3E2F7",
                                 "Windeyers", 
                                 recvDeployName))




## ----2 filter the noise plot things, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Checking 'motusFiltered in' tag data
ggplot(df.alltags %>%
         filter(motusFilter == 1),
       aes(x = recvDeployName)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Motus Station", y = "Nb of motusFilter = 1 (good)") 

# Checking 'motusFiltered out' tag data
ggplot(df.alltags %>%
         filter(motusFilter == 0),
       aes(x = recvDeployName)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Motus Station", y = "Nb of motusFilter = 0 (filtered out)")

# Checking proportion of data quality for each station
perc <- ggplot(df.alltags %>% 
                 filter(motusFilter %in% c(0, 1)),
       aes(x = recvDeployName, fill = factor(motusFilter))) +
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("0" = "orange",
                               "1" = "steelblue"),
                    labels = c("0 (filtered out)", 
                               "1 (good)"),
                    name = "motusFilter") +
  theme_minimal() +
  labs(x = "Motus Station",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ggpubr::ggexport(perc,
#                  filename = here("figures", "motus_filter_perc.jpg"),
#                  width = 800, height = 1000)

perc


## ----2 filter the noise, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data
         runLen >= 3) # value might be further thought




## ----2 filter the doubledetec, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Ambiguous  
clarify(sql.motus) 



## ----filter recv, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Get summary 
df.recvDeps <- tbl(sql.motus, "recvDeps") %>%   
  collect() %>%   
  as.data.frame() %>%    
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"), 
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney")) 


## ----recever names ex, message = FALSE, warning = FALSE, eval = FALSE, echo = TRUE----
# # Correct the recv names (example)
# station_rename <- list("Barry_Fullerton_cove"  = "Fullerton Entrance",
#                        "North Swann Pond" = "Swan Pond" ,
#                        "Example_three" = "Example 3")
# 


## ----filter recv 2, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Apply corrections
df.recvDeps <- df.recvDeps %>% 
  mutate(name = recode(name, !!!station_rename)) %>%
  rename(recvDeployName = "name")

# Filter not used stations as out of the local array 
df.recvDeps <- df.recvDeps %>% 
  filter(!is.na(latitude),   
         recvDeployName != c("Throsby Creek Test Site"),    
         serno != c("SG-C621RPI3E17F",   # test station
                    "SG-62A5RPI36710") ) # test station



## ----update T/F, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

# This avoid to treat the complete motus dataset everytime (takes a while)
# So if update = T, compare last and new data to keep only new ones. Also set the run_analysis = T if new data are != 0 (it may happen when set update = T but there is no new data since last run), this allows the following junks to run with eval = run_analysis. If update = F and you rewrite the whole data set, run_analysis = T per default.

if (update == TRUE) {
  
# Load last data
df.alltags.past <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) 

# Keep only new data
df.alltags <- df.alltags %>%
    filter(!timeAus %in% df.alltags.past$timeAus)

}

if (update == TRUE) {
  run_analysis <- nrow(df.alltags) > 0 
} else {
  run_analysis <- TRUE
}
























## ----ready-to-go, message = FALSE, warning = FALSE, eval = FALSE, echo = FALSE----
# 
# save.image(file = here::here("qmd", "chapter_1", "data", "motus_data.RData"))
# 

