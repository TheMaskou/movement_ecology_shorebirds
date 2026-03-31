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

source(here::here("R_callum", "globals.R"))

# Code for downloading Motus data redundant to include here. Should be left in
# a separate script. (Could be sourced here)

# Load Data ====
sql.motus <- DBI::dbConnect(SQLite(), here("data", "motus", "project-294.motus"))

# List tables in sql.motus (good sanity check)
sql.motus |> DBI::dbListTables()

# ==== Extract detections table as a dataframe ====
# TODO: This should be elsewhere really
df.alltags <- tbl(sql.motus, "alltags") %>%
  dplyr::collect() %>%
  as.data.frame() 

# ==== Basic Data Cleaning ====
# For now, test using df.alltags from the motus_data.RData that Maxime provided
df.alltags <- readRDS(here("qmd", "chapter_1", "data", "motus", "df_alltags.rds"))
df.alltags_raw <- df.alltags

## ==== Date and Time ====
df.alltags <- df.alltags |> 
  # Date and Time
  # Note: time = in UTC (default for Motus data)
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time),
         day = yday(time))


# TODO: Couldn't verify as I only have the already filtered df.alltags
# (Will run full thing overnight)
## ==== Filter Tags ====

# Cleaning and correcting tags metadata
df.alltags <- df.alltags %>% 
  filter(
    # Exclude Test Tags
    !(motusTagID %in% motus_tags_test),
    # Exclude pending, unconfirmed or undeployed tags
    !motusTagID %in% motus_tags_undeployed,
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


## ==== Motusfilter Plots====

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




## ==== Motusfilter Filter ====

# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data
         runLen >= 3) # value might be further thought



## ----2 filter the doubledetec, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Ambiguous  
clarify(sql.motus) 


# ==== Receiver Deployment Table  ====
# TODO: this should probably be a separate script

# Convert receiver deployment table (recvDeps) to a dataframe
df.recvDeps <- tbl(sql.motus, "recvDeps") %>%   
  collect() %>%   
  as.data.frame() %>%    
  # Add timestamps (note tsStart and tsEnd are in UTC)
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"), 
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney")) 

df.recvDeps |> 
  filter(projectID == motus_proj_num) |> 
  select(name) |> 
  unique()

## ==== Update Receiver Names ====

# Correct the recv names (example)
station_rename <- list("Barry_Fullerton_cove"  = "Fullerton Entrance",
                       "North Swann Pond" = "Swan Pond" ,
                       "Example_three" = "Example 3")


# Apply corrections
df.recvDeps <- df.recvDeps %>% 
  mutate(name = recode(name, !!!station_rename)) %>%
  rename(recvDeployName = "name")

df.recvDeps |> 
  filter(projectID == motus_proj_num) |> 
  select(recvDeployName) |> 
  unique()

# TODO: Suggestion = just list stations we are interested in could be simpler
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


# Apply corrections
df.alltags <- df.alltags %>% 
  mutate(recvDeployName = recode(recvDeployName, !!!station_rename))

df.recvDeps <- df.recvDeps %>%   filter(timeStartAus > "2023-01-31 00:00:00 AEDT")

# ==== Band ID ====
# Call and extract last up to date Spreadsheet record (sync your one drive with the TEAMS channel first) 
tryCatch({
  write.csv(
    readxl::read_excel("C:/Users/c3541851/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/SHOREBIRD NUMBER TRACKING.xlsx"),
    file.path(here::here( "qmd", "chapter_1", "data", "spreadsheet"), paste0(Sys.Date(), "-teams.sheet", ".csv")),
    row.names = FALSE
  )
}, error = function(e) {
  write.csv(
    readxl::read_excel("C:/Users/marin/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/SHOREBIRD NUMBER TRACKING.xlsx"),
    file.path(here::here( "qmd", "chapter_1", "data", "spreadsheet"), paste0(Sys.Date(), "-teams.sheet", ".csv")),
    row.names = FALSE
  )
})

# Load df with date at the beginning 
spreadsheet <- read.csv(here::here( "qmd", "chapter_1", "data", "spreadsheet", paste0(Sys.Date(), "-teams.sheet.csv"))) %>%   
  
  # Keep only the tagged ones 
  filter(Radio.tag. == "Y") %>%      
  
  # Variable names
  rename(DateAUS.Trap = "Date", 
         motusTagID = "Motus.tag.ID", 
         speciesEN = "Species") %>%
  
  # Value names
  mutate(speciesEN = case_when(
    speciesEN == "Eastern Curlew" ~ "Far Eastern Curlew",
    speciesEN == "Black-winged Stilt" ~ "Pied Stilt",
    speciesEN == "Pacific Golden Plover" ~ "Pacific Golden-Plover",
    speciesEN == "Whimbrel" ~ "Eurasian Whimbrel",
    TRUE ~ speciesEN )) %>% 
  
  # Format
  mutate(motusTagID = as.factor(motusTagID),
         DateAUS.Trap = as.Date(DateAUS.Trap),
         Band.ID = as.factor(Band.ID)) %>%
  select(Band.ID, motusTagID, speciesEN, DateAUS.Trap, everything())  

# Format motusTagID for further merging
df.alltags <- df.alltags %>%
  mutate(motusTagID = as.factor(motusTagID))

# Join unique Band IDs for inconsistent motusTag (same bird re-tagged, etc) 
df.alltags <- left_join(df.alltags,
                        spreadsheet %>%
                          filter(is.na(Euthanised.)) %>%  
                          select(motusTagID, DateAUS.Trap, Band.ID, Bander), 
                        by = "motusTagID")

# ==== Tide Data ====
# Read tide.csv
tideData <- read.csv(here("qmd", "chapter_1", "data", "tides", "TideDataNewcastle.csv"))

# Format date and datetime columns
tideData <- tideData %>% mutate(
  date = dmy(date, tz = "Australia/Sydney"),
  tideDateTimeAus = dmy_hm(tideDateTimeAus, tz = "Australia/Sydney")
)

# Classify tides as diurnal or nocturnal
tideData <- tideData %>% mutate(
  sunriseNewc = sunrise(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
  sunsetNewc = sunset(date, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
  sunriseNewcTime = strftime(sunriseNewc, format = "%H:%M:%S", tz = "Australia/Sydney"),
  sunsetNewcTime = strftime(sunsetNewc, format = "%H:%M:%S", tz = "Australia/Sydney")
)

# Define either diurnal or nocturnal 
tideData <- tideData %>% mutate(
  day_night = case_when(
    tideDateTimeAus >= sunriseNewc & tideDateTimeAus <= sunsetNewc ~ "Diurnal",
    TRUE ~ "Nocturnal"
  )
)

# Categorise each tide by tidal/diel period
tideData <- tideData %>% mutate(  
  tideCategory = case_when(
    high_low == "Low" & day_night == "Diurnal" ~ "Diurnal_Low",
    high_low == "Low" & day_night == "Nocturnal" ~ "Nocturnal_Low",
    high_low == "High" & day_night == "Diurnal" ~ "Diurnal_High",
    high_low == "High" & day_night == "Nocturnal" ~ "Nocturnal_High") %>% 
    as_factor())

# Add numeric ID to each category, allowing for unique tide bins
tideData <- tideData %>%
  group_by(tideCategory) %>% 
  mutate(tideID = paste0(tideCategory, "_", row_number())) %>% 
  ungroup()

# Load useful functions from Callum Gapes work
tidalCurve <- readRDS(here::here("qmd", "chapter_1", "data", "tides", "tidalCurve.rds"))
tidalCurveFunc <- splinefun(tideData$tideDateTimeAus, tideData$tideHeight, method = "natural")
get.tideIndex <- function(time){ return(which.min(abs(tideData$tideDateTimeAus-time)))}

# Add key variables
df.alltags <- df.alltags  %>%
  
  # Positive signal strength (min. = 0) for plotting
  mutate(sigPositive = sig + abs(min(sig))) %>% 
  
  # Sunrise/set
  sunRiseSet(lat = "recvDeployLat", 
             lon = "recvDeployLon", 
             ts = "ts") %>% 
  mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
  
  # Tide
  mutate(tideHeight = tidalCurveFunc(timeAus),
         tideIndex = map_dbl(timeAus, get.tideIndex))

tide_values <- tideData[df.alltags$tideIndex, 
                        c("tideDateTimeAus",
                          "high_low",
                          "day_night",
                          "tideCategory",
                          "tideID",
                          "tideHeight")]

# Stick and factorise the variables
df.alltags <- df.alltags %>%
  mutate(tideDateTimeAus = tide_values$tideDateTimeAus,
         tideHighLow = as_factor(tide_values$high_low),
         tideDiel = as_factor(tide_values$day_night),
         tideCategory = as_factor(tide_values$tideCategory),
         tideCategoryHeight = tide_values$tideHeight,
         tideID = as_factor(tide_values$tideID),
         tideTimeDiff = abs(difftime(timeAus, tideDateTimeAus, units = "hours")))

df.alltags <- df.alltags %>% 
  mutate(Band.ID = as.factor(Band.ID))

# If update = T, then combine past and new data and sort with time

if (update == TRUE) {
  
  df.alltags.past <- df.alltags.past %>% 
    mutate(Band.ID = as.factor(Band.ID))
  
  # Combine updated data to new
  df.alltags <- bind_rows(df.alltags, df.alltags.past)
}

# ==== More Data Cleaning? ====
df.alltags <- df.alltags %>% 
  filter(!is.na(speciesEN))

# Because I don't want to save multiple data.rds every time I render the .qmd, if past and new data are the same, run_analysis = F for next saving chunk. Per default = T.

if (update == TRUE) {
  # check whether df.alltags.past and df.alltags are same data
  run_analysis <- nrow(df.alltags) != nrow(df.alltags.past)
} else {
  run_analysis <- TRUE
}

# Bird detection dqtq
saveRDS(df.alltags, here::here("qmd", "chapter_1", "data", "motus", paste0(Sys.Date(), "-data", ".rds" )))

# Receiver information
saveRDS(df.recvDeps, here::here("qmd", "chapter_1", "data", "motus", paste0(Sys.Date(), "-recv-info", ".rds" )))

# Spreadsheet tracking BandID
saveRDS(spreadsheet, here::here("qmd", "chapter_1", "data", "spreadsheet", paste0(Sys.Date(), "-spreadsheet", ".rds" )))

# Tide tables
saveRDS(tideData, here("qmd", "chapter_1", "data", "tides", "tideData.rds"))

## ----ready-to-go, message = FALSE, warning = FALSE, eval = FALSE, echo = FALSE----
# 
# save.image(file = here::here("qmd", "chapter_1", "data", "motus_data.RData"))
# 

