## ----style legend, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = FALSE, results = 'hide'----
# Source the script into that environment
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_3.qmd"), 
                   output = tempfile(fileext = ".R"),   
                   quiet = TRUE))       


## ----packages,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----
library(dplyr)
library(here) 
library(ggplot2)
library(lubridate)
library(tidyr)


## ----my data, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE, results = 'hide', include = FALSE----
# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) 


## ----process,  message = FALSE, warning = FALSE, echo = TRUE, eval = TRUE-----

# Get deployment duration for each tags (in days)
tag_dep_duration <- data_all %>%
  group_by(Band.ID) %>%
  summarise(start_date = min(DateAUS.Trap),
            end_date = max(dateAus),
            period_tag_dep_d = 1 + time_length(interval(start = start_date, end = end_date), unit = "day"), # +1 to include the trapping day
            .groups = "drop") %>%
  select(Band.ID, period_tag_dep_d)

# Get number days each tag has been detected at least once in any station
tag_dep_nb_d <- data_all %>%
  mutate(dateAus = as.Date(dateAus)) %>%
  group_by(Band.ID) %>%
  summarise(days_detect = n_distinct(dateAus), .groups = "drop")

# Get number of days one tag has been recorded in >1 station (for each tags)
multiple_site_detect <- data_all %>%
  group_by(Band.ID, dateAus) %>%
  # Counts per day the number of station one tag has been recorded
  summarise(distinct_sites = n_distinct(recvDeployName),
            .groups = "drop_last") %>%
  # Tells if one day one tag has been recorded several sites
  mutate(multiple_detect = distinct_sites > 1) %>%
  group_by(Band.ID) %>%
  summarise(multiple_site_detect = sum(multiple_detect),
            .groups = "drop")



## ----indiv table, echo = TRUE, eval = TRUE------------------------------------
# Get number of day each tag has been recorded /species and /station
tag_detection_indiv <- data_all %>%
  group_by(Band.ID, recvDeployName, speciesEN) %>%
  # to count sp*ID per recv
  reframe(days_recorded = n_distinct(dateAus)) %>%
  # flip table to get one column per station
  pivot_wider(names_from = recvDeployName, 
              values_from = days_recorded,
              values_fill = 0) %>% # = 0 if NA
  left_join(multiple_site_detect, by = "Band.ID") %>%
  left_join(tag_dep_nb_d, by = "Band.ID") %>%
  left_join(tag_dep_duration, by = "Band.ID") %>%
  mutate(perc = round((days_detect / period_tag_dep_d) * 100, 1)) # nb of day one tag is recorded once / nb of day the tag is deployed



## ----indiv table pub,  message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----
# library(gt)
# tag_detection_indiv  %>%
#   gt() %>%
#   opt_align_table_header(align = "left")  %>%
#   tab_style(
#     style = cell_text(weight = "bold"),
#     locations = cells_column_labels() ) %>%
#   tab_style(
#     style = cell_text(style = "italic"),
#     locations = cells_body(columns = c(speciesEN))) 

DT::datatable(
  tag_detection_indiv,
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    fixedColumns = list(leftColumns = 3)
  ),
  extensions = c('FixedColumns'),
  caption = 'Details for individuals',
  class = 'nowrap'  
) %>%
  DT::formatStyle(
    c('Band.ID','speciesEN'),
    fontWeight = 'bold'
  ) %>%
  DT::formatStyle(
    c('speciesEN'),
    fontWeight = 'italic',
    color = '#808080'  
  )



## ----3 plot indiv, echo = FALSE, eval = TRUE----------------------------------
ggplot(tag_detection_indiv %>%
         group_by(speciesEN) %>%
         arrange(desc(perc), .by_group = TRUE) %>%
         mutate(Band.ID = factor(Band.ID, levels = unique(Band.ID))), 
       aes(x = Band.ID, y = perc, fill = speciesEN)) +
  geom_col(width = 0.8, alpha = 0.7, color = "black") +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(x = "Band ID",
       y = "Detection (%)",
       title = "Bird detection across MOTUS array (% of days)")


## ----species table,  message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

# Get deployment duration for each tags (in days)
tag_dep_duration <- data_all %>%
  group_by(speciesEN) %>%
  summarise(start_date = min(DateAUS.Trap),
            end_date = max(dateAus),
            period_tag_dep_d = 1 + time_length(interval(start = start_date, end = end_date), unit = "day"), # +1 to include the trapping day
            .groups = "drop") %>%
  select(speciesEN, period_tag_dep_d)

# Get number days each tag has been detected at least once in any station
tag_dep_nb_d <- data_all %>%
  mutate(dateAus = as.Date(dateAus)) %>%
  group_by(speciesEN) %>%
  summarise(days_detect = n_distinct(dateAus), .groups = "drop")

# Get number of days one tag has been recorded in >1 station (for each tags)
multiple_site_detect <- data_all %>%
  group_by(speciesEN, dateAus) %>%
  # Counts per day the number of station one tag has been recorded
  summarise(distinct_sites = n_distinct(recvDeployName),
            .groups = "drop_last") %>%
  # Tells if one day one tag has been recorded several sites
  mutate(multiple_detect = distinct_sites > 1) %>%
  group_by(speciesEN) %>%
  summarise(multiple_site_detect = sum(multiple_detect),
            .groups = "drop")

tag_detection <- data_all %>%
  group_by(recvDeployName, speciesEN) %>%
  # to count sp*ID per recv
  reframe(days_recorded = n_distinct(dateAus)) %>%
  # flip table to get one column per station
  pivot_wider(names_from = recvDeployName, 
              values_from = days_recorded,
              values_fill = 0) %>% # = 0 if NA
  left_join(multiple_site_detect, by = "speciesEN") %>%
  left_join(tag_dep_nb_d, by = "speciesEN") %>%
  left_join(tag_dep_duration, by = "speciesEN") %>%
  mutate(perc = round((days_detect / period_tag_dep_d) * 100, 1))

library(gt)
tag_detection  %>%
  gt() %>%
  opt_align_table_header(align = "left")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(speciesEN))) 



## ----3 species histo, echo = FALSE, eval = TRUE-------------------------------
ggplot(tag_detection %>% 
         mutate(speciesEN = forcats::fct_reorder(speciesEN, perc, .desc = TRUE)),      
       aes(x = speciesEN, 
           y = perc, 
           fill = speciesEN)) +  # fill mapped to speciesEN
  geom_col(width = 0.8, alpha = 0.7, color = "black") +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Species",
       y = "Detection (%)",
       title = "Species detectability across the MOTUS array (Histogram)")



## ----3 species plot boxplot, echo = TRUE, eval = TRUE-------------------------
ggplot(tag_detection_indiv %>%
         add_count(speciesEN) %>%
         mutate(species_label = paste0(speciesEN, " (n = ", n, ")")),
       aes(x = reorder(species_label, -perc, FUN = median),
           y = perc, fill = speciesEN)) +   
  geom_boxplot(width = 0.8, alpha = 0.7, color = "black") +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Species (n = number of individuals)",
       y = "Detection (%)",
       title = "Species detectability across the MOTUS array (Boxplot)")



## ----3 species plot value table, echo = TRUE, eval = TRUE---------------------
detection_table <- tag_detection_indiv %>%
  add_count(speciesEN) %>%
  mutate(Species = speciesEN) %>%
  group_by(Species, speciesEN) %>%
  summarise(
    n_individuals = n(),
    min = round(min(perc, na.rm = TRUE), 1),
    q1 = round(quantile(perc, 0.25, na.rm = TRUE),1),
    median = round(median(perc, na.rm = TRUE),1),
    q3 = round(quantile(perc, 0.75, na.rm = TRUE),1),
    max = round(max(perc, na.rm = TRUE),1),
    mean = round(mean(perc, na.rm = TRUE),1),
    sd = round(sd(perc, na.rm = TRUE),1),
    .groups = 'drop'
  ) %>%
  arrange(desc(median)) %>%
  select(-speciesEN)

library(gt)
detection_table  %>%
  gt() %>%
  opt_align_table_header(align = "left")  %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(Species))) 


