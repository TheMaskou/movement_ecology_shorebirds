# ==== Estuary Usage (Days) ====
#
# Quantifies how many days each bird (and species) was detected at least once
# on at least one Motus station, expressed as a percentage of the total days
# the tag was deployed and active.
#
# This addresses the question: do individuals remain within the same estuary,
# or move between estuaries? A high detection percentage indicates consistent
# presence; a low one may indicate absence, movement outside the array, or
# tag/receiver issues.
#
# Key metric:
#   perc = (days_detect / period_tag_dep_d) * 100
#
# Outputs: individual-level and species-level summary tables and plots.
#
# Requires: globals.R (constants, paths), detection data .rds from ch1_1.

source(here::here("R_callum", "globals.R"))

library(dplyr)
library(here)
library(ggplot2)
library(lubridate)
library(tidyr)


# ==== Load Data ====

data_all <- readRDS(path_detection_data)

# ==== Individual-level Detection Metrics ====

# Deployment duration: number of days each tag was active, from trapping day
# to last detection. The +1 includes the trapping day itself.
tag_dep_duration <- data_all %>%
  group_by(Band.ID) %>%
  summarise(start_date = min(DateAUS.Trap),
            end_date = max(dateAus),
            period_tag_dep_d = 1 + time_length(interval(start = start_date, end = end_date), unit = "day"), # +1 to include the trapping day
            .groups = "drop") %>%
  select(Band.ID, period_tag_dep_d)

# Number of distinct days each tag was detected anywhere in the array
tag_dep_nb_d <- data_all %>%
  mutate(dateAus = as.Date(dateAus)) %>%
  group_by(Band.ID) %>%
  summarise(days_detect = n_distinct(dateAus), .groups = "drop")

# Number of days each tag was detected at >1 station (multi-site days).
# Indicates movement between stations within a single day.
multiple_site_detect <- data_all %>%
  group_by(Band.ID, dateAus) %>%
  summarise(distinct_sites = n_distinct(recvDeployName),
            .groups = "drop_last") %>%
  mutate(multiple_detect = distinct_sites > 1) %>%
  group_by(Band.ID) %>%
  summarise(multiple_site_detect = sum(multiple_detect),
            .groups = "drop")


## ---- Individual Summary Table ----

# Combine per-individual station counts with deployment and detection metrics.
# Each station becomes its own column showing days recorded there.
# perc = proportion of deployed days on which the bird was detected at least once.
tag_detection_indiv <- data_all %>%
  group_by(Band.ID, recvDeployName, speciesEN) %>%
  reframe(days_recorded = n_distinct(dateAus)) %>%
  pivot_wider(names_from = recvDeployName,
              values_from = days_recorded,
              values_fill = 0) %>%
  left_join(multiple_site_detect, by = "Band.ID") %>%
  left_join(tag_dep_nb_d, by = "Band.ID") %>%
  left_join(tag_dep_duration, by = "Band.ID") %>%
  mutate(perc = round((days_detect / period_tag_dep_d) * 100, 1))


## ---- Individual Interactive Table ----

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


## ---- Individual Detection Bar Chart ----

# Bar chart of detection % per individual, ordered within species groups.
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


# ==== Species-level Detection Metrics ====
#
# Same approach as individual-level above, but grouped by species instead of
# Band.ID. The intermediate variables (tag_dep_duration, tag_dep_nb_d,
# multiple_site_detect) are intentionally overwritten with species-grouped
# versions.

# Deployment duration per species (first trapping to last detection across all
# individuals of that species)
tag_dep_duration <- data_all %>%
  group_by(speciesEN) %>%
  summarise(start_date = min(DateAUS.Trap),
            end_date = max(dateAus),
            period_tag_dep_d = 1 + time_length(interval(start = start_date, end = end_date), unit = "day"), # +1 to include the trapping day
            .groups = "drop") %>%
  select(speciesEN, period_tag_dep_d)

# Distinct detection days per species
tag_dep_nb_d <- data_all %>%
  mutate(dateAus = as.Date(dateAus)) %>%
  group_by(speciesEN) %>%
  summarise(days_detect = n_distinct(dateAus), .groups = "drop")

# Multi-station days per species
multiple_site_detect <- data_all %>%
  group_by(speciesEN, dateAus) %>%
  summarise(distinct_sites = n_distinct(recvDeployName),
            .groups = "drop_last") %>%
  mutate(multiple_detect = distinct_sites > 1) %>%
  group_by(speciesEN) %>%
  summarise(multiple_site_detect = sum(multiple_detect),
            .groups = "drop")

## ---- Species Summary Table ----

tag_detection <- data_all %>%
  group_by(recvDeployName, speciesEN) %>%
  reframe(days_recorded = n_distinct(dateAus)) %>%
  pivot_wider(names_from = recvDeployName,
              values_from = days_recorded,
              values_fill = 0) %>%
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


## ---- Species Detection Histogram ----

ggplot(tag_detection %>%
         mutate(speciesEN = forcats::fct_reorder(speciesEN, perc, .desc = TRUE)),
       aes(x = speciesEN,
           y = perc,
           fill = speciesEN)) +
  geom_col(width = 0.8, alpha = 0.7, color = "black") +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Species",
       y = "Detection (%)",
       title = "Species detectability across the MOTUS array (Histogram)")


## ---- Species Detection Boxplot ----

# Boxplot using individual-level data to show within-species variation.
# Labels include sample size (n = number of individuals per species).
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


## ---- Species Detection Summary Statistics ----

# Descriptive statistics of detection % across individuals within each species.
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
