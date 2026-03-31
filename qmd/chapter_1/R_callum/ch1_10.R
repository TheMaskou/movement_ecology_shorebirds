## ----style legend, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, include = FALSE, results = 'hide'----
# Source the script into that environment
source(knitr::purl(here::here("qmd", "chapter_1", "ch1_3.qmd"), 
                   output = tempfile(fileext = ".R"),    
                   quiet = TRUE))    


## ----packages,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----
library(motus)
library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(vegan)  
library(philentropy) 
library(gridExtra)
library(gt)
library(ggpubr)
library(car)
library(FSA)
library(patchwork)


## ----my data, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, results = 'hide', include = FALSE----
# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) 

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("qmd", "chapter_1", "data", "motus"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 


## ----shannon, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE------
# Function to calculate Shannon entropy
calculate_shannon <- function(proportions) {
  # Remove zero proportions (shouldn't be any, but safety check)
  p <- proportions[proportions > 0]
  # Calculate Shannon entropy: H = -sum(p * ln(p))
  H <- -sum(p * log(p))
  return(H)
}



## ----shannon 2, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----

# Calculate entropy metrics for each Band.ID
entropy_results <- data_all %>%
  # Remove any NA values in Band.ID or recvSiteName
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  
  # Count detections per bird per site
  group_by(speciesEN, Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop") %>%
  
  # Calculate proportions for each bird
  group_by(speciesEN, Band.ID) %>%
  mutate(
    total_detections = sum(detections),
    proportion = detections / total_detections) %>%
  
  # Calculate metrics for each bird
  summarise(
    S = n(),                                    # Number of sites used
    H = round(calculate_shannon(proportion), 2),# Shannon entropy
    J = round(H / log(S), 2),                   # Pielou's evenness
    total_detections = first(total_detections), # Total detections for reference
    .groups = "drop") %>%
  
  # Add effective number of equally-used sites
  mutate(
    exp_H = exp(H)) %>% 
  
  # Arrange by Band.ID
  arrange(speciesEN, Band.ID)



## ----shannon 3, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----
# Species-level summary from entropy_results
species_entropy_summary <- entropy_results %>%
  
  group_by(speciesEN) %>%
  summarise(
    
    n_individuals = n(),
    
    # S (Number of sites) statistics
    median_S = median(S, na.rm = TRUE),
    mean_S = round(mean(S, na.rm = TRUE), 2),
    sd_S = round(sd(S, na.rm = TRUE), 2),
    
    # H (Shannon entropy) statistics
    median_H = median(H, na.rm = TRUE),
    mean_H = round(mean(H, na.rm = TRUE), 2),
    sd_H = round(sd(H, na.rm = TRUE), 2),
    CV_H = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),  # Coefficient of variation
    
    # J (Pielou's evenness) statistics
    median_J = median(J, na.rm = TRUE),
    mean_J = round(mean(J, na.rm = TRUE), 2),
    sd_J = round(sd(J, na.rm = TRUE), 2),
    
    # exp_H (Effective number of sites) statistics
    median_exp_H = round(median(exp_H, na.rm = TRUE), 2),
    mean_exp_H = round(mean(exp_H, na.rm = TRUE), 2),
    sd_exp_H = round(sd(exp_H, na.rm = TRUE), 2),
    
    # Total detections across all individuals
    total_detections = sum(total_detections, na.rm = TRUE),
    
    .groups = "drop") %>%
  arrange(speciesEN)


# Result table
species_entropy_summary %>%
  
  gt() %>%
  
  tab_header(
    title = md("**Table.** Shannon entropy metrics for site use patterns of shorebird species in the Hunter estuary.")) %>%
  opt_align_table_header(align = "left") %>%
  
  tab_footnote(
    footnote = md("**Legend.** Grouped by species, this table summarises site fidelity metrics using Shannon entropy analysis. **n_individuals**: number of tagged individuals; **S**: number of unique sites used; **H**: Shannon entropy (dispersion of site use, higher = more generalised); **J**: Pielou's evenness (0-1, higher = more even site use); **exp(H)**: effective number of equally-used sites; **CV_H**: coefficient of variation in H (% individual variation within species); **total_detections**: total number of detections across all individuals. Mean and standard deviation values are provided (x̄ ± SD). Median values represent the central tendency across individuals within each species."))  %>%
  opt_table_font(font = "Times New Roman") %>%
  
  cols_label(
    speciesEN = "Species (En.)",
    n_individuals = "N individuals",
    median_S = "Median S",
    mean_S = "Mean S",
    sd_S = "SD S",
    median_H = "Median H",
    mean_H = "Mean H",
    sd_H = "SD H",
    CV_H = "CV H (%)",
    median_J = "Median J",
    mean_J = "Mean J",
    sd_J = "SD J",
    median_exp_H = "Median exp(H)",
    mean_exp_H = "Mean exp(H)",
    sd_exp_H = "SD exp(H)",
    total_detections = "Total detections" ) %>%
  
  tab_spanner(
    label = "Sample",
    columns = c(n_individuals, total_detections)) %>%
  tab_spanner(
    label = "Number of Sites (S)",
    columns = c(median_S, mean_S, sd_S) ) %>%
  tab_spanner(
    label = "Shannon Entropy (H)",
    columns = c(median_H, mean_H, sd_H, CV_H) ) %>%
  tab_spanner(
    label = "Pielou's Evenness (J)",
    columns = c(median_J, mean_J, sd_J) ) %>%
  tab_spanner(
    label = "Effective Sites [exp(H)]",
    columns = c(median_exp_H, mean_exp_H, sd_exp_H) ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body(columns = everything()) ) %>%
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_column_labels(columns = everything()) ) %>%
  
  fmt_number(
    columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H),
    decimals = 2) %>%
  fmt_number(
    columns = c(median_S, median_H, median_J, median_exp_H),
    decimals = 2) %>%
  fmt_number(
    columns = CV_H,
    decimals = 1) %>%
  fmt_number(
    columns = total_detections,
    decimals = 0,
    use_seps = TRUE ) %>%
  
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    data_row.padding = px(3),  
    table.width = pct(100))



## ----shannon 4, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE, results = 'hide'----
# Prepare data for plotting
entropy_for_plot <- entropy_results %>%
  select(speciesEN, Band.ID, S, H, J, exp_H) %>%
  pivot_longer(
    cols = c(S, H, J, exp_H),
    names_to = "metric",
    values_to = "value" ) %>%
  mutate( metric = factor(metric, 
                   levels = c("S", "H", "J", "exp_H"),
                   labels = c("S (Number of Sites)", 
                             "H (Shannon Entropy)", 
                             "J (Pielou's Evenness)", 
                             "exp(H) (Effective Sites)")))

# Create the boxplot
ggplot(entropy_for_plot, aes(x = speciesEN, y = value, fill = speciesEN)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  labs(
    title = "Shannon Entropy Metrics by Species",
    subtitle = "Distribution of site use patterns across individuals",
    x = "Species",
    y = "Value",
    fill = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none",  
    panel.grid.major.x = element_blank())



## ----shannon 5, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE, results = 'hide', include = FALSE----

# S table
gt_S <- entropy_results %>%
  select(speciesEN, Band.ID, S) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**S - Number of Sites Used**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", S = "S") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

# H table
gt_H <- entropy_results %>%
  select(speciesEN, Band.ID, H) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**H - Shannon Entropy**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", H = "H") %>%
  fmt_number(columns = H, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

# J table
gt_J <- entropy_results %>%
  select(speciesEN, Band.ID, J) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**J - Pielou's Evenness**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", J = "J") %>%
  fmt_number(columns = J, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

# exp_H table
gt_exp_H <- entropy_results %>%
  select(speciesEN, Band.ID, exp_H) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**exp(H) - Effective Number of Sites**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", exp_H = "exp(H)") %>%
  fmt_number(columns = exp_H, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())



## ----shannon 61, echo = FALSE, message = FALSE, warning = FALSE---------------
gt_S


## ----shannon 62, echo = FALSE, message = FALSE, warning = FALSE---------------
gt_H


## ----shannon 63, echo = FALSE, message = FALSE, warning = FALSE---------------
gt_J


## ----shannon 64, echo = FALSE, message = FALSE, warning = FALSE---------------
gt_exp_H


## ----shannon 7, message = FALSE,  warning = FALSE, echo = TRUE, eval = FALSE----
# 
# # Run Kruskal-Wallis for each species separately
# kruskal_within_species <- entropy_results %>%
#   group_by(speciesEN) %>%
#   filter(n() >= 3) %>% # minimum 3 individuals required
#   summarise(
#     n_individuals = n(),
# 
#     # H (Shannon Entropy)
#     Chi_sq_H = kruskal.test(H ~ Band.ID)$statistic,
#     df_H = kruskal.test(H ~ Band.ID)$parameter,
#     p_value_H = kruskal.test(H ~ Band.ID)$p.value,
# 
#     # J (Pielou's Evenness)
#     Chi_sq_J = kruskal.test(J ~ Band.ID)$statistic,
#     df_J = kruskal.test(J ~ Band.ID)$parameter,
#     p_value_J = kruskal.test(J ~ Band.ID)$p.value,
# 
#     # S (Number of Sites)
#     Chi_sq_S = kruskal.test(S ~ Band.ID)$statistic,
#     df_S = kruskal.test(S ~ Band.ID)$parameter,
#     p_value_S = kruskal.test(S ~ Band.ID)$p.value,
# 
#     .groups = "drop")


## ----shannon 71, message = FALSE,  warning = FALSE, echo = FALSE, eval = TRUE----

# Run Kruskal-Wallis for each species separately
kruskal_within_species <- entropy_results %>%
  group_by(speciesEN) %>%
  filter(n() >= 3) %>% # minimum 3 individuals required
  summarise(
    n_individuals = n(),
    
    # H (Shannon Entropy)
    Chi_sq_H = kruskal.test(H ~ Band.ID)$statistic,
    df_H = kruskal.test(H ~ Band.ID)$parameter,
    p_value_H = kruskal.test(H ~ Band.ID)$p.value,
    
    # J (Pielou's Evenness)
    Chi_sq_J = kruskal.test(J ~ Band.ID)$statistic,
    df_J = kruskal.test(J ~ Band.ID)$parameter,
    p_value_J = kruskal.test(J ~ Band.ID)$p.value,
    
    # S (Number of Sites)
    Chi_sq_S = kruskal.test(S ~ Band.ID)$statistic,
    df_S = kruskal.test(S ~ Band.ID)$parameter,
    p_value_S = kruskal.test(S ~ Band.ID)$p.value,
    
    .groups = "drop"
  ) %>%
  mutate(
    Sig_H = case_when(
      p_value_H < 0.001 ~ "***",
      p_value_H < 0.01 ~ "**",
      p_value_H < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    Sig_J = case_when(
      p_value_J < 0.001 ~ "***",
      p_value_J < 0.01 ~ "**",
      p_value_J < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    Sig_S = case_when(
      p_value_S < 0.001 ~ "***",
      p_value_S < 0.01 ~ "**",
      p_value_S < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

# Create the gt table
kruskal_within_species %>%
  
  gt() %>%
  
  tab_header(
    title = md("**Within Species: Kruskal-Wallis Test Results**"),
    subtitle = "Comparing entropy metrics across individuals within each species") %>%
  
  cols_label(
    speciesEN = "Species",
    n_individuals = "N",
    Chi_sq_H = "χ²",
    df_H = "df",
    p_value_H = "p-value",
    Sig_H = "Sig.",
    Chi_sq_J = "χ²",
    df_J = "df",
    p_value_J = "p-value",
    Sig_J = "Sig.",
    Chi_sq_S = "χ²",
    df_S = "df",
    p_value_S = "p-value",
    Sig_S = "Sig.") %>%
  
  tab_spanner(
    label = "H (Shannon Entropy)",
    columns = c(Chi_sq_H, df_H, p_value_H, Sig_H)) %>%
  tab_spanner(
    label = "J (Pielou's Evenness)",
    columns = c(Chi_sq_J, df_J, p_value_J, Sig_J)) %>%
  tab_spanner(
    label = "S (Number of Sites)",
    columns = c(Chi_sq_S, df_S, p_value_S, Sig_S)) %>%
  
  fmt_number(
    columns = c(Chi_sq_H, Chi_sq_J, Chi_sq_S),
    decimals = 3) %>%
  
  fmt_scientific(
    columns = c(p_value_H, p_value_J, p_value_S),
    decimals = 3) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(Sig_H, Sig_J, Sig_S),
      rows = p_value_H < 0.05 | p_value_J < 0.05 | p_value_S < 0.05 )) %>%
  
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = c(Chi_sq_H, df_H, p_value_H, Sig_H),
      rows = p_value_H < 0.05 )) %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(
      columns = c(Chi_sq_J, df_J, p_value_J, Sig_J),
      rows = p_value_J < 0.05 ) ) %>%
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(
      columns = c(Chi_sq_S, df_S, p_value_S, Sig_S),
      rows = p_value_S < 0.05 ) ) %>%
  
  tab_footnote(
    footnote = md("**Significance codes:** *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant (p ≥ 0.05). **N** = number of individuals. Green/blue/yellow highlighting indicates significant differences among individuals within that species. Only species with ≥3 individuals are included (minimum requirement for Kruskal-Wallis test).") ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  
  opt_table_font(font = "Times New Roman") %>%
  
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    data_row.padding = px(4),
    table.width = pct(100))



## ----shannon tide 1, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE, results = 'hide'----

# Calculate entropy metrics for each Band.ID
entropy_results_tide <- data_all %>%
  # Remove any NA values in Band.ID or recvSiteName
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  
  # Count detections per bird per site
  group_by(speciesEN, Band.ID, recvDeployName, tideHighLow) %>%
  summarise(detections = n(), .groups = "drop") %>%
  
  # Calculate proportions for each bird
  group_by(speciesEN, Band.ID, tideHighLow) %>%
  mutate(
    total_detections = sum(detections),
    proportion = detections / total_detections) %>%
  
  # Calculate metrics for each bird
  summarise(
    S = n(),                                              # Number of sites used
    H = round(calculate_shannon(proportion), 2),          # Shannon entropy
    J = round(H / log(S), 2),                             # Pielou's evenness
    total_detections = first(total_detections),           # Total detections for reference
    .groups = "drop") %>%
  
  # Add effective number equally-used sites
  mutate(exp_H = exp(H)) %>%

  # Arrange by Band.ID
  arrange(speciesEN, Band.ID, tideHighLow)



## ----shannon tide 2, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

# Species-level summary from entropy_results
species_entropy_summary_tide <- entropy_results_tide %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(
    n_individuals = n(),
    
    # S (Number of sites) statistics
    median_S = median(S, na.rm = TRUE),
    mean_S = round(mean(S, na.rm = TRUE), 2),
    sd_S = round(sd(S, na.rm = TRUE), 2),
    
    # H (Shannon entropy) statistics
    median_H = median(H, na.rm = TRUE),
    mean_H = round(mean(H, na.rm = TRUE), 2),
    sd_H = round(sd(H, na.rm = TRUE), 2),
    CV_H = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),  # Coefficient of variation
    
    # J (Pielou's evenness) statistics
    median_J = median(J, na.rm = TRUE),
    mean_J = round(mean(J, na.rm = TRUE), 2),
    sd_J = round(sd(J, na.rm = TRUE), 2),
    
    # exp_H (Effective number of sites) statistics
    median_exp_H = round(median(exp_H, na.rm = TRUE), 2),
    mean_exp_H = round(mean(exp_H, na.rm = TRUE), 2),
    sd_exp_H = round(sd(exp_H, na.rm = TRUE), 2),
    
    # Total detections across all individuals
    total_detections = sum(total_detections, na.rm = TRUE),
    
    .groups = "drop") %>%
  
  arrange(speciesEN)

# Pivot data to prepare plotting
entropy_for_plot_tide <- entropy_results_tide %>%
  select(speciesEN, tideHighLow, Band.ID, S, H, J, exp_H) %>%
  pivot_longer(
    cols = c(S, H, J, exp_H),
    names_to = "metric",
    values_to = "value" ) %>%
  mutate( 
    metric = factor(metric, 
                   levels = c("S", "H", "J", "exp_H"),
                   labels = c("S (Number of Sites)", 
                             "H (Shannon Entropy)", 
                             "J (Pielou's Evenness)", 
                             "exp(H) (Effective Sites)")) )


# Plotting
ggplot(entropy_for_plot_tide, aes(x = speciesEN, y = value, fill = speciesEN, alpha = tideHighLow)) +
  geom_boxplot(aes(group = interaction(speciesEN, tideHighLow)), 
               position = position_dodge(width = 0.8), 
               outlier.shape = 16) +
  geom_point(aes(group = interaction(speciesEN, tideHighLow)),
           position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
           size = 1.1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  scale_alpha_manual( values = c("High" = 1.0, "Low" = 0.2)) +
  
  # Add significance brackets
  stat_compare_means(
    aes(group = tideHighLow),
    method = "t.test",
    label = "p.signif",
    hide.ns = TRUE,
    size = 3,
    bracket.size = 0.3) +
  labs(
    title = "Shannon Entropy Metrics by Species and Tide",
    subtitle = "Distribution of site use patterns across individuals and tide (High tide: solid, Low tide: transparent)",
    x = "Species",
    y = "Value",
    fill = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank())


## ----shannon tide 3, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----

# Result table with tideHighLow grouping
species_entropy_summary_tide %>%
  
  gt() %>%
  
  tab_header(
    title = md("**Table.** Shannon entropy metrics for site use patterns of shorebird species by tide state in the Hunter estuary.")) %>%
  opt_align_table_header(align = "left") %>%
  
  tab_footnote(
    footnote = md("**Legend.** Grouped by species and tide state (Low/High), this table summarises site fidelity metrics using Shannon entropy analysis. **tideHighLow**: tide state during detections (Low or High tide); **n_individuals**: number of tagged individuals; **S**: number of unique sites used; **H**: Shannon entropy (dispersion of site use, higher = more generalised); **J**: Pielou's evenness (0-1, higher = more even site use); **exp(H)**: effective number of equally-used sites; **CV_H**: coefficient of variation in H (% individual variation within species); **total_detections**: total number of detections across all individuals. Mean and standard deviation values are provided (x̄ ± SD). Median values represent the central tendency across individuals within each species."))  %>%
  opt_table_font(font = "Times New Roman") %>%
  
  cols_label(
    speciesEN = "Species",
    tideHighLow = "Tide",
    n_individuals = "N indiv.",
    median_S = "Median",
    mean_S = "Mean",
    sd_S = "SD",
    median_H = "Median",
    mean_H = "Mean",
    sd_H = "SD",
    CV_H = "CV (%)",
    median_J = "Median",
    mean_J = "Mean",
    sd_J = "SD",
    median_exp_H = "Median",
    mean_exp_H = "Mean",
    sd_exp_H = "SD",
    total_detections = "Total det." ) %>%
  
  tab_spanner(
    label = "Sample",
    columns = c(n_individuals, total_detections)) %>%
  tab_spanner(
    label = "Number of Sites (S)",
    columns = c(median_S, mean_S, sd_S) ) %>%
  tab_spanner(
    label = "Shannon Entropy (H)",
    columns = c(median_H, mean_H, sd_H, CV_H) ) %>%
  tab_spanner(
    label = "Pielou's Evenness (J)",
    columns = c(median_J, mean_J, sd_J) ) %>%
  tab_spanner(
    label = "Effective Sites [exp(H)]",
    columns = c(median_exp_H, mean_exp_H, sd_exp_H) ) %>%
  
  # Row grouping by species
  tab_row_group(
    label = "Bar-tailed Godwit",
    rows = speciesEN == "Bar-tailed Godwit" ) %>%
  tab_row_group(
    label = "Curlew Sandpiper",
    rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(
    label = "Eurasian Whimbrel",
    rows = speciesEN == "Eurasian Whimbrel") %>%
  tab_row_group(
    label = "Far Eastern Curlew",
    rows = speciesEN == "Far Eastern Curlew") %>%
  tab_row_group(
    label = "Masked Lapwing",
    rows = speciesEN == "Masked Lapwing") %>%
  tab_row_group(
    label = "Pacific Golden-Plover",
    rows = speciesEN == "Pacific Golden-Plover") %>%
  tab_row_group(
    label = "Pied Stilt",
    rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(
    label = "Red-necked Avocet",
    rows = speciesEN == "Red-necked Avocet") %>%
  
  # Hide the speciesEN column since it's now in row groups
  cols_hide(columns = speciesEN) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups() ) %>%
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body(columns = everything()) ) %>%
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_column_labels(columns = everything()) ) %>%
  
  # Alternate row colors by tide within each species
  tab_style(
    style = cell_fill(color = "#F0F8FF"),
    locations = cells_body(rows = tideHighLow == "Low")) %>%
  tab_style(
    style = cell_fill(color = "#FFF8DC"),
    locations = cells_body(rows = tideHighLow == "High")) %>%
  
  fmt_number(
    columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H),
    decimals = 2) %>%
  fmt_number(
    columns = c(median_S, median_H, median_J, median_exp_H),
    decimals = 2) %>%
  fmt_number(
    columns = CV_H,
    decimals = 1) %>%
  fmt_number(
    columns = total_detections,
    decimals = 0,
    use_seps = TRUE ) %>%
  
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    data_row.padding = px(3),  
    table.width = pct(100),
    row_group.font.weight = "bold")


## ----shannon tide 4, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----
# Filter species with enough individuals
entropy_aov_data_tide <- entropy_for_plot_tide %>%
  group_by(speciesEN) %>%
  filter(n_distinct(Band.ID) >= 5) %>%
  mutate(
    Band.ID     = droplevels(as.factor(Band.ID)),
    tideHighLow = droplevels(as.factor(tideHighLow))) %>%
  ungroup()



## ----shannon tide 5, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----

results      <- data.frame()
assumptions  <- data.frame()
species_list_entropy <- unique(entropy_aov_data_tide$speciesEN)
metrics_list_entropy <- unique(entropy_aov_data_tide$metric)


for (sp in species_list_entropy) {
  for (m in metrics_list_entropy) {
    
    df <- subset(entropy_aov_data_tide, speciesEN == sp & metric == m)
    df <- na.omit(df)
    df$Band.ID     <- droplevels(df$Band.ID)       # drop unused factor levels
    df$tideHighLow <- droplevels(df$tideHighLow)
    
    model <- aov(value ~ Band.ID + tideHighLow, data = df) # ANOVA test
    s     <- summary(model)[[1]]
    
    # --- ANOVA results (tideHighLow row only) ---
    for (term in c("Band.ID", "tideHighLow")) {
      if (term %in% rownames(s)) {
        results <- rbind(results, data.frame(
          Species = sp,
          Metric  = as.character(m),
          Term    = ifelse(term == "tideHighLow", "Tide effect", term),
          Df      = s[term, "Df"],
          SS      = round(s[term, "Sum Sq"],  3),
          MS      = round(s[term, "Mean Sq"], 3),
          F_value = round(s[term, "F value"], 3),
          p_value = round(s[term, "Pr(>F)"],  4),
          stringsAsFactors = FALSE))
      }
    }
    
    # Independence
    indep_note <- "By design"
    
    # Normality of residuals (Shapiro-Wilk) 
    res <- residuals(model)
    sw  <- shapiro.test(res)
    
    # Homogeneity of variance (Levene's test) 
    lev <- tryCatch(
      leveneTest(value ~ tideHighLow, data = df, center = median),
      error = function(e) NULL)
    
    assumptions <- rbind(assumptions, data.frame(
      Species          = sp,
      Metric           = as.character(m),
      N                = nrow(df),
      Independence     = indep_note,
      SW_W             = round(sw$statistic, 4),
      SW_p             = round(sw$p.value,   4),
      Normality        = ifelse(sw$p.value > 0.049, "✓ Met", "✗ Violated"),
      Levene_F         = ifelse(!is.null(lev), round(lev$`F value`[1], 4), NA),
      Levene_p         = ifelse(!is.null(lev), round(lev$`Pr(>F)`[1],  4), NA),
      Homogeneity      = ifelse(!is.null(lev),
                           ifelse(lev$`Pr(>F)`[1] > 0.049, "✓ Met", "✗ Violated"),
                           "Insufficient levels"),
      stringsAsFactors = FALSE))
  }
}


# Prepare results sign.
results <- results %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""))


## ----shannon tide 6, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----

# Display assumption test plot
assumptions %>%
  gt(groupname_col = "Species") %>%
  
  tab_header(
    title    = md("**ANOVA Assumption Tests**"),
    subtitle = md("*Normality (Shapiro-Wilk) and Homogeneity of Variance (Levene's test)*") ) %>%
  
  cols_label(
    Metric       = "Metric",
    N            = "n",
    Independence = "Independence",
    SW_W         = "W",
    SW_p         = "p value",
    Normality    = "Normality",
    Levene_F     = "F",
    Levene_p     = "p value",
    Homogeneity  = "Homogeneity") %>%
  
  tab_spanner(
    label   = "Shapiro-Wilk",
    columns = c(SW_W, SW_p, Normality)) %>%
  
  tab_spanner(
    label   = "Levene's Test",
    columns = c(Levene_F, Levene_p, Homogeneity)) %>%
  
  # Green = met
  tab_style(
    style     = cell_text(color = "#27ae60", weight = "bold"),
    locations = cells_body(
      columns = c(Normality, Homogeneity),
      rows    = Normality == "✓ Met")) %>%
  tab_style(
    style     = cell_text(color = "#27ae60", weight = "bold"),
    locations = cells_body(
      columns = Homogeneity,
      rows    = Homogeneity == "✓ Met")) %>%
  
  # Red = violated
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      columns = Normality,
      rows    = Normality == "✗ Violated")) %>%
  
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      columns = Homogeneity,
      rows    = Homogeneity == "✗ Violated")) %>%
  
  # Red p-value when Shapiro violated
  tab_style(
    style     = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = SW_p,
      rows    = SW_p < 0.049)) %>%
  
  opt_row_striping() %>%
  
  tab_source_note(
    source_note = md(
      "**Independence**: verified by design — each Band.ID is a unique individual measured once per tide condition per metric.  
       **Shapiro-Wilk**: H₀ = residuals are normally distributed; p > 0.05 → assumption met.  
       **Levene's test**: H₀ = variances are equal across tide groups; p > 0.05 → assumption met.")) %>%
  
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13)



## ----shannon tide 7, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----
# Display plot
results %>%
  gt(groupname_col = "Species", rowname_col = "Term") %>%
  
  tab_header(
    title    = md("**Two-Way ANOVA Results**"),
    subtitle = md("*value ~ Band.ID + tideHighLow — by Species and Metric*")) %>%
  
  cols_label(
    Metric  = "Metric",
    Df      = "df",
    SS      = "Sum Sq",
    MS      = "Mean Sq",
    F_value = "F value",
    p_value = "p value",
    sig     = "") %>%
  
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(p_value, sig),
      rows    = sig %in% c("*", "**", "***", ".") )) %>%
  
  opt_row_striping() %>%
  
  tab_source_note(
    source_note = "Significance codes: *** p<0.001  ** p<0.01  * p<0.05  . p<0.1") %>%
  
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13 )


## ----jensen-shannon, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----
# Function to calculate Jensen-Shannon Divergence
calculate_jsd <- function(p, q) {
  # Add small value to avoid log(0)
  epsilon <- 1e-10
  p <- p + epsilon
  q <- q + epsilon
  
  # Normalise
  p <- p / sum(p)
  q <- q / sum(q)
  
  # Calculate JSD
  m <- (p + q) / 2
  jsd <- 0.5 * sum(p * log2(p / m)) + 0.5 * sum(q * log2(q / m))
  
  return(jsd)
}


## ----jensen-shannon 2, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Create site use matrix (rows = individuals, columns = sites)
site_use_matrix <- data_all %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  group_by(Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Band.ID) %>%
  mutate(proportion = round(detections / sum(detections), 2)) %>%
  select(Band.ID, recvDeployName, proportion) %>%
  pivot_wider(
    names_from = recvDeployName,
    values_from = proportion,
    values_fill = 0)

# Calculate pairwise JSD
individual_ids <- site_use_matrix$Band.ID
n_individuals <- length(individual_ids)

jsd_results <- data.frame()

for (i in 1:(n_individuals - 1)) {
  for (j in (i + 1):n_individuals) {
    
    p <- as.numeric(site_use_matrix[i, -1])
    q <- as.numeric(site_use_matrix[j, -1])
    
    jsd_results <- rbind(jsd_results, data.frame(
      Band.ID_1 = individual_ids[i],
      Band.ID_2 = individual_ids[j],
      JSD = round(calculate_jsd(p, q), 3), # using formula built beforehands
      Similarity = round(1 - calculate_jsd(p, q), 3)))
  }
}

# Add species information
jsd_results <- jsd_results %>%
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), 
            by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), 
            by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  mutate(same_species = species_1 == species_2)



## ----jensen-shannon 3, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Filter to same_species comparisons and prepare for boxplot (matching entropy_for_plot structure)
jsd_for_plot <- jsd_results %>%
  
  filter(same_species == TRUE) %>%
  
  mutate(
    metric = "JSD",
    speciesEN = species_1) %>% 
  
  select(speciesEN, JSD, metric) %>%
  rename(value = JSD)

# Boxplot exactly matching your entropy format
ggplot(jsd_for_plot, aes(x = speciesEN, y = value, fill = speciesEN)) +
  
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  
  scale_fill_manual(values = species_colors) +
  
  labs(
    title = "Jensen-Shannon Divergence by Species",
    subtitle = "Individual pairwise similarity within species",
    x = "Species",
    y = "JSD Value",
    fill = "Species") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none",  
    panel.grid.major.x = element_blank())




## ----jensen-shannon 4, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----

# Preparing table
jsd_results_tab <- jsd_results %>%
  filter(same_species == TRUE) %>%
  group_by(species_1) %>%
  summarise(
    n_comparisons = n(),
    mean_JSD = round(mean(JSD), 3),
    sd_JSD = round(sd(JSD), 3),
    mean_Similarity = round(mean(Similarity), 3),
    sd_Similarity = round(sd(Similarity), 3),
    .groups = "drop") %>%
  rename(speciesEN = species_1)


# Adding extra useful metrics
n_indiv <- data_all %>%
  group_by(speciesEN) %>%
  summarise(n_indiv = n_distinct(Band.ID), .groups = "drop")

# Finalise table
jsd_results_tab <- jsd_results_tab %>%
  left_join(n_indiv, by = "speciesEN") %>%
  select(speciesEN, n_indiv, everything()) %>%
  arrange(mean_JSD)



## ----jensen-shannon 5, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

jsd_results_tab %>%
  gt() %>%
  
  tab_header(
    title    = md("**Jensen-Shannon Divergence by Species**"),
    subtitle = md("**Individual pairwise similarity within species**")) %>%
   tab_source_note(
    source_note = md(
      "**JSD ≈ 0** → high similarity in site composition, using same sites in same proportions.  
       **JSD ≈ 1** → low similarity in site composition, using different sets of sites or proportions. ")) %>%
  
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13)   %>%
  
  opt_row_striping()



## ----jensen-shannon 6, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----
kruskal_value <- kruskal.test(JSD ~ species_1, 
                            data = jsd_results %>% filter(same_species == TRUE))

kruskal_summary <- data.frame(
  Metric = c("Value"),
  Chi_squared = round(kruskal_value$statistic, 3),
  df = kruskal_value$parameter,
  p_value = kruskal_value$p.value) %>%
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"))

kruskal_summary  %>%
  
  gt() %>%
  
  tab_header(
    title = md("**Kruskal-Wallis Test Results**"),
    subtitle = "Comparing value across groups") %>%
  
  cols_label(
    Metric = "Metric",
    Chi_squared = "χ2",
    df = "df",
    p_value = "p-value",
    Significance = "Sig.") %>% 
  
  fmt_number(
    columns = Chi_squared,
    decimals = 3) %>%
  
  fmt_scientific(
    columns = p_value,
    decimals = 3) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Significance,
      rows = p_value < 0.05)) %>%
  
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = p_value < 0.05)) %>%
  
  tab_footnote(
    footnote = md("**Significance codes:** *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant (p ≥ 0.05). Green highlighting indicates significant differences.")) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  
  opt_table_font(font = "Times New Roman") %>%
  
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    data_row.padding = px(5),
    table.width = pct(100)
  )




## ----jensen-shannon 7, message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE, results = 'hide'----

# If significant, do post-hoc pairwise comparisons
if(kruskal_value$p.value < 0.05) {
  dunn_jsd <- dunnTest(JSD ~ species_1, 
                       data = jsd_results %>% filter(same_species == TRUE),
                       method = "bonferroni")
  print(dunn_jsd)
}



## ----jensen-shannon 8, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

dunn_jsd_table <- dunn_jsd$res %>%
  mutate(
    Significance = case_when(
      P.adj < 0.001 ~ "***",
      P.adj < 0.01 ~ "**",
      P.adj < 0.05 ~ "*",
      TRUE ~ "ns" )) %>%
  select(Comparison, Z, P.unadj, P.adj, Significance)  # Adjust columns as needed

dunn_jsd_table %>% 
  
  arrange(Comparison) %>%
  
  gt() %>%
  
  tab_header(
    title = md("**Bonferroni Correction Post-Hoc Test (Dunn)**"),
    subtitle = "Pairwise JSD comparisons across species") %>%
  
  cols_label(
    Comparison = "Comparison",
    Z = "Z",
    P.unadj = "p unadj",
    P.adj = "p adj",
    Significance = "Sig.") %>%
  
  fmt_scientific(
    columns = c(P.unadj, P.adj),
    decimals = 3) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Significance,
      rows = P.adj < 0.05)) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = P.adj < 0.05)) %>%
  
  tab_footnote(
    footnote = md("**Significance codes:** *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not significant. Green = significant. **p unadj** and **p adj** the raw and Bonferroni corrected p-values.")) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  
  opt_table_font(font = "Times New Roman") %>%
  
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    data_row.padding = px(5),
    table.width = pct(100))



## ----jensen-shannon tide 9, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

# Create site use matrix (rows = individuals, columns = sites)
site_use_matrix_tide <- data_all %>%
  
  # Sanity check
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
 
  # Filter out speciesn with less than 5 indiv
  group_by(speciesEN) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  
  # Actual JSD computation per tides
  group_by(Band.ID, recvDeployName, tideHighLow) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Band.ID, tideHighLow) %>%
  mutate(proportion = round(detections / sum(detections), 2)) %>%
  select(Band.ID, recvDeployName, tideHighLow, proportion) %>%
  pivot_wider(
    names_from = recvDeployName,
    values_from = proportion,
    values_fill = 0) %>%
  unite("id_tide", Band.ID, tideHighLow, remove = FALSE)  # Unique ID for each individual*tide combo

# Calculate pairwise JSD
individual_tide_ids <- site_use_matrix_tide$id_tide
n_individuals_tide <- length(individual_tide_ids)

jsd_results_tide <- data.frame()

for (i in 1:(n_individuals_tide - 1)) {
  for (j in (i + 1):n_individuals_tide) {
    
    # Only compare birds with same tideHighLow
    if (site_use_matrix_tide$tideHighLow[i] == site_use_matrix_tide$tideHighLow[j]) {
      p <- as.numeric(site_use_matrix_tide[i, -c(1:3)])  # Exclude ID columns
      q <- as.numeric(site_use_matrix_tide[j, -c(1:3)])
      
      jsd_results_tide <- rbind(jsd_results_tide, data.frame(
        Band.ID_1 = site_use_matrix_tide$Band.ID[i],
        Band.ID_2 = site_use_matrix_tide$Band.ID[j],
        tideHighLow = site_use_matrix_tide$tideHighLow[i],
        JSD = round(calculate_jsd(p, q), 3),
        Similarity = round(1 - calculate_jsd(p, q), 3)))
    }
  }
}

# Add species information
jsd_results_tide <- jsd_results_tide %>%
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), 
            by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), 
            by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  mutate(same_species = species_1 == species_2) %>%
  filter(same_species == TRUE)  # Only conspecific comparisons



## ----jensen-shannon tide 10, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE----

jsd_results_tab_tide <- jsd_results_tide %>%
  filter(same_species == TRUE) %>%
  group_by(species_1, tideHighLow) %>%
  summarise(
    n_comparisons   = n(),
    mean_JSD        = round(mean(JSD),        3),
    median_JSD      = round(median(JSD),      3),
    sd_JSD          = round(sd(JSD),          3),
    mean_Similarity = round(mean(Similarity), 3),
    median_Similarity = round(median(Similarity), 3),
    sd_Similarity   = round(sd(Similarity),   3),
    .groups = "drop") %>%
  rename(speciesEN = species_1)

n_indiv <- data_all %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_indiv = n_distinct(Band.ID), 
            .groups = "drop")
n_detect <- data_all %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_detect = n(), 
            .groups = "drop")

# Finalise the result table
jsd_results_tab_tide <- jsd_results_tab_tide %>%
  left_join(n_indiv, by = c("speciesEN", "tideHighLow")) %>%
  left_join(n_detect , by = c("speciesEN", "tideHighLow")) %>%
  select(speciesEN, tideHighLow, n_indiv, n_detect, n_comparisons, mean_JSD, median_JSD, sd_JSD, 
         mean_Similarity, median_Similarity, sd_Similarity) %>%
  arrange(tideHighLow, mean_JSD)


# Display the JSD result table
jsd_results_tab_tide %>%
  gt() %>%
  tab_header(
    title = md("**Table.** Jensen-Shannon Divergence (JSD) for intra-species habitat similarity by tide state in the Hunter estuary."),
    subtitle = md("**JSD ≈ 0** → high similarity in site composition, using same sites in same proportions.  
                   **JSD ≈ 1** → low similarity in site composition, using different sets of sites or proportions. ") ) %>%

  tab_footnote(
    footnote = md("**Legend.** **n_indiv**: tagged individuals per species×tide combination; **n_detect**: total number of detections per species×tide combination; **n_comparisons**: pairwise comparisons; **mean_JSD ± sd_JSD**: average dissimilarity in site used; **mean_Similarity ± sd_Similarity**: 1/JSD (average site used similarity). **Low tide** rows blue, **High tide** rows yellow.")) %>%
  opt_table_font(font = "Times New Roman") %>%
  
  cols_label(
    speciesEN = "Species",
    tideHighLow = "Tide", 
    n_indiv = "N indiv.",
    n_detect = "N detect.",
    n_comparisons = "N comp.",
    mean_JSD = "Mean",
    median_JSD = "Median",
    sd_JSD = "SD",
    mean_Similarity = "Mean",
    median_Similarity = "Median",
    sd_Similarity = "SD") %>%
  
  tab_spanner(
    label = "Sample Size",
    columns = c(n_indiv, n_detect, n_comparisons) ) %>%
  tab_spanner(
    label = "JSD (Dissimilarity)",
    columns = c(mean_JSD, median_JSD,  sd_JSD) ) %>%
  tab_spanner(
    label = "Similarity (1/JSD)",
    columns = c(mean_Similarity, median_Similarity, sd_Similarity) ) %>%
  
  # Row grouping by species (update with your actual species names)
  tab_row_group(
    label = "Bar-tailed Godwit",
    rows = speciesEN == "Bar-tailed Godwit" ) %>%
  tab_row_group(
    label = "Pied Stilt", 
    rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(
    label = "Pacific Golden-Plover",
    rows = speciesEN == "Pacific Golden-Plover" ) %>%
  tab_row_group(
    label = "Red-necked Avocet",
    rows = speciesEN == "Red-necked Avocet" ) %>%
  tab_row_group(
    label = "Curlew Sandpiper",
    rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(
    label = "Eurasian Whimbrel",
    rows = speciesEN == "Eurasian Whimbrel" ) %>%
  
  cols_hide(columns = speciesEN) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) %>%
  tab_style(
    style = cell_text(whitespace = "nowrap"),
    locations = cells_body()) %>%
  
  # Tide-specific row colors
  tab_style(
    style = cell_fill(color = "#F0F8FF"),  # Light blue for Low
    locations = cells_body(rows = tideHighLow == "Low") ) %>%
  tab_style(
    style = cell_fill(color = "#FFF8DC"),  # Light yellow for High  
    locations = cells_body(rows = tideHighLow == "High")) %>%
  
  fmt_number(
    columns = c(mean_JSD, median_JSD, sd_JSD, mean_Similarity, median_Similarity, sd_Similarity),
    decimals = 3) %>%
  fmt_number(
    columns = c(n_indiv, n_detect, n_comparisons),
    decimals = 0,
    use_seps = TRUE ) %>%
  
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    data_row.padding = px(3),
    table.width = pct(100),
    row_group.font.weight = "bold")


## ----jensen-shannon tide 11, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----
# Pivot data to prepare plotting
jsd_for_plot_tide <- jsd_results_tide %>%
  pivot_longer(
    cols      = c(JSD, Similarity),
    names_to  = "metric",
    values_to = "value")

species_list_jsd <- unique(jsd_for_plot_tide$species_1)


## ----jensen-shannon tide 12, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----

jsd_for_plot_tide %>%
  filter(metric == "JSD") %>%
  ggplot(aes(x     = species_1,
             y     = value,
             fill  = species_1,
             alpha = tideHighLow,
             group = interaction(species_1, tideHighLow))) +
  
  geom_boxplot(position      = position_dodge(width = 0.8),
               outlier.shape = 16,
               outlier.size  = 1.5) +
  
  geom_point(position = position_jitterdodge(jitter.width = 0.15,
                                             dodge.width  = 0.8),
             size = 1.1) +
  
  scale_fill_manual(values = species_colors) +
  scale_alpha_manual(values = c("High" = 1.0, "Low" = 0.35)) +
  
  labs(
    title    = "Within-species Jensen-Shannon Divergence by Tide",
    subtitle = "Pairwise JSD between individuals — High tide (solid) vs Low tide (transparent)",
    x        = NULL,
    y        = "Jensen-Shannon Divergence") +
  
  theme_minimal() +
  theme(
    legend.position    = "none",
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(size = 9, color = "grey40"))



## ----jensen-shannon tide 13, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----
results_jsd_tide<- data.frame()
assumptions_jsd <- data.frame()

for (sp in species_list_jsd) {
  
  df <- subset(jsd_for_plot_tide, species_1 == sp & metric == "JSD")
  df <- na.omit(df)
  df$Band.ID_1   <- droplevels(df$Band.ID_1)
  df$Band.ID_2   <- droplevels(df$Band.ID_2)
  df$tideHighLow <- droplevels(df$tideHighLow)
  
  # Check levels before fitting
  n_tide <- nlevels(df$tideHighLow)
  n_id   <- nlevels(df$Band.ID_1)
  
  # Skip species with insufficient variation to fit any model
  if (n_id < 2) {
    message("Skipping ", sp, ": fewer than 2 individuals")
    next
  }
  
  # Build formula depending on whether tide has 2 levels
  model_formula <- if (n_tide >= 2) {
    value ~ Band.ID_1 + tideHighLow
  } else {
    message("Note: ", sp, " has only 1 tide level — fitting without tideHighLow")
    value ~ Band.ID_1
  }
  
  model <- aov(model_formula, data = df)
  s     <- summary(model)[[1]]
  
  # --- ANOVA results ---
  # Only loop over terms that were actually in the model
  terms_to_check <- intersect(c("Band.ID_1", "tideHighLow"), rownames(s))
  
  for (term in terms_to_check) {
    results_jsd_tide <- rbind(results_jsd_tide, data.frame(
      Species = sp,
      Metric  = "JSD",
      Term    = ifelse(term == "tideHighLow", "Tide effect",
                ifelse(term == "Band.ID_1",   "Individual", term)),
      Df      = s[term, "Df"],
      SS      = round(s[term, "Sum Sq"],  4),
      MS      = round(s[term, "Mean Sq"], 4),
      F_value = round(s[term, "F value"], 3),
      p_value = round(s[term, "Pr(>F)"],  4),
      stringsAsFactors = FALSE
    ))
  }
  
  # --- Normality (Shapiro-Wilk on residuals) ---
  sw <- shapiro.test(residuals(model))
  
  # --- Homogeneity of variance (Levene's test — only if tide has >= 2 levels) ---
  lev <- if (n_tide >= 2) {
    tryCatch(
      leveneTest(value ~ tideHighLow, data = df, center = median),
      error = function(e) NULL
    )
  } else {
    NULL
  }
  
  assumptions_jsd <- rbind(assumptions_jsd, data.frame(
    Species      = sp,
    Metric       = "JSD",
    N            = nrow(df),
    Independence = "By design",
    SW_W         = round(sw$statistic,  4),
    SW_p         = round(sw$p.value,    4),
    Normality    = ifelse(sw$p.value > 0.05, "✓ Met", "✗ Violated"),
    Levene_F     = ifelse(!is.null(lev), round(lev[1, "F value"], 4), NA),
    Levene_p     = ifelse(!is.null(lev), round(lev[1, "Pr(>F)"],  4), NA),
    Homogeneity  = ifelse(!is.null(lev),
                     ifelse(lev[1, "Pr(>F)"] > 0.05, "✓ Met", "✗ Violated"),
                     "Insufficient levels"),
    stringsAsFactors = FALSE
  ))
}


## ----jensen-shannon tide 14, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----
results_jsd_tide <- results_jsd_tide %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""))

assumptions_jsd %>%
  gt(groupname_col = "Species") %>%

  tab_header(
    title    = md("**ANOVA Assumption Tests — JSD**"),
    subtitle = md("*Normality (Shapiro-Wilk) and Homogeneity of Variance (Levene's test)*")
  ) %>%

  cols_label(
    Metric       = "Metric",
    N            = "n",
    Independence = "Independence",
    SW_W         = "W",
    SW_p         = "p value",
    Normality    = "Normality",
    Levene_F     = "F",
    Levene_p     = "p value",
    Homogeneity  = "Homogeneity"
  ) %>%

  tab_spanner(
    label   = "Shapiro-Wilk",
    columns = c(SW_W, SW_p, Normality)
  ) %>%

  tab_spanner(
    label   = "Levene's Test",
    columns = c(Levene_F, Levene_p, Homogeneity)
  ) %>%

  tab_style(
    style     = cell_text(color = "#27ae60", weight = "bold"),
    locations = cells_body(
      columns = Normality,
      rows    = Normality == "✓ Met"
    )
  ) %>%
  tab_style(
    style     = cell_text(color = "#27ae60", weight = "bold"),
    locations = cells_body(
      columns = Homogeneity,
      rows    = Homogeneity == "✓ Met"
    )
  ) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      columns = Normality,
      rows    = Normality == "✗ Violated"
    )
  ) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      columns = Homogeneity,
      rows    = Homogeneity == "✗ Violated"
    )
  ) %>%
  tab_style(
    style     = cell_text(color = "#c0392b"),
    locations = cells_body(
      columns = SW_p,
      rows    = SW_p < 0.05
    )
  ) %>%

  opt_row_striping() %>%

  tab_source_note(
    source_note = md(
      "**Independence**: verified by design — each Band.ID is a unique individual measured once per tide condition.  
       **Shapiro-Wilk**: H₀ = residuals are normally distributed; p > 0.05 → assumption met.  
       **Levene's test**: H₀ = variances are equal across tide groups; p > 0.05 → assumption met."
    )
  ) %>%

  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13
  )


## ----jensen-shannon tide 15, message = FALSE, warning = FALSE, eval = TRUE,  echo = FALSE----

# Build model_list first
model_list <- list()

for (sp in species_list_jsd) {
  
  df <- subset(jsd_for_plot_tide, species_1 == sp & metric == "JSD")
  df <- na.omit(df)
  df$Band.ID_1   <- droplevels(df$Band.ID_1)
  df$tideHighLow <- droplevels(df$tideHighLow)
  
  n_tide <- nlevels(df$tideHighLow)
  n_id   <- nlevels(df$Band.ID_1)
  
  if (n_id < 2) {
    message("Skipping ", sp, ": fewer than 2 individuals")
    next
  }
  
  model_formula <- if (n_tide >= 2) {
    value ~ Band.ID_1 + tideHighLow
  } else {
    message("Note: ", sp, " has only 1 tide level — fitting without tideHighLow")
    value ~ Band.ID_1
  }
  
  model_list[[sp]] <- list(
    model  = aov(model_formula, data = df),
    data   = df,
    n_tide = n_tide)
}

# Build plot_list from model_list
plot_list <- list()

for (sp in names(model_list)) {
  
  model  <- model_list[[sp]]$model
  df     <- model_list[[sp]]$data
  n_tide <- model_list[[sp]]$n_tide
  
  res      <- residuals(model)
  fitted_v <- fitted(model)
  
  diag_df <- data.frame(
    fitted   = fitted_v,
    res      = res,
    sqrt_res = sqrt(abs(res)),
    tide     = df$tideHighLow,
    id       = df$Band.ID_1)
  
  # --- Plot 1: Residuals vs Fitted ---
  p1 <- ggplot(diag_df, aes(x = fitted, y = res)) +
    geom_point(color = "#2980b9", size = 1.5, alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
    geom_smooth(method = "loess", se = FALSE, color = "#e67e22", linewidth = 0.8) +
    labs(title = "Residuals vs Fitted",
         x     = "Fitted values",
         y     = "Residuals") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 10))
  
  # --- Plot 2: Q-Q plot ---
  p2 <- ggplot(diag_df, aes(sample = res)) +
    stat_qq(color = "#2980b9", size = 1.5, alpha = 0.7) +
    stat_qq_line(color = "#c0392b", linewidth = 0.8) +
    labs(title = "Normal Q-Q",
         x     = "Theoretical quantiles",
         y     = "Sample quantiles") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 10))
  
  # --- Plot 3: Scale-Location ---
  p3 <- ggplot(diag_df, aes(x = fitted, y = sqrt_res)) +
    geom_point(color = "#2980b9", size = 1.5, alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE, color = "#e67e22", linewidth = 0.8) +
    labs(title = "Scale-Location",
         x     = "Fitted values",
         y     = expression(sqrt("|Residuals|"))) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 10))
  
  # --- Plot 4: Residuals by tide or by individual ---
  if (n_tide >= 2) {
    p4 <- ggplot(diag_df, aes(x = tide, y = res, fill = tide)) +
      geom_boxplot(alpha = 0.7, outlier.shape = 16) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
      scale_fill_manual(values = c("High" = "#aed6f1", "Low" = "#a9dfbf")) +
      labs(title = "Residuals by Tide",
           x     = "Tide",
           y     = "Residuals") +
      theme_minimal() +
      theme(plot.title      = element_text(face = "bold", size = 10),
            legend.position = "none")
  } else {
    p4 <- ggplot(diag_df, aes(x = id, y = res)) +
      geom_boxplot(fill = "#aed6f1", alpha = 0.7, outlier.shape = 16) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
      labs(title = "Residuals by Individual\n(1 tide level only)",
           x     = "Individual",
           y     = "Residuals") +
      theme_minimal() +
      theme(plot.title  = element_text(face = "bold", size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
  }
  
  # Combine with patchwork and store
  plot_list[[sp]] <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = paste0("Diagnostic Plots — ", sp),
      theme = theme(plot.title = element_text(face  = "bold",
                                              size  = 13,
                                              color = "#2c3e50")))
}



## ----assumptions tabs, message = FALSE, warning = FALSE, results = 'asis', echo = FALSE, eval = TRUE ,fig.width = 10, fig.height = 6----
# Create tabs dynamically
for (sp in names(plot_list)) {
  cat("###", sp, "\n\n")
  print(plot_list[[sp]])
  cat("\n\n")
}


## ----jensen-shannon tide 16, message = FALSE, warning = FALSE, eval = TRUE,  echo = TRUE----
results_jsd_tide <- results_jsd_tide %>%
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""
    )
  )

# ── ANOVA table ──────────────────────────────────────────────────────────────

results_jsd_tide %>%
  gt(groupname_col = "Species", rowname_col = "Term") %>%

  tab_header(
    title    = md("**Two-Way ANOVA Results — JSD**"),
    subtitle = md("*value ~ Band.ID + tideHighLow — by Species*")
  ) %>%

  cols_label(
    Metric  = "Metric",
    Df      = "df",
    SS      = "Sum Sq",
    MS      = "Mean Sq",
    F_value = "F value",
    p_value = "p value",
    sig     = ""
  ) %>%

  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      columns = vars(p_value, sig),
      rows    = sig %in% c("*", "**", "***", ".")
    )
  ) %>%

  opt_row_striping() %>%

  tab_source_note(
    source_note = "Significance codes: *** p<0.001  ** p<0.01  * p<0.05  . p<0.1"
  ) %>%

  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13
  )

