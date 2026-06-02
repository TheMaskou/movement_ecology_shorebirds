# Quick check for if species grouping is redundant
grouped_species <- df.alltags %>%
  # Remove any NA values in Band.ID or recvSiteName
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  
  # Count detections per bird per site
  group_by(speciesEN, Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop")

grouped_no_species <- df.alltags %>%
  # Remove any NA values in Band.ID or recvSiteName
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  
  # Count detections per bird per site
  group_by(Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop")

all.equal(grouped_no_species, grouped_species)

# Shannon Entropy - Remove Zero Proportions ====
# Function to calculate Shannon entropy
calculate_shannon <- function(proportions) {
  # Remove zero proportions (shouldn't be any, but safety check)
  p <- proportions[proportions > 0]
  # Calculate Shannon entropy: H = -sum(p * ln(p))
  H <- -sum(p * log(p))
  return(H)
}

# NOTE: I am concerned about the proportions > 0 - zero detections of a bird
# at a specific receiver is a zero result, not something to be filtered out

# ==== Shannon Entropy — Per Individual ====
#
# For each bird, count detections at each station, convert to proportions,
# then compute:
#   S     = number of unique stations used
#   H     = Shannon entropy of the station-use distribution
#   J     = Pielou's evenness = H / ln(S)  (0 = all detections at one site,
#           1 = perfectly even use)
#   exp_H = effective number of equally-used sites

# Calculate entropy metrics for each Band.ID
entropy_results <- df.alltags %>%
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
  
  # NOTE: It is best practice to keep numeric values with full precision,
  # and then round them for display in a table (if rounded early, rounding error
  # will be propagated through further calculations)
  
  # Calculate metrics for each bird
  summarise(
    S = n(),                                    # Number of sites used
    H = calculate_shannon(proportion),# Shannon entropy
    J = H / log(S),                # Pielou's evenness
    total_detections = first(total_detections), # Total detections for reference
    .groups = "drop") %>%
  
  # Add effective number of equally-used sites
  mutate(
    exp_H = exp(H)) %>%
  
  # Arrange by Band.ID
  arrange(speciesEN, Band.ID)

# ==== Shannon Entropy — Species Summary Table ====

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

# Shannon Entropy - Include Zero Proportions ====
# Function to calculate Shannon entropy
calculate_shannon_with_zero_proportions <- function(proportions) {
  # Remove zero proportions (shouldn't be any, but safety check)
  p <- proportions
  # Calculate Shannon entropy: H = -sum(p * ln(p))
  H <- -sum(p * log(p))
  return(H)
}

# NOTE: I am concerned about the proportions > 0 - zero detections of a bird
# at a specific receiver is a zero result, not something to be filtered out

# ==== Shannon Entropy — Per Individual ====
#
# For each bird, count detections at each station, convert to proportions,
# then compute:
#   S     = number of unique stations used
#   H     = Shannon entropy of the station-use distribution
#   J     = Pielou's evenness = H / ln(S)  (0 = all detections at one site,
#           1 = perfectly even use)
#   exp_H = effective number of equally-used sites

# Calculate entropy metrics for each Band.ID
entropy_results_with_zero_proportions <- df.alltags %>%
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
  
  # NOTE: It is best practice to keep numeric values with full precision,
  # and then round them for display in a table (if rounded early, rounding error
  # will be propagated through further calculations)
  
  # Calculate metrics for each bird
  summarise(
    S = n(),                                    # Number of sites used
    H = calculate_shannon_with_zero_proportions(proportion),# Shannon entropy
    J = H / log(S),                # Pielou's evenness
    total_detections = first(total_detections), # Total detections for reference
    .groups = "drop") %>%
  
  # Add effective number of equally-used sites
  mutate(
    exp_H = exp(H)) %>%
  
  # Arrange by Band.ID
  arrange(speciesEN, Band.ID)

# ==== Shannon Entropy — Species Summary Table ====

# Species-level summary from entropy_results_with_zero_proportions
species_entropy_summary_with_zero_proportions <- entropy_results_with_zero_proportions %>%
  
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
species_entropy_summary_with_zero_proportions %>%
  
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


