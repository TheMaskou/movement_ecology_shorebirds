# ==== Jensen-Shannon Divergence — Site-Use Composition ====
#
# Pairwise comparison of site-use distributions between individuals of the
# same species, using Jensen-Shannon Divergence (JSD). Addresses:
# "Do conspecifics use the *same* sites?" JSD ~ 0 = identical site use,
# JSD ~ 1 = completely different site use.
#
# Computed globally and split by tide (High/Low) to inspect whether
# inter-individual site-use similarity changes with tidal condition.
#
# In addition to the raw pairwise boxplots, also reports a per-individual
# collapse (one mean value per bird) and per-species mean ± SD summaries —
# see "Per-Individual Collapse" and "Per-Species Summary" sections — which
# reduce (but do not eliminate) the pseudo-replication inherent in pairwise
# JSD values.
#
# Exploratory only — no statistical tests (see ch1_10_entropy_evenness_composition.R
# for the PERMANOVA / PERMDISP tests this script does not include).
#
# Requires: globals.R (constants, paths), ch1_1 detection data .rds.
#
# ==== Limitations ====
#
# 1. STATION UPTIME: this analysis assumes the receiver array was fully
#    operational for the entire study period. Receiver down-periods are ignored
#    in this version and represent a known source of bias — a bird detected at
#    few stations may simply reflect offline receivers, not specialised site
#    use. Document as a limitation in any writeup.
#
# 2. DETECTION-COUNT CURRENCY: proportions are derived from raw Motus hit
#    counts, which are heavily autocorrelated and confounded by residence-bout
#    length, tag burst rate, receiver sensitivity, antenna count, and distance.
#    A time/presence-based currency (e.g. detection-hours from
#    ch1_7_station_usage_hours.R, or distinct days/tide-cycles per station) is
#    more ecologically defensible. Recommended for a future iteration.
#
# 3. SAMPLING-EFFORT DEPENDENCE: site-use proportions are not rarefied and
#    scale with total detections per individual.

source(here::here("qmd", "chapter_1", "R", "globals.R"))


# ==== Packages ====

library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(gt)


# ==== Load Data ====

df.alltags <- readRDS(path_detection_data)


# ==== Date Range Filter ====
#
# Clip detections to a fixed date window for reproducibility (so the same
# script gives the same result regardless of which df.alltags build is
# loaded). Dates are "YYYY-MM-DD" strings; set either bound to NA for no
# limit on that side. Both NA = use all data (no filtering).

date_min <- NA   # e.g. "2023-01-01"
date_max <- NA   # e.g. "2024-12-31"

df.alltags <- df.alltags %>%
  filter(is.na(date_min) | dateAus >= as.Date(date_min),
         is.na(date_max) | dateAus <= as.Date(date_max))


# ==== Plot Settings ====
#
# Visual controls shared across the JSD boxplots below. Edit here to restyle
# without hunting through each ggplot block.

## ---- Settings: All Plots ----

outlier_shape   <- 16   # outlier symbol
axis_text_angle <- 45   # x-axis label rotation (degrees)
axis_text_size  <- 9    # x-axis label size

# Minimum tagged individuals (distinct Band.ID) per species for that species to
# appear in a plot. Species below the threshold are dropped from that plot only
# (summary tables are unaffected). Set to 1 for no filtering.
min_tags_main_plot <- 1   # non-tide boxplot (plot_jsd_box)
min_tags_tide_plot <- 1   # by-tide boxplot (plot_jsd_box_tide)

# Visibility of basic plot elements (TRUE = shown)
show_title    <- TRUE
show_subtitle <- TRUE

# Plot either JSD (dissimilarity) or Similarity (= 1 - JSD) on the y-axis.
# Affects the column plotted and all titles/axis labels below.
use_similarity <- TRUE

jsd_metric_col   <- if (use_similarity) "Similarity" else "JSD"
jsd_metric_label <- if (use_similarity) "Similarity (1 - JSD)" else "Jensen-Shannon Divergence (JSD)"

## ---- Settings: Non-tide Plot ----
# plot_jsd_box (geom_boxplot + geom_jitter)

box_alpha    <- 0.7   # box fill opacity
jitter_width <- 0.2   # horizontal spread of jittered points
jitter_alpha <- 0.3   # jittered point opacity
jitter_size  <- 1     # jittered point size

## ---- Settings: By-tide Plot ----
# plot_jsd_box_tide (dodged boxplot + dodged jittered points + Tide legend)

outlier_size       <- 1.5   # outlier point size
point_size         <- 1.1   # jittered point size
dodge_width        <- 0.8   # gap between High/Low boxes
tide_jitter_width  <- 0.15  # point spread within a box
tide_alpha         <- c(High = 1.0, Low = 0.35)  # opacity per tide state
tide_legend_colour <- "grey30"                   # Tide legend swatch colour
legend_position    <- "top"                      # Tide legend position

## ---- Settings: Summary Point-Range ----
# plot_jsd_summary_indiv / plot_jsd_summary_pairwise (geom_pointrange)

pr_point_size  <- 0.6    # centre dot size
pr_line_size   <- 0.8    # whisker (± SD) thickness
pr_clamp_01    <- TRUE   # clamp mean ± SD whiskers to [0, 1] for display


# ==== Min-Tags Filtering ====
#
# Distinct tagged individuals per species (used by the min-tags plot filters
# above). Species below the relevant threshold are dropped from that plot
# only — summary tables are unaffected.

species_tag_counts <- df.alltags %>%
  filter(!is.na(Band.ID)) %>%
  group_by(speciesEN) %>%
  summarise(n_tags = n_distinct(Band.ID), .groups = "drop")

species_keep_main <- species_tag_counts$speciesEN[species_tag_counts$n_tags >= min_tags_main_plot]
species_keep_tide <- species_tag_counts$speciesEN[species_tag_counts$n_tags >= min_tags_tide_plot]

# Suffix appended to a plot subtitle describing an active min-tags filter.
# Returns "" when min_tags == 1 (no filtering) so default output is unchanged.
filter_note <- function(min_tags) {
  if (min_tags > 1) paste0(" (species with ≥ ", min_tags, " tagged individuals)") else ""
}


# ==== JSD Function ====

calculate_jsd <- function(p, q) {
  epsilon <- 1e-10
  p <- p + epsilon
  q <- q + epsilon
  p <- p / sum(p)
  q <- q / sum(q)
  m   <- (p + q) / 2
  jsd <- 0.5 * sum(p * log2(p / m)) + 0.5 * sum(q * log2(q / m))
  return(jsd)
}


# ==== JSD — Pairwise Computation ====
#
# Build a site-use proportion matrix (individuals × stations) then compute
# JSD for every pair. Full-precision proportions — no rounding — to avoid
# zeroing small-use sites that can distort JSD.

site_use_matrix <- df.alltags %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  group_by(Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Band.ID) %>%
  mutate(proportion = detections / sum(detections)) %>%   # full precision
  select(Band.ID, recvDeployName, proportion) %>%
  pivot_wider(
    names_from  = recvDeployName,
    values_from = proportion,
    values_fill = 0)

individual_ids <- as.character(site_use_matrix$Band.ID)
prop_matrix    <- as.matrix(site_use_matrix[, -1])

# NOTE: This is equivalent to the loop in original code

# Every pairwise combination of individuals, computed once (no loop/rbind)
pair_idx  <- combn(nrow(prop_matrix), 2)
jsd_vals  <- apply(pair_idx, 2, function(ix)
  calculate_jsd(prop_matrix[ix[1], ], prop_matrix[ix[2], ]))

jsd_results <- data.frame(
  Band.ID_1  = individual_ids[pair_idx[1, ]],
  Band.ID_2  = individual_ids[pair_idx[2, ]],
  JSD        = round(jsd_vals, 3),
  Similarity = round(1 - jsd_vals, 3))

# Add species information
species_lookup <- df.alltags %>%
  mutate(Band.ID = as.character(Band.ID)) %>%
  select(Band.ID, speciesEN) %>%
  distinct()

jsd_results <- jsd_results %>%
  left_join(species_lookup, by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(species_lookup, by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  mutate(same_species = species_1 == species_2)


## ---- JSD Boxplot ----

jsd_for_plot <- jsd_results %>%
  filter(same_species) %>%
  mutate(speciesEN = species_1) %>%
  filter(speciesEN %in% species_keep_main)

plot_jsd_box <- ggplot(jsd_for_plot, aes(x = speciesEN, y = .data[[jsd_metric_col]], fill = speciesEN)) +
  geom_boxplot(alpha = box_alpha, outlier.shape = outlier_shape) +
  geom_jitter(width = jitter_width, alpha = jitter_alpha, size = jitter_size) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = if (show_title) paste(jsd_metric_label, "by Species") else NULL,
    subtitle = if (show_subtitle) paste0("Pairwise inter-individual site-use similarity within species", filter_note(min_tags_main_plot)) else NULL,
    x        = "Species",
    y        = jsd_metric_label,
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_jsd_box


## ---- JSD Summary Table ----

n_indiv_jsd <- df.alltags %>%
  group_by(speciesEN) %>%
  summarise(n_indiv = n_distinct(Band.ID), .groups = "drop")

jsd_results_tab <- jsd_results %>%
  filter(same_species) %>%
  group_by(species_1) %>%
  summarise(
    n_comparisons   = n(),
    mean_JSD        = round(mean(JSD),        3),
    sd_JSD          = round(sd(JSD),          3),
    mean_Similarity = round(mean(Similarity), 3),
    sd_Similarity   = round(sd(Similarity),   3),
    .groups = "drop") %>%
  rename(speciesEN = species_1) %>%
  left_join(n_indiv_jsd, by = "speciesEN") %>%
  select(speciesEN, n_indiv, everything()) %>%
  arrange(mean_JSD)

tbl_jsd_summary <- jsd_results_tab %>%
  gt() %>%
  tab_header(
    title    = md("**Jensen-Shannon Divergence by Species**"),
    subtitle = md("Pairwise inter-individual site-use similarity within species")) %>%
  tab_source_note(
    source_note = md(
      "**JSD ≈ 0** → high similarity in site composition (conspecifics use same sites).
       **JSD ≈ 1** → low similarity (conspecifics use different sites).")) %>%
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align               = "left",
    table.font.size             = 13) %>%
  opt_row_striping()

tbl_jsd_summary


# ==== JSD — Per-Individual Collapse (One Value per Bird) ====
#
# The pairwise boxplot above plots n(n-1)/2 JSD values per species, but these
# are not independent: each individual contributes to n-1 pairs, so one
# atypical bird can seed many correlated points, and the number of points
# scales quadratically with the number of tagged individuals (better-sampled
# species look "more variable" purely as a sampling artifact).
#
# This section collapses each individual to a single value — its mean JSD to
# all same-species conspecifics — giving exactly n_indiv points per species.
# This removes the quadratic-scaling artifact and greatly reduces
# pseudo-replication, while preserving the species-level mean (the mean of
# these per-individual means equals the overall mean pairwise JSD).
#
# Caveat: per-individual means are still derived from the same shared distance
# matrix (individual i's value and individual j's value both include
# JSD(i, j)), so this remains a descriptive view, not a basis for formal
# inference between individuals.

jsd_per_indiv <- jsd_results %>%
  filter(same_species) %>%
  { bind_rows(
      transmute(., Band.ID = Band.ID_1, speciesEN = species_1, JSD, Similarity),
      transmute(., Band.ID = Band.ID_2, speciesEN = species_2, JSD, Similarity)) } %>%
  group_by(Band.ID, speciesEN) %>%
  summarise(
    mean_JSD        = mean(JSD),
    mean_Similarity = mean(Similarity),
    n_partners      = n(),
    .groups = "drop")

per_indiv_metric_col <- if (use_similarity) "mean_Similarity" else "mean_JSD"


## ---- Per-Individual Boxplot ----

jsd_per_indiv_for_plot <- jsd_per_indiv %>%
  filter(speciesEN %in% species_keep_main)

plot_jsd_box_indiv <- ggplot(jsd_per_indiv_for_plot, aes(x = speciesEN, y = .data[[per_indiv_metric_col]], fill = speciesEN)) +
  geom_boxplot(alpha = box_alpha, outlier.shape = outlier_shape) +
  geom_jitter(width = jitter_width, alpha = jitter_alpha, size = jitter_size) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = if (show_title) paste(jsd_metric_label, "by Species — Per-Individual") else NULL,
    subtitle = if (show_subtitle) paste0("Each point is one individual's mean site-use ", tolower(jsd_metric_label), " to its conspecifics", filter_note(min_tags_main_plot)) else NULL,
    x        = "Species",
    y        = jsd_metric_label,
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_jsd_box_indiv


## ---- Per-Individual Summary Table ----

jsd_per_indiv_tab <- jsd_per_indiv %>%
  group_by(speciesEN) %>%
  summarise(
    n_indiv         = n(),
    # sd_* computed before mean_* of the same name (see note in
    # "Per-Species Summary" section for why ordering matters here).
    sd_JSD          = round(sd(mean_JSD),          3),
    sd_Similarity   = round(sd(mean_Similarity),   3),
    mean_JSD        = round(mean(mean_JSD),        3),
    mean_Similarity = round(mean(mean_Similarity), 3),
    .groups = "drop") %>%
  select(speciesEN, n_indiv, mean_JSD, sd_JSD, mean_Similarity, sd_Similarity) %>%
  arrange(mean_JSD)

tbl_jsd_per_indiv <- jsd_per_indiv_tab %>%
  gt() %>%
  tab_header(
    title    = md("**Jensen-Shannon Divergence by Species — Per-Individual**"),
    subtitle = md("Each individual collapsed to its mean JSD/Similarity to same-species conspecifics")) %>%
  tab_source_note(
    source_note = md(
      "**mean_JSD / sd_JSD**: mean and SD of the per-individual mean-JSD values
       (one value per tagged individual, n = n_indiv).
       **JSD ≈ 0** → high similarity in site composition (conspecifics use same sites).
       **JSD ≈ 1** → low similarity (conspecifics use different sites).")) %>%
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align               = "left",
    table.font.size             = 13) %>%
  opt_row_striping()

tbl_jsd_per_indiv


# ==== JSD — Per-Species Summary (Mean ± SD Point-Range) ====
#
# A minimal "mean ± SD per species" summary. Note this is a point-range plot,
# not a boxplot — a boxplot shows median/IQR, which would not match a
# mean ± SD caption. Two versions are shown:
#  - from the per-individual means (jsd_per_indiv): SD reflects genuine
#    between-individual variation (n_indiv independent-ish points per species).
#  - from the raw pairwise values (jsd_results_tab): matches the existing
#    summary table exactly, but the SD is understated because pairwise JSD
#    values are not independent (see Per-Individual Collapse section above).

make_pointrange <- function(df, mean_col, sd_col, subtitle) {
  ymin <- df[[mean_col]] - df[[sd_col]]
  ymax <- df[[mean_col]] + df[[sd_col]]
  if (pr_clamp_01) {
    ymin <- pmax(0, ymin)
    ymax <- pmin(1, ymax)
  }
  ggplot(df, aes(x = speciesEN, y = .data[[mean_col]], colour = speciesEN)) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax), size = pr_point_size, linewidth = pr_line_size) +
    scale_colour_manual(values = species_colors) +
    labs(
      title    = if (show_title) paste(jsd_metric_label, "by Species (Mean ± SD)") else NULL,
      subtitle = if (show_subtitle) subtitle else NULL,
      x        = "Species",
      y        = jsd_metric_label) +
    theme_minimal() +
    theme(
      axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
      legend.position    = "none",
      panel.grid.major.x = element_blank())
}

summary_mean_col <- if (use_similarity) "mean_Similarity" else "mean_JSD"
summary_sd_col   <- if (use_similarity) "sd_Similarity"   else "sd_JSD"


## ---- Summary: From Per-Individual Means ----

jsd_per_indiv_summary <- jsd_per_indiv %>%
  filter(speciesEN %in% species_keep_main) %>%
  group_by(speciesEN) %>%
  summarise(
    # sd_* computed before mean_* of the same name, since summarise() evaluates
    # sequentially and mean_JSD/mean_Similarity below would otherwise shadow
    # the per-individual columns with the (single-valued) group mean first.
    sd_JSD          = sd(mean_JSD),
    sd_Similarity   = sd(mean_Similarity),
    mean_JSD        = mean(mean_JSD),
    mean_Similarity = mean(mean_Similarity),
    .groups = "drop") %>%
  select(speciesEN, mean_JSD, sd_JSD, mean_Similarity, sd_Similarity)

plot_jsd_summary_indiv <- make_pointrange(
  jsd_per_indiv_summary, summary_mean_col, summary_sd_col,
  subtitle = if (show_subtitle) paste0("Mean ± SD across per-individual values", filter_note(min_tags_main_plot)) else NULL)

plot_jsd_summary_indiv


## ---- Summary: From Raw Pairwise Values ----

jsd_pairwise_summary <- jsd_results_tab %>%
  filter(speciesEN %in% species_keep_main)

plot_jsd_summary_pairwise <- make_pointrange(
  jsd_pairwise_summary, summary_mean_col, summary_sd_col,
  subtitle = if (show_subtitle) paste0("Mean ± SD across all pairwise values (SD understated — non-independent pairs)", filter_note(min_tags_main_plot)) else NULL)

plot_jsd_summary_pairwise


# ==== JSD — By Tide Pairwise Computation ====
#
# Computes within-tide pairwise JSD (High-High and Low-Low pairs only) to
# describe how similar conspecifics are within each tidal state.
# Species are filtered for the by-tide boxplot via `min_tags_tide_plot`
# (see Min-Tags Filtering above); this table itself is unfiltered.
# Full-precision proportions used (no rounding).

site_use_matrix_tide <- df.alltags %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%
  group_by(Band.ID, recvDeployName, tideHighLow) %>%
  summarise(detections = n(), .groups = "drop") %>%
  group_by(Band.ID, tideHighLow) %>%
  mutate(proportion = detections / sum(detections)) %>%   # full precision
  select(Band.ID, recvDeployName, tideHighLow, proportion) %>%
  pivot_wider(
    names_from  = recvDeployName,
    values_from = proportion,
    values_fill = 0) %>%
  unite("id_tide", Band.ID, tideHighLow, remove = FALSE)

tide_prop_cols    <- setdiff(names(site_use_matrix_tide), c("id_tide", "Band.ID", "tideHighLow"))
prop_matrix_tide  <- as.matrix(site_use_matrix_tide[, tide_prop_cols])

# Pairwise JSD within each tide state only (no cross-tide pairs)
jsd_results_tide <- site_use_matrix_tide$tideHighLow %>%
  unique() %>%
  lapply(function(tide) {
    idx <- which(site_use_matrix_tide$tideHighLow == tide)
    if (length(idx) < 2) return(NULL)

    pairs    <- combn(idx, 2)
    jsd_vals <- apply(pairs, 2, function(ix)
      calculate_jsd(prop_matrix_tide[ix[1], ], prop_matrix_tide[ix[2], ]))

    data.frame(
      Band.ID_1   = site_use_matrix_tide$Band.ID[pairs[1, ]],
      Band.ID_2   = site_use_matrix_tide$Band.ID[pairs[2, ]],
      tideHighLow = tide,
      JSD         = round(jsd_vals, 3),
      Similarity  = round(1 - jsd_vals, 3))
  }) %>%
  bind_rows()

jsd_results_tide <- jsd_results_tide %>%
  left_join(species_lookup %>% mutate(Band.ID = as.character(Band.ID)),
            by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(species_lookup %>% mutate(Band.ID = as.character(Band.ID)),
            by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  filter(species_1 == species_2)


## ---- JSD by Tide — Summary Table ----

n_indiv_tide  <- df.alltags %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_indiv  = n_distinct(Band.ID), .groups = "drop")
n_detect_tide <- df.alltags %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_detect = n(), .groups = "drop")

jsd_results_tab_tide <- jsd_results_tide %>%
  group_by(species_1, tideHighLow) %>%
  summarise(
    n_comparisons     = n(),
    mean_JSD          = round(mean(JSD),          3),
    median_JSD        = round(median(JSD),        3),
    sd_JSD            = round(sd(JSD),            3),
    mean_Similarity   = round(mean(Similarity),   3),
    median_Similarity = round(median(Similarity), 3),
    sd_Similarity     = round(sd(Similarity),     3),
    .groups = "drop") %>%
  rename(speciesEN = species_1) %>%
  left_join(n_indiv_tide,  by = c("speciesEN", "tideHighLow")) %>%
  left_join(n_detect_tide, by = c("speciesEN", "tideHighLow")) %>%
  select(speciesEN, tideHighLow, n_indiv, n_detect, n_comparisons,
         mean_JSD, median_JSD, sd_JSD,
         mean_Similarity, median_Similarity, sd_Similarity) %>%
  arrange(tideHighLow, mean_JSD)

tbl_jsd_summary_tide <- jsd_results_tab_tide %>%
  gt() %>%
  tab_header(
    title    = md("**Table.** Jensen-Shannon Divergence (JSD) for intra-species site-use similarity by tide state in the Hunter estuary."),
    subtitle = md("**JSD ≈ 0** → high similarity.  **JSD ≈ 1** → low similarity.")) %>%
  tab_footnote(
    footnote = md(
      "**n_indiv**: tagged individuals per species×tide combination; **n_detect**:
       total detections; **n_comparisons**: within-tide pairwise comparisons;
       **mean_JSD ± sd_JSD**: average inter-individual dissimilarity within same
       tide state. Low tide rows blue, High tide rows yellow.")) %>%
  opt_table_font(font = "Times New Roman") %>%
  cols_label(
    speciesEN         = "Species",
    tideHighLow       = "Tide",
    n_indiv           = "N indiv.",
    n_detect          = "N detect.",
    n_comparisons     = "N comp.",
    mean_JSD          = "Mean",
    median_JSD        = "Median",
    sd_JSD            = "SD",
    mean_Similarity   = "Mean",
    median_Similarity = "Median",
    sd_Similarity     = "SD") %>%
  tab_spanner(label = "Sample Size",         columns = c(n_indiv, n_detect, n_comparisons)) %>%
  tab_spanner(label = "JSD (Dissimilarity)", columns = c(mean_JSD, median_JSD, sd_JSD)) %>%
  tab_spanner(label = "Similarity (1-JSD)",  columns = c(mean_Similarity, median_Similarity, sd_Similarity)) %>%

  tab_row_group(label = "Bar-tailed Godwit",     rows = speciesEN == "Bar-tailed Godwit") %>%
  tab_row_group(label = "Pied Stilt",            rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(label = "Pacific Golden-Plover", rows = speciesEN == "Pacific Golden-Plover") %>%
  tab_row_group(label = "Red-necked Avocet",     rows = speciesEN == "Red-necked Avocet") %>%
  tab_row_group(label = "Curlew Sandpiper",      rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(label = "Eurasian Whimbrel",     rows = speciesEN == "Eurasian Whimbrel") %>%

  cols_hide(columns = speciesEN) %>%

  tab_style(style = cell_text(weight = "bold"),      locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"),      locations = cells_row_groups()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"), locations = cells_body()) %>%
  tab_style(style = cell_fill(color = "#F0F8FF"),    locations = cells_body(rows = tideHighLow == "Low")) %>%
  tab_style(style = cell_fill(color = "#FFF8DC"),    locations = cells_body(rows = tideHighLow == "High")) %>%

  fmt_number(columns = c(mean_JSD, median_JSD, sd_JSD,
                          mean_Similarity, median_Similarity, sd_Similarity), decimals = 3) %>%
  fmt_number(columns = c(n_indiv, n_detect, n_comparisons), decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size         = px(13),
    heading.title.font.size = px(16),
    data_row.padding        = px(3),
    table.width             = pct(100),
    row_group.font.weight   = "bold")

tbl_jsd_summary_tide


## ---- JSD by Tide — Boxplot ----

plot_jsd_box_tide <- jsd_results_tide %>%
  filter(species_1 %in% species_keep_tide) %>%
  ggplot(aes(x     = species_1,
             y     = .data[[jsd_metric_col]],
             fill  = species_1,
             alpha = tideHighLow,
             group = interaction(species_1, tideHighLow))) +

  geom_boxplot(position = position_dodge(width = dodge_width),
               outlier.shape = outlier_shape, outlier.size = outlier_size) +

  geom_point(position = position_jitterdodge(jitter.width = tide_jitter_width, dodge.width = dodge_width),
             size = point_size) +

  scale_fill_manual(values  = species_colors) +
  scale_alpha_manual(
    name   = "Tide",
    values = tide_alpha,
    guide  = guide_legend(override.aes = list(fill = tide_legend_colour))) +
  guides(fill = "none") +

  labs(
    title    = if (show_title) paste("Within-species", jsd_metric_label, "by Tide") else NULL,
    subtitle = if (show_subtitle) paste0("Pairwise comparison between individuals", filter_note(min_tags_tide_plot)) else NULL,
    x        = NULL,
    y        = jsd_metric_label) +

  theme_minimal() +
  theme(
    legend.position    = legend_position,
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(size = 9, color = "grey40"))

plot_jsd_box_tide

# Diagnostics / Misc ====

## Number of Tags Per Species ====
df.alltags |> 
  group_by(speciesEN) |> 
  summarise(n_tags = n_distinct(Band.ID))
