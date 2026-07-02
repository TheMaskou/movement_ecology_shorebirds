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
# Statistical testing: pairwise JSD values are non-independent (each individual
# contributes to n-1 pairs), so parametric/rank tests (ANOVA, Kruskal-Wallis) on
# the raw pairwise values would be invalid (pseudoreplication). Instead this
# script uses permutation-based tests that operate on the whole distance matrix
# and permute individuals rather than pairs:
#   - PERMANOVA (adonis2): do species differ in site-use composition (location)?
#   - Pairwise PERMANOVA: which species pairs differ (BH-adjusted post-hoc)
#   - PERMDISP (betadisper): do species differ in within-species dispersion
#     (spread of conspecific site use)?
#   - Per-species PERMANOVA + PERMDISP by tide: permutations blocked within
#     Band.ID to respect the paired (same-bird, both-tides) structure.
# Ported from ch1_10_entropy_evenness_composition.R, which implements the same
# tests; see that script for the original template.
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
#
# 4. DISTANCE METRIC: PERMANOVA/PERMDISP below use raw JSD (not its square
#    root). JSD itself is a semimetric — it satisfies non-negativity and
#    symmetry but not the triangle inequality; sqrt(JSD) is a true metric.
#    betadisper's internal ordination (PCoA) assumes Euclidean-embeddable
#    distances, so a non-metric input can produce a small negative-eigenvalue
#    correction (vegan handles this automatically; a routine situation for
#    non-Euclidean dissimilarities such as Bray-Curtis too). Raw JSD is used
#    here to stay numerically comparable with
#    ch1_10_entropy_evenness_composition.R; switching to sqrt(JSD) is a
#    possible future refinement to discuss with Maxime, not a bug.
#
# 5. PERMANOVA vs PERMDISP: PERMANOVA (adonis2) tests whether group centroids
#    differ (composition/location); a significant result can be driven by
#    dispersion differences rather than true location differences. PERMDISP
#    (betadisper) is reported alongside to separate the two.
#
# 6. VERY SMALL GROUPS: pairwise-species PERMANOVA and per-species by-tide
#    PERMANOVA/PERMDISP can return extreme, statistically-unreliable F-values
#    (with a "essentially perfect fit" warning from vegan) when a
#    species/tide combination has only 2-3 individuals — e.g. observed with
#    Red-necked Avocet (n=3) and Far Eastern Curlew's by-tide test (n=2
#    individuals x 2 tides = 4 points, 2 per group). These results are
#    mathematically correct given the design but not meaningful; the p-value
#    resolution is coarse (few possible permutations) and near-zero residual
#    variance inflates F. Treat any significant result for a species/test
#    near the min_indiv_test / max(4, min_indiv_test) threshold with caution
#    — do not report without checking n first.

source(here::here("qmd", "chapter_1", "R", "globals.R"))


# ==== Packages ====

library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(gt)
library(vegan)
library(philentropy)


# ==== Load Data ====

df.alltags <- readRDS(path_detection_data)

# Output directory for exported gt tables (see "Export Tables" section at the
# end of this script). Created if missing, since empty directories aren't
# tracked by git.
dir_tables <- here::here("tables")
dir.create(dir_tables, showWarnings = FALSE, recursive = TRUE)


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

## ---- Settings: Statistical Tests ----
# PERMANOVA / PERMDISP sections below (between-species and by-tide).

# Minimum tagged individuals per species to be included in a test. Species
# below this threshold are dropped (too few individuals give a degenerate
# PERMDISP dispersion estimate). Set to 1 to disable (test all species).
min_indiv_test <- 3


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


# ==== JSD — Distance Matrix ====
#
# Reconstruct the full individual x individual JSD distance matrix for use in
# PERMANOVA and PERMDISP below. Built directly from the site-use proportion
# matrix (prop_matrix) via philentropy::distance(), which returns JSD values
# numerically identical to calculate_jsd() (verified to ~1e-9) — used here
# instead of a hand-rolled loop for simplicity. calculate_jsd() itself is left
# untouched, since the descriptive plots/tables above depend on it.
#
# Unlike jsd_results (filtered to same_species for plotting), this matrix must
# retain cross-species pairs: PERMANOVA tests a species effect by contrasting
# between-species distances against within-species distances, so both are
# needed.

jsd_mat_full <- philentropy::distance(
  prop_matrix,
  method       = "jensen-shannon",
  unit         = "log2",
  mute.message = TRUE)
dimnames(jsd_mat_full) <- list(individual_ids, individual_ids)

# Species metadata in the same order as individual_ids
species_for_dist <- data.frame(Band.ID = individual_ids, stringsAsFactors = FALSE) %>%
  left_join(species_lookup, by = "Band.ID")

# Drop species below the min_indiv_test threshold (degenerate PERMDISP
# dispersion estimates otherwise) — applied to both the matrix and metadata,
# keeping them aligned.
species_n_indiv   <- table(species_for_dist$speciesEN)
species_keep_test <- names(species_n_indiv)[species_n_indiv >= min_indiv_test]
test_idx          <- which(species_for_dist$speciesEN %in% species_keep_test)

jsd_mat_test      <- jsd_mat_full[test_idx, test_idx]
species_for_dist  <- species_for_dist[test_idx, , drop = FALSE]
jsd_dist_full     <- as.dist(jsd_mat_test)


# ==== JSD — PERMANOVA Across Species ====
#
# Tests whether site-use composition (as captured by JSD) differs between
# species. PERMANOVA (adonis2) is permutation-based: it permutes individuals
# (not pairwise values) to build its null distribution, so it is valid for
# distance matrices where pairwise values are non-independent — unlike
# Kruskal-Wallis/ANOVA applied directly to the pairwise JSD values, which
# would treat n(n-1)/2 non-independent values as independent replicates.

set.seed(123)
permanova_jsd_species <- adonis2(
  jsd_dist_full ~ speciesEN,
  data         = species_for_dist,
  permutations = 999)

permanova_jsd_species_df <- as.data.frame(permanova_jsd_species) %>%
  tibble::rownames_to_column("Term") %>%
  filter(Term != "Total") %>%
  mutate(
    n_indiv = nrow(species_for_dist),
    R2      = round(R2, 3),
    F       = round(F,  3),
    p_value = `Pr(>F)`,
    Sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ "")) %>%
  relocate(n_indiv, .after = Term)

tbl_permanova_species <- permanova_jsd_species_df %>%
  gt(rowname_col = "Term") %>%
  tab_header(
    title    = md("**PERMANOVA — JSD Across Species**"),
    subtitle = md("*adonis2(JSD ~ speciesEN, permutations = 999)*")) %>%
  cols_label(
    n_indiv  = "N indiv.",
    Df       = "df",
    SumOfSqs = "Sum of Sqs",
    R2       = "R²",
    F        = "F",
    `Pr(>F)` = "p-value",
    Sig      = "") %>%
  fmt_number(columns = c(SumOfSqs, R2, F), decimals = 3) %>%
  fmt_number(columns = `Pr(>F)`, decimals = 3) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(columns = c(`Pr(>F)`, Sig),
                           rows = !is.na(p_value) & p_value < 0.05)) %>%
  tab_source_note(
    source_note = md(paste0(
      "Species with < ", min_indiv_test, " tagged individuals excluded. ",
      "Significance: *** p<0.001  ** p<0.01  * p<0.05  . p<0.1"))) %>%
  tab_options(heading.align = "left", table.font.size = 13)

tbl_permanova_species


## ---- JSD Pairwise PERMANOVA ----
#
# Post-hoc species-pair comparisons (mirrors Dunn post-hoc after Kruskal-
# Wallis in the entropy script); p-values adjusted with Benjamini-Hochberg
# (BH) correction for multiple comparisons.

species_unique     <- unique(species_for_dist$speciesEN)
species_pairs_mat  <- combn(species_unique, 2)
pairwise_permanova <- data.frame()

for (k in seq_len(ncol(species_pairs_mat))) {
  sp1 <- species_pairs_mat[1, k]
  sp2 <- species_pairs_mat[2, k]

  idx      <- which(species_for_dist$speciesEN %in% c(sp1, sp2))
  sub_dist <- as.dist(jsd_mat_test[idx, idx])
  sub_df   <- species_for_dist[idx, , drop = FALSE]

  if (n_distinct(sub_df$speciesEN) < 2) next

  set.seed(123)
  ad <- adonis2(sub_dist ~ speciesEN, data = sub_df, permutations = 999)

  # adonis2 labels the model term row "speciesEN" in some vegan versions and
  # "Model" in others (confirmed: this project's installed vegan uses "Model"
  # even without a custom permutation scheme) — look up whichever is present
  # rather than hardcoding one, same guard used in the by-tide loop below.
  tr <- intersect(c("speciesEN", "Model"), rownames(ad))[1]
  if (is.na(tr)) {
    message("Pairwise adonis2 succeeded for ", sp1, " vs ", sp2,
            " but term row not found: ", paste(rownames(ad), collapse = ", "))
    next
  }

  pairwise_permanova <- rbind(pairwise_permanova, data.frame(
    Species_1 = sp1,
    Species_2 = sp2,
    n_indiv   = length(idx),
    F_value   = round(ad[tr, "F"],      3),
    R2        = round(ad[tr, "R2"],     3),
    p_value   = round(ad[tr, "Pr(>F)"], 4),
    stringsAsFactors = FALSE))
}

pairwise_permanova$p_adj <- round(p.adjust(pairwise_permanova$p_value, method = "BH"), 4)
pairwise_permanova$Sig   <- case_when(
  pairwise_permanova$p_adj < 0.001 ~ "***",
  pairwise_permanova$p_adj < 0.01  ~ "**",
  pairwise_permanova$p_adj < 0.05  ~ "*",
  TRUE                             ~ "ns")

tbl_pairwise_permanova <- pairwise_permanova %>%
  arrange(p_adj) %>%
  gt() %>%
  tab_header(
    title    = md("**Pairwise PERMANOVA — JSD Species Comparisons**"),
    subtitle = md("p-values adjusted with Benjamini-Hochberg correction")) %>%
  cols_label(
    Species_1 = "Species 1",
    Species_2 = "Species 2",
    n_indiv   = "N indiv.",
    F_value   = "F",
    R2        = "R²",
    p_value   = "p unadj",
    p_adj     = "p adj (BH)",
    Sig       = "Sig.") %>%
  fmt_number(columns = c(F_value, R2), decimals = 3) %>%
  fmt_number(columns = c(p_value, p_adj), decimals = 3) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = Sig, rows = p_adj < 0.05)) %>%
  tab_style(
    style     = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = p_adj < 0.05)) %>%
  tab_footnote(
    footnote = md(
      "Each row is one pairwise species comparison. **R²**: proportion of
       variation in JSD explained by species membership. **p adj**: BH-corrected
       p-value. Significant result = site-use compositions differ between those
       two species.")) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(table.font.size = px(13), table.width = pct(100))

tbl_pairwise_permanova


## ---- JSD PERMDISP — Between-Species Dispersion ----
#
# Tests whether within-species variation in site-use composition (dispersion)
# differs between species. Complements PERMANOVA (which tests centroid
# location). High dispersion in a species = conspecifics use very different
# sites — this is the most direct read of "does JSD differ between species".

betadisp_jsd_species <- betadisper(jsd_dist_full, group = species_for_dist$speciesEN)

set.seed(123)
permutest_betadisp_species <- permutest(betadisp_jsd_species, permutations = 999)

# permutest.betadisper$tab column order: Df(1), SS(2), MeanSqs(3), F(4), N.Perm(5), Pr(>F)(6)
# Column *names* vary across vegan versions so access by position except for Pr(>F)
.perm_tab_sp <- permutest_betadisp_species$tab
.pval_col_sp <- grep("Pr\\(", colnames(.perm_tab_sp), value = TRUE)[1]

betadisp_summary_df <- data.frame(
  Term    = rownames(.perm_tab_sp),
  n_indiv = nrow(species_for_dist),
  Df      = .perm_tab_sp[, 1],
  SS      = round(.perm_tab_sp[, 2], 4),
  MeanSqs = round(.perm_tab_sp[, 3], 4),
  F       = round(.perm_tab_sp[, 4], 3),
  p_value = .perm_tab_sp[, .pval_col_sp])

betadisp_summary_df$Sig <- case_when(
  !is.na(betadisp_summary_df$p_value) & betadisp_summary_df$p_value < 0.001 ~ "***",
  !is.na(betadisp_summary_df$p_value) & betadisp_summary_df$p_value < 0.01  ~ "**",
  !is.na(betadisp_summary_df$p_value) & betadisp_summary_df$p_value < 0.05  ~ "*",
  !is.na(betadisp_summary_df$p_value) & betadisp_summary_df$p_value < 0.1   ~ ".",
  TRUE                                                                        ~ "")

tbl_permdisp_species <- betadisp_summary_df %>%
  gt(rowname_col = "Term") %>%
  tab_header(
    title    = md("**PERMDISP — Within-Species JSD Dispersion**"),
    subtitle = md("*betadisper + permutest, permutations = 999*")) %>%
  cols_label(
    n_indiv = "N indiv.",
    Df      = "df",
    SS      = "SS",
    MeanSqs = "Mean Sq",
    F       = "F",
    p_value = "p-value",
    Sig     = "") %>%
  fmt_number(columns = c(SS, MeanSqs, F), decimals = 3) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  tab_source_note(
    source_note = md(
      "PERMDISP tests homogeneity of multivariate dispersion (within-group spread
       in JSD space). Significant result = species differ in how variable
       conspecific site use is — *not* in where they forage (that is PERMANOVA).
       Species with < min_indiv_test tagged individuals excluded.
       **Significance:** *** p<0.001  ** p<0.01  * p<0.05  . p<0.1")) %>%
  tab_options(heading.align = "left", table.font.size = 13)

tbl_permdisp_species


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


# ==== JSD by Tide — PERMANOVA and PERMDISP (per species) ====
#
# Tests whether tide condition shifts site-use composition within each
# species. For each species, builds a full JSD distance matrix across all
# Bird x Tide observations (including cross-tide pairs — this is why
# jsd_results_tide, which only keeps within-tide pairs, cannot be reused
# here), then runs:
#   - adonis2(dist ~ tideHighLow), permutations blocked within Band.ID
#     (permute::how(blocks = ...)) to respect the paired (same bird, both
#     tides) structure
#   - betadisper + permutest to test whether dispersion differs by tide
#
# This is the valid replacement for a test applied directly to
# jsd_results_tide, which would carry the same pseudoreplication problem as
# the between-species case above.

site_use_tide_meta <- site_use_matrix_tide %>%
  select(id_tide, Band.ID, tideHighLow) %>%
  mutate(Band.ID = as.character(Band.ID)) %>%
  left_join(species_lookup, by = "Band.ID")

results_jsd_tide_permanova <- data.frame()
results_jsd_tide_betadisp  <- data.frame()

for (sp in unique(site_use_tide_meta$speciesEN)) {

  sp_idx  <- which(site_use_tide_meta$speciesEN == sp)
  sp_meta <- site_use_tide_meta[sp_idx, ]
  sp_prop <- prop_matrix_tide[sp_idx, , drop = FALSE]

  n_sp       <- nrow(sp_prop)
  n_tides    <- n_distinct(sp_meta$tideHighLow)
  n_indiv_sp <- n_distinct(sp_meta$Band.ID)   # distinct birds, vs. n_sp (bird x tide rows)

  if (n_sp < max(4, min_indiv_test) || n_tides < 2) {
    message("Skipping ", sp, ": < ", max(4, min_indiv_test), " obs or < 2 tide levels for tide PERMANOVA")
    next
  }

  # Full JSD distance matrix for this species (all cross-tide pairs included)
  sp_jsd_mat  <- philentropy::distance(sp_prop, method = "jensen-shannon",
                                       unit = "log2", mute.message = TRUE)
  dimnames(sp_jsd_mat) <- list(sp_meta$id_tide, sp_meta$id_tide)
  sp_jsd_dist <- as.dist(sp_jsd_mat)

  # Stratified permutations within Band.ID (paired structure)
  perm_h <- permute::how(blocks = sp_meta$Band.ID, nperm = 999)

  set.seed(123)
  ad_sp <- tryCatch(
    adonis2(sp_jsd_dist ~ tideHighLow, data = sp_meta, permutations = perm_h),
    error = function(e) {
      message("adonis2 failed for ", sp, ": ", conditionMessage(e))
      NULL})

  if (is.null(ad_sp)) {
    message("adonis2 returned NULL for ", sp, " — skipping PERMANOVA row")
  } else {
    # When adonis2 is called with a custom how() scheme it labels the term "Model"
    # rather than the variable name; try both so the code works across vegan versions
    tr <- intersect(c("tideHighLow", "Model"), rownames(ad_sp))[1]
    if (is.na(tr)) {
      message("adonis2 succeeded for ", sp, " but term row not found: ",
              paste(rownames(ad_sp), collapse = ", "))
    } else {
      results_jsd_tide_permanova <- rbind(results_jsd_tide_permanova, data.frame(
        Species = sp,
        n_indiv = n_indiv_sp,
        Df      = ad_sp[tr, "Df"],
        SS      = round(ad_sp[tr, "SumOfSqs"], 4),
        R2      = round(ad_sp[tr, "R2"],       3),
        F       = round(ad_sp[tr, "F"],        3),
        p_value = round(ad_sp[tr, "Pr(>F)"],   4),
        stringsAsFactors = FALSE))
    }
  }

  bd_sp <- tryCatch(
    betadisper(sp_jsd_dist, group = sp_meta$tideHighLow),
    error = function(e) NULL)

  if (!is.null(bd_sp)) {
    set.seed(123)
    perm_bd     <- permutest(bd_sp, permutations = 999)
    .pval_col_t <- grep("Pr\\(", colnames(perm_bd$tab), value = TRUE)[1]
    results_jsd_tide_betadisp <- rbind(results_jsd_tide_betadisp, data.frame(
      Species = sp,
      n_indiv = n_indiv_sp,
      F       = round(perm_bd$tab["Groups", 4],           3),
      Df      = perm_bd$tab["Groups", 1],
      SS      = round(perm_bd$tab["Groups", 2],           4),
      p_value = round(perm_bd$tab["Groups", .pval_col_t], 4),
      stringsAsFactors = FALSE))
  }
}

if (nrow(results_jsd_tide_permanova) > 0) {
  results_jsd_tide_permanova <- results_jsd_tide_permanova %>%
    mutate(Sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""))
} else {
  message("Note: per-species tide PERMANOVA produced no results — check diagnostic messages above.")
}

if (nrow(results_jsd_tide_betadisp) > 0) {
  results_jsd_tide_betadisp <- results_jsd_tide_betadisp %>%
    mutate(Sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""))
} else {
  message("Note: per-species tide PERMDISP produced no results — check diagnostic messages above.")
}


## ---- JSD by Tide — PERMANOVA gt Table ----

if (nrow(results_jsd_tide_permanova) > 0) {
  tbl_jsd_permanova_tide <- results_jsd_tide_permanova %>%
    gt(groupname_col = "Species") %>%
    tab_header(
      title    = md("**PERMANOVA — Tide Effect on JSD Composition (per species)**"),
      subtitle = md("*adonis2(JSD ~ tideHighLow), permutations blocked within Band.ID*")) %>%
    cols_label(
      n_indiv = "N indiv.",
      Df      = "df",
      SS      = "Sum of Sqs",
      R2      = "R²",
      F       = "F",
      p_value = "p-value",
      Sig     = "") %>%
    fmt_number(columns = c(SS, R2, F), decimals = 3) %>%
    fmt_number(columns = p_value, decimals = 3) %>%
    tab_style(
      style     = cell_text(color = "#c0392b", weight = "bold"),
      locations = cells_body(columns = c(p_value, Sig),
                             rows = p_value < 0.05)) %>%
    tab_source_note(
      source_note = md(
        "Permutations blocked within Band.ID to account for within-individual
         pairing. Significant result = site-use composition shifts significantly
         between High and Low tide for that species.
         **Significance:** *** p<0.001  ** p<0.01  * p<0.05  . p<0.1")) %>%
    tab_options(
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4f8",
      heading.align              = "left",
      table.font.size            = 13)
} else {
  tbl_jsd_permanova_tide <- NULL
}

tbl_jsd_permanova_tide


## ---- JSD by Tide — PERMDISP gt Table ----

if (nrow(results_jsd_tide_betadisp) > 0) {
  tbl_jsd_permdisp_tide <- results_jsd_tide_betadisp %>%
    gt(groupname_col = "Species") %>%
    tab_header(
      title    = md("**PERMDISP — Tide Effect on Within-Group JSD Dispersion (per species)**"),
      subtitle = md("*betadisper + permutest, permutations = 999*")) %>%
    cols_label(
      n_indiv = "N indiv.",
      F       = "F",
      Df      = "df",
      SS      = "SS",
      p_value = "p-value",
      Sig     = "") %>%
    fmt_number(columns = c(F, SS), decimals = 3) %>%
    fmt_number(columns = p_value, decimals = 3) %>%
    tab_source_note(
      source_note = md(
        "PERMDISP tests whether variance in site-use composition differs between
         tide states. A significant result means conspecifics are more (or less)
         variable in their site use at one tide than the other.
         **Significance:** *** p<0.001  ** p<0.01  * p<0.05  . p<0.1")) %>%
    tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13)
} else {
  tbl_jsd_permdisp_tide <- NULL
}

tbl_jsd_permdisp_tide


# ==== Export Tables (docx) ====
#
# Exports every gt table produced by this script to a Word document via
# gtsave() (pandoc-based; format inferred from the .docx extension), for
# pasting directly into a manuscript. One file per table in dir_tables,
# named after the table object with the tbl_ prefix stripped. The two
# by-tide PERMANOVA/PERMDISP tables can be NULL (see "JSD by Tide —
# PERMANOVA and PERMDISP" above) if no species had enough per-tide
# observations to test — skipped with a message rather than erroring.

tables_to_export <- list(
  jsd_summary        = tbl_jsd_summary,
  jsd_per_indiv      = tbl_jsd_per_indiv,
  jsd_summary_tide   = tbl_jsd_summary_tide,
  permanova_species  = tbl_permanova_species,
  pairwise_permanova = tbl_pairwise_permanova,
  permdisp_species   = tbl_permdisp_species,
  jsd_permanova_tide = tbl_jsd_permanova_tide,
  jsd_permdisp_tide  = tbl_jsd_permdisp_tide)

for (tbl_name in names(tables_to_export)) {
  tbl <- tables_to_export[[tbl_name]]
  if (is.null(tbl)) {
    message("Skipping export for ", tbl_name, ": table is NULL")
    next
  }
  # gtsave()'s docx export shells out to pandoc via rmarkdown::pandoc_convert()
  # with no explicit `to =`, so pandoc infers the format from the output
  # filename's extension. On Windows, rmarkdown converts any output path
  # containing a space (this repo lives under ".../Shorebirds Newcastle/...")
  # to its legacy 8.3 short path name — but Windows can only generate a short
  # name for a file that already exists, and 8.3 extensions are capped at 3
  # chars. So a *pre-existing* "name.docx" becomes "NAME~1.DOC" and pandoc
  # errors with "Unknown output format doc", while a brand-new file keeps its
  # long path (and correct .docx extension). Deleting any previous export
  # first keeps every run in the "new file" case.
  target_path <- file.path(dir_tables, paste0(tbl_name, ".docx"))
  if (file.exists(target_path)) file.remove(target_path)
  gtsave(tbl, filename = paste0(tbl_name, ".docx"), path = dir_tables)
}


# Diagnostics / Misc ====

## Number of Tags Per Species ====
df.alltags |> 
  group_by(speciesEN) |> 
  summarise(n_tags = n_distinct(Band.ID))
