# ==== Shannon Entropy, Evenness and Effective Sites ====
#
# Quantifies site-use diversity for each tagged shorebird via Shannon entropy
# (H) of its detection-proportion distribution across Motus stations, plus
# two derived metrics:
#
#   - Pielou's evenness (J = H / ln(S)) — rescales H to [0,1] independent of
#     the number of sites: "Are the sites a bird uses visited equally?"
#   - Effective number of sites (exp(H)) — interpretable count of
#     equally-used sites.
#
# H is a snapshot diversity/breadth metric; low H (concentrated, predictable
# use of few sites) serves as a proxy/correlate of high site fidelity. Strict
# site fidelity is a temporal concept (returning to the same sites over time)
# — entropy of detection proportions does not by itself encode that return
# structure, so H indicates but does not directly measure temporal fidelity.
#
# Metrics are computed globally and split by tide (High/Low) to inspect
# whether site-use diversity changes with tidal condition.
#
# Includes a between-species test of J (Pielou's evenness): ANOVA assumption
# checks -> ANOVA -> Kruskal-Wallis assumption checks -> Kruskal-Wallis, so the
# inappropriate test can be deleted once assumptions are inspected. The broader
# inferential suite (H, exp_H, S, JSD, PERMANOVA, tide effects, etc.) lives in
# ch1_10_entropy_evenness_composition.R.
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
# 3. SAMPLING-EFFORT DEPENDENCE: S and H scale with total detections per
#    individual; estimates are not rarefied. total_detections is reported
#    alongside all metrics to enable this assessment.

source(here::here("qmd", "chapter_1", "R", "globals.R"))


# ==== Packages ====

library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(gt)
library(car)   # leveneTest (ANOVA assumption check)
library(FSA)   # dunnTest (Kruskal-Wallis post-hoc)


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
# Visual controls shared across the entropy boxplots below. Edit here to
# restyle without hunting through each ggplot block.

# Metrics shown in the "Selected Metrics" plots (friendly names or raw codes):
# "Sites"/"S", "Entropy"/"H", "Evenness"/"J", "Effective Sites"/"exp_H"
selected_metrics <- c("Sites", "Evenness")

# Lookup: friendly name / code -> factor label used in the plots
metric_labels <- c(
  S     = "S (Number of Sites)",      Sites             = "S (Number of Sites)",
  H     = "H (Shannon Entropy)",      Entropy           = "H (Shannon Entropy)",
  J     = "J (Pielou's Evenness)",    Evenness          = "J (Pielou's Evenness)",
  exp_H = "exp(H) (Effective Sites)", `Effective Sites` = "exp(H) (Effective Sites)")

# Facet layout: single column when <= this many metrics, else 2 columns
facet_single_col_max <- 2

## ---- Settings: All Plots ----

outlier_shape   <- 16   # outlier symbol
axis_text_angle <- 45   # x-axis label rotation (degrees)
axis_text_size  <- 9    # x-axis label size
strip_text_size <- 11   # facet strip label size

# Minimum tagged individuals (distinct Band.ID) per species for that species to
# appear in a plot. Species below the threshold are dropped from that plot only
# (summary tables are unaffected). Set to 1 for no filtering.
min_tags_main_plot <- 1   # non-tide boxplots (plot_entropy_box, plot_entropy_box_selected)
min_tags_tide_plot <- 1   # by-tide boxplots (plot_entropy_box_tide, plot_entropy_box_tide_selected)

# Visibility of basic plot elements (TRUE = shown)
show_title    <- TRUE
show_subtitle <- TRUE

## ---- Settings: Non-tide Plots ----
# plot_entropy_box, plot_entropy_box_selected (geom_boxplot + geom_jitter)

box_alpha    <- 0.7   # box fill opacity
jitter_width <- 0.2   # horizontal spread of jittered points
jitter_alpha <- 0.3   # jittered point opacity
jitter_size  <- 1     # jittered point size

## ---- Settings: By-tide Plots ----
# plot_entropy_box_tide, plot_entropy_box_tide_selected
# (dodged boxplot + dodged jittered points + Tide legend)

point_size         <- 1.1   # jittered point size
dodge_width        <- 0.8   # gap between High/Low boxes
tide_jitter_width  <- 0.1   # point spread within a box
tide_alpha         <- c(High = 1.0, Low = 0.2)  # opacity per tide state
tide_legend_colour <- "grey30"                  # Tide legend swatch colour
legend_position    <- "top"                     # Tide legend position

# Derived (used by both Selected-Metrics plots)
selected_labels <- unname(metric_labels[selected_metrics])
ncol_selected   <- if (length(selected_metrics) <= facet_single_col_max) 1 else 2


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


# ==== Shannon Entropy Function ====

# Calculates Shannon entropy H = -sum(p * ln(p)) for a vector of proportions.
calculate_shannon <- function(proportions) {
  p <- proportions[proportions > 0]   # exclude zeros (log(0) undefined)
  H <- -sum(p * log(p))
  return(H)
}


# ==== Shannon Entropy — Per Individual ====
#
# For each bird, count detections at each station, convert to proportions,
# then compute:
#   S     = number of unique stations used
#   H     = Shannon entropy of the station-use distribution (full precision)
#   J     = Pielou's evenness = H / ln(S);  NA when S = 1 (0/0 undefined)
#   exp_H = effective number of equally-used sites = exp(H)
#
# NOTE: numeric values are kept at full precision here; round only for display.

entropy_results <- df.alltags %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%

  # Count detections per bird per site
  group_by(speciesEN, Band.ID, recvDeployName) %>%
  summarise(detections = n(), .groups = "drop") %>%

  # Proportions per bird
  group_by(speciesEN, Band.ID) %>%
  mutate(
    total_detections = sum(detections),
    proportion       = detections / total_detections) %>%

  # Entropy metrics per bird
  summarise(
    S                = n(),
    H                = calculate_shannon(proportion),
    J                = if_else(S == 1, NA_real_, H / log(S)),   # guard S=1
    total_detections = first(total_detections),
    .groups          = "drop") %>%

  mutate(exp_H = exp(H)) %>%
  arrange(speciesEN, Band.ID)


## ==== Shannon Entropy — Species Summary Table ====

species_entropy_summary <- entropy_results %>%
  group_by(speciesEN) %>%
  summarise(
    n_individuals = n(),

    median_S = median(S, na.rm = TRUE),
    mean_S   = round(mean(S, na.rm = TRUE), 2),
    sd_S     = round(sd(S,   na.rm = TRUE), 2),

    median_H = median(H, na.rm = TRUE),
    mean_H   = round(mean(H, na.rm = TRUE), 2),
    sd_H     = round(sd(H,   na.rm = TRUE), 2),
    CV_H     = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),

    median_J = median(J, na.rm = TRUE),
    mean_J   = round(mean(J, na.rm = TRUE), 2),
    sd_J     = round(sd(J,   na.rm = TRUE), 2),

    median_exp_H = round(median(exp_H, na.rm = TRUE), 2),
    mean_exp_H   = round(mean(exp_H,   na.rm = TRUE), 2),
    sd_exp_H     = round(sd(exp_H,     na.rm = TRUE), 2),

    total_detections = sum(total_detections, na.rm = TRUE),

    .groups = "drop") %>%
  arrange(speciesEN)


tbl_entropy_summary <- species_entropy_summary %>%

  gt() %>%

  tab_header(
    title = md("**Table.** Shannon entropy metrics for site-use diversity of shorebird species in the Hunter estuary.")) %>%
  opt_align_table_header(align = "left") %>%

  tab_footnote(
    footnote = md(
      "**Legend.** Grouped by species, this table summarises site-use diversity
       metrics using Shannon entropy analysis. **n_individuals**: number of tagged
       individuals; **S**: number of unique sites used; **H**: Shannon entropy
       (higher = more generalised / spread site use); **J**: Pielou's evenness
       (0–1, higher = more even use across sites visited; NA for single-site
       birds); **exp(H)**: effective number of equally-used sites; **CV_H**:
       coefficient of variation in H (% between-individual variability within
       species); **total_detections**: total detections across all individuals.
       See Limitations in script header for important caveats on detection-count
       currency and station uptime.")) %>%
  opt_table_font(font = "Times New Roman") %>%

  cols_label(
    speciesEN        = "Species (En.)",
    n_individuals    = "N individuals",
    median_S         = "Median S",
    mean_S           = "Mean S",
    sd_S             = "SD S",
    median_H         = "Median H",
    mean_H           = "Mean H",
    sd_H             = "SD H",
    CV_H             = "CV H (%)",
    median_J         = "Median J",
    mean_J           = "Mean J",
    sd_J             = "SD J",
    median_exp_H     = "Median exp(H)",
    mean_exp_H       = "Mean exp(H)",
    sd_exp_H         = "SD exp(H)",
    total_detections = "Total detections") %>%

  tab_spanner(label = "Sample",                   columns = c(n_individuals, total_detections)) %>%
  tab_spanner(label = "Number of Sites (S)",      columns = c(median_S, mean_S, sd_S)) %>%
  tab_spanner(label = "Shannon Entropy (H)",      columns = c(median_H, mean_H, sd_H, CV_H)) %>%
  tab_spanner(label = "Pielou's Evenness (J)",    columns = c(median_J, mean_J, sd_J)) %>%
  tab_spanner(label = "Effective Sites [exp(H)]", columns = c(median_exp_H, mean_exp_H, sd_exp_H)) %>%

  tab_style(style = cell_text(weight = "bold"),      locations = cells_column_labels()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"), locations = cells_body(columns = everything())) %>%
  tab_style(style = cell_text(whitespace = "nowrap"), locations = cells_column_labels(columns = everything())) %>%

  fmt_number(columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H), decimals = 2) %>%
  fmt_number(columns = c(median_S, median_H, median_J, median_exp_H), decimals = 2) %>%
  fmt_number(columns = CV_H,             decimals = 1) %>%
  fmt_number(columns = total_detections, decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size         = px(14),
    heading.title.font.size = px(16),
    data_row.padding        = px(3),
    table.width             = pct(100))

tbl_entropy_summary


## ---- Boxplot - All Metrics ----

entropy_for_plot <- entropy_results %>%
  filter(speciesEN %in% species_keep_main) %>%
  select(speciesEN, Band.ID, S, H, J, exp_H) %>%
  pivot_longer(
    cols      = c(S, H, J, exp_H),
    names_to  = "metric",
    values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("S", "H", "J", "exp_H"),
                         labels = c("S (Number of Sites)",
                                    "H (Shannon Entropy)",
                                    "J (Pielou's Evenness)",
                                    "exp(H) (Effective Sites)")))

plot_entropy_box <- ggplot(entropy_for_plot, aes(x = speciesEN, y = value, fill = speciesEN)) +
  geom_boxplot(alpha = box_alpha, outlier.shape = outlier_shape) +
  geom_jitter(width = jitter_width, alpha = jitter_alpha, size = jitter_size) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = if (show_title) "Shannon Entropy Metrics by Species" else NULL,
    subtitle = if (show_subtitle) paste0("Distribution of site-use diversity across individuals", filter_note(min_tags_main_plot)) else NULL,
    x        = "Species",
    y        = "Value",
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    strip.text         = element_text(face = "bold", size = strip_text_size),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_entropy_box


## ---- Boxplot - Selected Metrics ----
#
# Metrics shown here are controlled by `selected_metrics` in Plot Settings
# above. Layout: 2 columns for 3-4 metrics, 1 column (2 rows) for 1-2 metrics.

entropy_for_plot_selected <- entropy_for_plot %>%
  filter(metric %in% selected_labels) %>%
  mutate(metric = droplevels(metric))

plot_entropy_box_selected <- ggplot(entropy_for_plot_selected,
       aes(x = speciesEN, y = value, fill = speciesEN)) +
  geom_boxplot(alpha = box_alpha, outlier.shape = outlier_shape) +
  geom_jitter(width = jitter_width, alpha = jitter_alpha, size = jitter_size) +
  facet_wrap(~ metric, scales = "free_y", ncol = ncol_selected) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = if (show_title) "Selected Entropy Metrics by Species" else NULL,
    subtitle = if (show_subtitle) paste0("Distribution of site-use diversity across individuals", filter_note(min_tags_main_plot)) else NULL,
    x        = "Species",
    y        = "Value",
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    strip.text         = element_text(face = "bold", size = strip_text_size),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_entropy_box_selected


# ==== Between-Species Test: Pielou's Evenness (J) ====
#
# Does site-use evenness (J) differ between species? Each bird contributes one
# J value, so individuals (Band.ID) are independent replicates within species —
# a valid between-groups comparison.

## Setup Including Minimum Samples ====

# J is undefined (NA) for single-site birds (S = 1) and is excluded here.
# Species need >= 3 individuals with a defined J to be tested.

j_test_data <- entropy_results %>%
  filter(!is.na(J)) %>%
  group_by(speciesEN) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  mutate(speciesEN = factor(speciesEN))

# Sample sizes per species going into the tests below
j_test_data %>%
  count(speciesEN, name = "n") %>%
  print()


## ---- ANOVA Assumptions ----
#
# 1. Independence: satisfied by design (one J value per individual).
# 2. Normality of residuals: Shapiro-Wilk test on the residuals of the ANOVA
#    model. p > 0.05 => residuals consistent with a normal distribution.
# 3. Homogeneity of variance: Levene's test across species. p > 0.05 => group
#    variances are consistent with being equal.
#
# If either normality or equal-variance fails (p < 0.05), prefer the
# Kruskal-Wallis test below and delete the ANOVA block.

aov_J <- aov(J ~ speciesEN, data = j_test_data)

# Normality of residuals
shapiro.test(residuals(aov_J))

# Visual check (optional)
qqnorm(residuals(aov_J)); qqline(residuals(aov_J))

# Homogeneity of variance
car::leveneTest(J ~ speciesEN, data = j_test_data)


## ---- One-Way ANOVA ----

# NOTE: Commented out, as the dataset as of 13/06/2026 failed Levene's

# #
# # Tests whether mean J differs between species. Only proceed/trust this if the
# # assumption checks above were reasonable.
# 
# summary(aov_J)   # p-value for "speciesEN" row = overall species effect
# 
# # Tukey HSD post-hoc: which species pairs differ (only meaningful if the
# # ANOVA assumptions above held).
# TukeyHSD(aov_J)


## ---- Kruskal-Wallis Assumptions ----
#
# Kruskal-Wallis requires:
# 1. Independence: satisfied by design (as above).
# 2. To interpret a significant result as a difference in MEDIANS, the
#    species' J distributions should have similar shape/spread. If shapes
#    differ a lot, a significant KW still indicates one species tends to have
#    systematically higher/lower J (stochastic dominance) — just not strictly
#    a median difference.
#
# Quick eyeball of spread per species (also visible in the boxplots above):

j_test_data %>%
  group_by(speciesEN) %>%
  summarise(
    n   = n(),
    sd  = round(sd(J),  3),
    IQR = round(IQR(J), 3),
    .groups = "drop") %>%
  print()


## ---- Kruskal-Wallis Test ----
#
# Non-parametric alternative to ANOVA — does not require normal residuals or
# equal variances. Tests whether J distributions differ between species.

kt_J <- kruskal.test(J ~ speciesEN, data = j_test_data)
kt_J

# Dunn post-hoc (Bonferroni-corrected): which species pairs differ. Most
# informative when the omnibus Kruskal-Wallis above is significant.
dunn_J <- dunnTest(J ~ speciesEN, data = j_test_data, method = "bonferroni")
dunn_J


## ---- Kruskal-Wallis Results Table ----

kw_J_results <- data.frame(
  Metric       = "J",
  n_species    = n_distinct(j_test_data$speciesEN),
  n_indiv      = nrow(j_test_data),
  Chi_sq       = round(kt_J$statistic, 3),
  df           = kt_J$parameter,
  p_value      = kt_J$p.value,
  Significance = case_when(
    kt_J$p.value < 0.001 ~ "***",
    kt_J$p.value < 0.01  ~ "**",
    kt_J$p.value < 0.05  ~ "*",
    TRUE                 ~ "ns"),
  row.names        = NULL,
  stringsAsFactors = FALSE)

tbl_kw_between <- kw_J_results %>%
  gt() %>%
  tab_header(
    title    = md("**Between-Species: Kruskal-Wallis Test Results**"),
    subtitle = "Do species differ in site-use diversity metrics? (individuals as replicates)") %>%
  cols_label(
    Metric       = "Metric",
    n_species    = "N species",
    n_indiv      = "N indiv.",
    Chi_sq       = "χ²",
    df           = "df",
    p_value      = "p-value",
    Significance = "Sig.") %>%
  fmt_number(columns = Chi_sq,   decimals = 3) %>%
  fmt_scientific(columns = p_value, decimals = 3) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = Significance, rows = p_value < 0.05)) %>%
  tab_style(
    style     = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = p_value < 0.05)) %>%
  tab_footnote(
    footnote = md(
      "**Significance codes:** *** p < 0.001, ** p < 0.01, * p < 0.05, ns = not
       significant (p ≥ 0.05). Only species with ≥ 3 individuals included.
       J is NA for single-station birds and excluded from that metric's test.")) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.font.size         = px(14),
    heading.title.font.size = px(16),
    data_row.padding        = px(5),
    table.width             = pct(100))

tbl_kw_between


## ---- Dunn Post-Hoc Results Table ----

dunn_J_df <- dunn_J$res %>%
  arrange(Comparison) %>%
  mutate(Significance = case_when(
    P.adj < 0.001 ~ "***",
    P.adj < 0.01  ~ "**",
    P.adj < 0.05  ~ "*",
    TRUE          ~ "ns")) %>%
  select(Comparison, Z, P.unadj, P.adj, Significance)

tbl_dunn_between <- dunn_J_df %>%
  gt() %>%
  tab_header(
    title    = md("**Dunn Post-Hoc Test — Between-Species (Bonferroni)**"),
    subtitle = "Pairwise species comparisons for Pielou's evenness (J)") %>%
  cols_label(
    Comparison   = "Comparison",
    Z            = "Z",
    P.unadj      = "p unadj",
    P.adj        = "p adj",
    Significance = "Sig.") %>%
  fmt_scientific(columns = c(P.unadj, P.adj), decimals = 3) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = Significance, rows = P.adj < 0.05)) %>%
  tab_style(
    style     = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = P.adj < 0.05)) %>%
  tab_footnote(
    footnote = md(
      "Only shown for metrics where Kruskal-Wallis was significant (p < 0.05).
       **p adj**: Bonferroni-corrected p-value.")) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.font.size            = px(14),
    heading.title.font.size    = px(16),
    data_row.padding           = px(5),
    table.width                = pct(100))

tbl_dunn_between


## ---- Shannon Entropy Individual-Level Table ----

tbl_entropy_individual <- entropy_results %>%
  select(speciesEN, Band.ID, S, H, J, exp_H) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**Shannon Entropy Metrics by Individual**")) %>%
  cols_label(
    speciesEN = "Species",
    Band.ID   = "Band ID",
    S         = "S",
    H         = "H",
    J         = "J",
    exp_H     = "exp(H)") %>%
  fmt_number(columns = c(H, J, exp_H), decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

tbl_entropy_individual


# ==== Shannon Entropy — By Tide Per Individual ====
#
# Re-computes entropy metrics (S, H, J, exp_H) separately for High and Low
# tide detections. Allows inspection of whether site-use breadth differs by
# tidal state (e.g., concentrated foraging sites at low tide vs spread roosting
# at high tide).

entropy_results_tide <- df.alltags %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%

  group_by(speciesEN, Band.ID, recvDeployName, tideHighLow) %>%
  summarise(detections = n(), .groups = "drop") %>%

  group_by(speciesEN, Band.ID, tideHighLow) %>%
  mutate(
    total_detections = sum(detections),
    proportion       = detections / total_detections) %>%

  # Full precision — round only at display stage
  summarise(
    S                = n(),
    H                = calculate_shannon(proportion),
    J                = if_else(S == 1, NA_real_, H / log(S)),   # guard S=1
    total_detections = first(total_detections),
    .groups          = "drop") %>%

  mutate(exp_H = exp(H)) %>%
  arrange(speciesEN, Band.ID, tideHighLow)


## ---- Shannon by Tide — Species Summary ----

species_entropy_summary_tide <- entropy_results_tide %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(
    n_individuals = n(),

    median_S = median(S, na.rm = TRUE),
    mean_S   = round(mean(S, na.rm = TRUE), 2),
    sd_S     = round(sd(S,   na.rm = TRUE), 2),

    median_H = median(H, na.rm = TRUE),
    mean_H   = round(mean(H, na.rm = TRUE), 2),
    sd_H     = round(sd(H,   na.rm = TRUE), 2),
    CV_H     = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),

    median_J = median(J, na.rm = TRUE),
    mean_J   = round(mean(J, na.rm = TRUE), 2),
    sd_J     = round(sd(J,   na.rm = TRUE), 2),

    median_exp_H = round(median(exp_H, na.rm = TRUE), 2),
    mean_exp_H   = round(mean(exp_H,   na.rm = TRUE), 2),
    sd_exp_H     = round(sd(exp_H,     na.rm = TRUE), 2),

    total_detections = sum(total_detections, na.rm = TRUE),

    .groups = "drop") %>%
  arrange(speciesEN)

## ---- Boxplot by Tide - All Metrics ----

entropy_for_plot_tide <- entropy_results_tide %>%
  filter(speciesEN %in% species_keep_tide) %>%
  select(speciesEN, tideHighLow, Band.ID, S, H, J, exp_H) %>%
  pivot_longer(
    cols      = c(S, H, J, exp_H),
    names_to  = "metric",
    values_to = "value") %>%
  mutate(metric = factor(metric,
                         levels = c("S", "H", "J", "exp_H"),
                         labels = c("S (Number of Sites)",
                                    "H (Shannon Entropy)",
                                    "J (Pielou's Evenness)",
                                    "exp(H) (Effective Sites)")))

plot_entropy_box_tide <- ggplot(entropy_for_plot_tide,
       aes(x = speciesEN, y = value, fill = speciesEN, alpha = tideHighLow)) +
  geom_boxplot(aes(group = interaction(speciesEN, tideHighLow)),
               position = position_dodge(width = dodge_width),
               outlier.shape = outlier_shape) +
  geom_point(aes(group = interaction(speciesEN, tideHighLow)),
             position = position_jitterdodge(jitter.width = tide_jitter_width, dodge.width = dodge_width),
             size = point_size) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  scale_alpha_manual(
    name   = "Tide",
    values = tide_alpha,
    guide  = guide_legend(override.aes = list(fill = tide_legend_colour))) +
  guides(fill = "none") +
  labs(
    title    = if (show_title) "Shannon Entropy Metrics by Species and Tide" else NULL,
    subtitle = if (show_subtitle) paste0("Site-use diversity across individuals and tide", filter_note(min_tags_tide_plot)) else NULL,
    x        = "Species",
    y        = "Value") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    strip.text         = element_text(face = "bold", size = strip_text_size),
    legend.position    = legend_position,
    panel.grid.major.x = element_blank())

plot_entropy_box_tide


## ---- Boxplot by Tide - Selected Metrics ----
#
# Metrics shown here are controlled by `selected_metrics` in Plot Settings
# above (shared with the non-tide "Selected Metrics" plot).

entropy_for_plot_tide_selected <- entropy_for_plot_tide %>%
  filter(metric %in% selected_labels) %>%
  mutate(metric = droplevels(metric))

plot_entropy_box_tide_selected <- ggplot(entropy_for_plot_tide_selected,
       aes(x = speciesEN, y = value, fill = speciesEN, alpha = tideHighLow)) +
  geom_boxplot(aes(group = interaction(speciesEN, tideHighLow)),
               position = position_dodge(width = dodge_width),
               outlier.shape = outlier_shape) +
  geom_point(aes(group = interaction(speciesEN, tideHighLow)),
             position = position_jitterdodge(jitter.width = tide_jitter_width, dodge.width = dodge_width),
             size = point_size) +
  facet_wrap(~ metric, scales = "free_y", ncol = ncol_selected) +
  scale_fill_manual(values = species_colors) +
  scale_alpha_manual(
    name   = "Tide",
    values = tide_alpha,
    guide  = guide_legend(override.aes = list(fill = tide_legend_colour))) +
  guides(fill = "none") +
  labs(
    title    = if (show_title) "Selected Entropy Metrics by Species and Tide" else NULL,
    subtitle = if (show_subtitle) paste0("Site-use diversity across individuals and tide", filter_note(min_tags_tide_plot)) else NULL,
    x        = "Species",
    y        = "Value") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = axis_text_angle, hjust = 1, size = axis_text_size),
    strip.text         = element_text(face = "bold", size = strip_text_size),
    legend.position    = legend_position,
    panel.grid.major.x = element_blank())

plot_entropy_box_tide_selected


## ---- Shannon by Tide — Summary gt Table ----

tbl_entropy_summary_tide <- species_entropy_summary_tide %>%

  gt() %>%

  tab_header(
    title = md("**Table.** Shannon entropy metrics for site-use diversity of shorebird species by tide state in the Hunter estuary.")) %>%
  opt_align_table_header(align = "left") %>%

  tab_footnote(
    footnote = md(
      "**Legend.** Grouped by species and tide state (Low/High). **tideHighLow**:
       tide state during detections; **n_individuals**: tagged individuals;
       **S**: unique sites used; **H**: Shannon entropy; **J**: Pielou's evenness
       (NA for single-site birds); **exp(H)**: effective number of equally-used
       sites; **CV_H**: coefficient of variation in H; **total_detections**:
       total detections.")) %>%
  opt_table_font(font = "Times New Roman") %>%

  cols_label(
    speciesEN        = "Species",
    tideHighLow      = "Tide",
    n_individuals    = "N indiv.",
    median_S         = "Median",
    mean_S           = "Mean",
    sd_S             = "SD",
    median_H         = "Median",
    mean_H           = "Mean",
    sd_H             = "SD",
    CV_H             = "CV (%)",
    median_J         = "Median",
    mean_J           = "Mean",
    sd_J             = "SD",
    median_exp_H     = "Median",
    mean_exp_H       = "Mean",
    sd_exp_H         = "SD",
    total_detections = "Total det.") %>%

  tab_spanner(label = "Sample",                   columns = c(n_individuals, total_detections)) %>%
  tab_spanner(label = "Number of Sites (S)",      columns = c(median_S, mean_S, sd_S)) %>%
  tab_spanner(label = "Shannon Entropy (H)",      columns = c(median_H, mean_H, sd_H, CV_H)) %>%
  tab_spanner(label = "Pielou's Evenness (J)",    columns = c(median_J, mean_J, sd_J)) %>%
  tab_spanner(label = "Effective Sites [exp(H)]", columns = c(median_exp_H, mean_exp_H, sd_exp_H)) %>%

  tab_row_group(label = "Bar-tailed Godwit",     rows = speciesEN == "Bar-tailed Godwit") %>%
  tab_row_group(label = "Curlew Sandpiper",      rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(label = "Eurasian Whimbrel",     rows = speciesEN == "Eurasian Whimbrel") %>%
  tab_row_group(label = "Far Eastern Curlew",    rows = speciesEN == "Far Eastern Curlew") %>%
  tab_row_group(label = "Masked Lapwing",        rows = speciesEN == "Masked Lapwing") %>%
  tab_row_group(label = "Pacific Golden-Plover", rows = speciesEN == "Pacific Golden-Plover") %>%
  tab_row_group(label = "Pied Stilt",            rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(label = "Red-necked Avocet",     rows = speciesEN == "Red-necked Avocet") %>%

  cols_hide(columns = speciesEN) %>%

  tab_style(style = cell_text(weight = "bold"),      locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"),      locations = cells_row_groups()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"), locations = cells_body(columns = everything())) %>%
  tab_style(style = cell_text(whitespace = "nowrap"), locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = cell_fill(color = "#F0F8FF"),    locations = cells_body(rows = tideHighLow == "Low")) %>%
  tab_style(style = cell_fill(color = "#FFF8DC"),    locations = cells_body(rows = tideHighLow == "High")) %>%

  fmt_number(columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H), decimals = 2) %>%
  fmt_number(columns = c(median_S, median_H, median_J, median_exp_H), decimals = 2) %>%
  fmt_number(columns = CV_H,             decimals = 1) %>%
  fmt_number(columns = total_detections, decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size         = px(13),
    heading.title.font.size = px(16),
    data_row.padding        = px(3),
    table.width             = pct(100),
    row_group.font.weight   = "bold")

tbl_entropy_summary_tide
