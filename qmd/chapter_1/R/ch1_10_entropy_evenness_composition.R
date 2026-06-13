# ==== Entropy, Evenness and Composition ====
#
# Quantifies site-use diversity and inter-individual site-use overlap across
# tagged shorebirds using two complementary information-theoretic metrics:
#
# 1. Shannon entropy (H) — measures how broadly an individual distributes its
#    detections across Motus stations. Addresses: "How spread is a bird's
#    site use?" H is a snapshot diversity/breadth metric; low H (concentrated,
#    predictable use of few sites) serves as a proxy/correlate of
#    high site fidelity. Strict site fidelity is a temporal concept (returning
#    to the same sites over time) — entropy of detection proportions does not
#    by itself encode that return structure, so H indicates but does not directly
#    measure temporal fidelity.
#    Derived metrics:
#      - Pielou's evenness (J = H / ln(S)) — rescales H to [0,1] independent
#        of number of sites: "Are the sites the bird uses visited equally?"
#      - Effective number of sites (exp(H)) — interpretable count of
#        equally-used sites.
#      - Coefficient of variation (CV) — between-individual heterogeneity in H
#        within a species (descriptive; no inferential test — each bird has
#        one H value so within-species testing requires temporal replication).
#
# 2. Jensen-Shannon Divergence (JSD) — pairwise comparison of site-use
#    distributions between individuals of the same species. Addresses:
#    "Do conspecifics use the *same* sites?" JSD ~ 0 = identical site use,
#    JSD ~ 1 = completely different site use.
#
# Both metrics are computed globally and split by tide (High/Low) to test
# whether site-use diversity or composition changes with tidal condition.
#
# Statistical tests:
#   - Kruskal-Wallis + Dunn (Bonferroni): between-species differences in
#     H / J / exp(H) / S  (individuals as replicates — valid)
#   - Paired Wilcoxon signed-rank: tide effect on entropy metrics per species
#     (paired on individual — valid for one High/Low value per bird)
#   - PERMANOVA (adonis2): between-species differences in JSD site-use
#     composition (permutation-based — handles non-independence of pairwise
#     distances)
#   - Pairwise PERMANOVA: species-pair comparisons, p adjusted with BH
#   - PERMDISP (betadisper): between-species differences in within-species
#     dispersion (spread of conspecific site use)
#   - Per-species PERMANOVA + PERMDISP by tide: permutations stratified within
#     Band.ID to account for the paired individual structure
#
# Requires: globals.R (constants, paths), ch1_1 detection data .rds,
#           receiver info .rds.
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

library(motus)
library(dplyr)
library(here)
library(ggplot2)
library(tidyr)
library(readr)
library(vegan)
library(philentropy)
library(gt)
library(FSA)
library(patchwork)


# ==== Load Data ====

# Birds
df.alltags <- readRDS(path_detection_data)

# Receivers info
recv <- readRDS(path_recv_info)


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                     PART 1 — SHANNON ENTROPY                            ║
# ╚════════════════════════════════════════════════════════════════════════════╝

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


# ==== Shannon Entropy — Species Summary Table ====

species_entropy_summary <- entropy_results %>%
  group_by(speciesEN) %>%
  summarise(

    n_individuals = n(),

    median_S = median(S,     na.rm = TRUE),
    mean_S   = round(mean(S, na.rm = TRUE), 2),
    sd_S     = round(sd(S,   na.rm = TRUE), 2),

    median_H = median(H,     na.rm = TRUE),
    mean_H   = round(mean(H, na.rm = TRUE), 2),
    sd_H     = round(sd(H,   na.rm = TRUE), 2),
    CV_H     = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),

    median_J = median(J,     na.rm = TRUE),
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

  tab_spanner(label = "Sample",                  columns = c(n_individuals, total_detections)) %>%
  tab_spanner(label = "Number of Sites (S)",     columns = c(median_S, mean_S, sd_S)) %>%
  tab_spanner(label = "Shannon Entropy (H)",     columns = c(median_H, mean_H, sd_H, CV_H)) %>%
  tab_spanner(label = "Pielou's Evenness (J)",   columns = c(median_J, mean_J, sd_J)) %>%
  tab_spanner(label = "Effective Sites [exp(H)]",columns = c(median_exp_H, mean_exp_H, sd_exp_H)) %>%

  tab_style(style = cell_text(weight = "bold"),     locations = cells_column_labels()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"),locations = cells_body(columns = everything())) %>%
  tab_style(style = cell_text(whitespace = "nowrap"),locations = cells_column_labels(columns = everything())) %>%

  fmt_number(columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H), decimals = 2) %>%
  fmt_number(columns = c(median_S, median_H, median_J, median_exp_H), decimals = 2) %>%
  fmt_number(columns = CV_H,             decimals = 1) %>%
  fmt_number(columns = total_detections, decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size            = px(14),
    heading.title.font.size    = px(16),
    data_row.padding           = px(3),
    table.width                = pct(100))

tbl_entropy_summary


## ---- Shannon Entropy Boxplots ----

entropy_for_plot <- entropy_results %>%
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
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = "Shannon Entropy Metrics by Species",
    subtitle = "Distribution of site-use diversity across individuals",
    x        = "Species",
    y        = "Value",
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_entropy_box


## ---- Shannon Entropy Individual-Level Tables ----

gt_S <- entropy_results %>%
  select(speciesEN, Band.ID, S) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**S - Number of Sites Used**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", S = "S") %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

gt_H <- entropy_results %>%
  select(speciesEN, Band.ID, H) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**H - Shannon Entropy**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", H = "H") %>%
  fmt_number(columns = H, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

gt_J <- entropy_results %>%
  select(speciesEN, Band.ID, J) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**J - Pielou's Evenness**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", J = "J") %>%
  fmt_number(columns = J, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

gt_exp_H <- entropy_results %>%
  select(speciesEN, Band.ID, exp_H) %>%
  arrange(speciesEN, Band.ID) %>%
  gt() %>%
  tab_header(title = md("**exp(H) - Effective Number of Sites**")) %>%
  cols_label(speciesEN = "Species", Band.ID = "Band ID", exp_H = "exp(H)") %>%
  fmt_number(columns = exp_H, decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels())

gt_S
gt_H
gt_J
gt_exp_H


# ==== Shannon Entropy — Between-Species Kruskal-Wallis ====
#
# Tests whether H, J, exp(H), and S differ BETWEEN species (individuals as
# replicates within each species). Valid: one observation per individual.
# Only species with >= 3 individuals are included.
# If significant, Dunn post-hoc with Bonferroni correction identifies which
# species pairs differ.
#
# NOTE: Between-INDIVIDUAL variation within a species is described
# descriptively via CV_H, SD, and the boxplots above. It cannot be tested
# inferentially here because each bird yields only one H value (no within-
# individual replication). Temporal replication would be required for that.

entropy_kw_data <- entropy_results %>%
  group_by(speciesEN) %>%
  filter(n() >= 3) %>%
  ungroup()

kw_between_species <- data.frame()
for (m in c("H", "J", "exp_H", "S")) {
  vals  <- entropy_kw_data[[m]]
  grp   <- entropy_kw_data$speciesEN
  valid <- !is.na(vals)

  # Require >= 2 non-NA observations per species
  n_per_sp <- tapply(vals[valid], grp[valid], length)
  if (any(n_per_sp < 2)) {
    message("Skipping KW for ", m, ": some species have < 2 non-NA values")
    next
  }

  kt <- kruskal.test(vals[valid] ~ grp[valid])

  kw_between_species <- rbind(kw_between_species, data.frame(
    Metric       = m,
    n_species    = n_distinct(grp[valid]),
    n_indiv      = sum(valid),
    Chi_sq       = round(kt$statistic, 3),
    df           = kt$parameter,
    p_value      = kt$p.value,
    Significance = case_when(
      kt$p.value < 0.001 ~ "***",
      kt$p.value < 0.01  ~ "**",
      kt$p.value < 0.05  ~ "*",
      TRUE               ~ "ns"),
    stringsAsFactors = FALSE))
}


tbl_kw_between <- kw_between_species %>%
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


## ---- Shannon Between-Species Dunn Post-Hoc ----

dunn_between_list <- list()
for (m in c("H", "J", "exp_H", "S")) {
  kw_row <- kw_between_species[kw_between_species$Metric == m, ]
  if (nrow(kw_row) == 0 || kw_row$p_value >= 0.05) next

  vals  <- entropy_kw_data[[m]]
  grp   <- entropy_kw_data$speciesEN
  valid <- !is.na(vals)

  d <- dunnTest(vals[valid] ~ grp[valid], method = "bonferroni")
  dunn_between_list[[m]] <- d$res %>%
    mutate(
      Metric       = m,
      Significance = case_when(
        P.adj < 0.001 ~ "***",
        P.adj < 0.01  ~ "**",
        P.adj < 0.05  ~ "*",
        TRUE          ~ "ns")) %>%
    select(Metric, Comparison, Z, P.unadj, P.adj, Significance)
}

dunn_between_df <- bind_rows(dunn_between_list)

if (nrow(dunn_between_df) > 0) {
  tbl_dunn_between <- dunn_between_df %>%
    arrange(Metric, Comparison) %>%
    gt(groupname_col = "Metric") %>%
    tab_header(
      title    = md("**Dunn Post-Hoc Test — Between-Species (Bonferroni)**"),
      subtitle = "Pairwise species comparisons for significant entropy metrics") %>%
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
      row_group.font.weight      = "bold",
      row_group.background.color = "#f0f4f8",
      table.width                = pct(100))

  tbl_dunn_between
}


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


## ---- Shannon by Tide — Species Summary and Boxplot ----

species_entropy_summary_tide <- entropy_results_tide %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(
    n_individuals = n(),

    median_S = median(S,     na.rm = TRUE),
    mean_S   = round(mean(S, na.rm = TRUE), 2),
    sd_S     = round(sd(S,   na.rm = TRUE), 2),

    median_H = median(H,     na.rm = TRUE),
    mean_H   = round(mean(H, na.rm = TRUE), 2),
    sd_H     = round(sd(H,   na.rm = TRUE), 2),
    CV_H     = round((sd(H, na.rm = TRUE) / mean(H, na.rm = TRUE)) * 100, 1),

    median_J = median(J,     na.rm = TRUE),
    mean_J   = round(mean(J, na.rm = TRUE), 2),
    sd_J     = round(sd(J,   na.rm = TRUE), 2),

    median_exp_H = round(median(exp_H, na.rm = TRUE), 2),
    mean_exp_H   = round(mean(exp_H,   na.rm = TRUE), 2),
    sd_exp_H     = round(sd(exp_H,     na.rm = TRUE), 2),

    total_detections = sum(total_detections, na.rm = TRUE),

    .groups = "drop") %>%
  arrange(speciesEN)


entropy_for_plot_tide <- entropy_results_tide %>%
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
               position = position_dodge(width = 0.8),
               outlier.shape = 16) +
  geom_point(aes(group = interaction(speciesEN, tideHighLow)),
             position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
             size = 1.1) +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = species_colors) +
  scale_alpha_manual(values = c("High" = 1.0, "Low" = 0.2)) +
  labs(
    title    = "Shannon Entropy Metrics by Species and Tide",
    subtitle = "Site-use diversity across individuals and tide (High tide: solid, Low tide: transparent)",
    x        = "Species",
    y        = "Value",
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_entropy_box_tide


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
       total detections. Tide significance tested below by paired Wilcoxon.")) %>%
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

  tab_spanner(label = "Sample",                  columns = c(n_individuals, total_detections)) %>%
  tab_spanner(label = "Number of Sites (S)",     columns = c(median_S, mean_S, sd_S)) %>%
  tab_spanner(label = "Shannon Entropy (H)",     columns = c(median_H, mean_H, sd_H, CV_H)) %>%
  tab_spanner(label = "Pielou's Evenness (J)",   columns = c(median_J, mean_J, sd_J)) %>%
  tab_spanner(label = "Effective Sites [exp(H)]",columns = c(median_exp_H, mean_exp_H, sd_exp_H)) %>%

  tab_row_group(label = "Bar-tailed Godwit",    rows = speciesEN == "Bar-tailed Godwit") %>%
  tab_row_group(label = "Curlew Sandpiper",     rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(label = "Eurasian Whimbrel",    rows = speciesEN == "Eurasian Whimbrel") %>%
  tab_row_group(label = "Far Eastern Curlew",   rows = speciesEN == "Far Eastern Curlew") %>%
  tab_row_group(label = "Masked Lapwing",       rows = speciesEN == "Masked Lapwing") %>%
  tab_row_group(label = "Pacific Golden-Plover",rows = speciesEN == "Pacific Golden-Plover") %>%
  tab_row_group(label = "Pied Stilt",           rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(label = "Red-necked Avocet",    rows = speciesEN == "Red-necked Avocet") %>%

  cols_hide(columns = speciesEN) %>%

  tab_style(style = cell_text(weight = "bold"),     locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"),     locations = cells_row_groups()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"),locations = cells_body(columns = everything())) %>%
  tab_style(style = cell_text(whitespace = "nowrap"),locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = cell_fill(color = "#F0F8FF"),   locations = cells_body(rows = tideHighLow == "Low")) %>%
  tab_style(style = cell_fill(color = "#FFF8DC"),   locations = cells_body(rows = tideHighLow == "High")) %>%

  fmt_number(columns = c(mean_S, sd_S, mean_H, sd_H, mean_J, sd_J, mean_exp_H, sd_exp_H), decimals = 2) %>%
  fmt_number(columns = c(median_S, median_H, median_J, median_exp_H), decimals = 2) %>%
  fmt_number(columns = CV_H,             decimals = 1) %>%
  fmt_number(columns = total_detections, decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size            = px(13),
    heading.title.font.size    = px(16),
    data_row.padding           = px(3),
    table.width                = pct(100),
    row_group.font.weight      = "bold")

tbl_entropy_summary_tide


# ==== Shannon by Tide — Paired Wilcoxon Signed-Rank ====
#
# Tests whether tide condition (High vs Low) shifts entropy metrics per
# species. Each bird provides one High-tide and one Low-tide value
# → paired design; valid with one paired observation per individual.
#
# Only birds with BOTH High and Low tide observations are included
# (complete pairs). Species with >= 5 complete pairs are tested.
# exact = FALSE used to handle ties.

entropy_tide_wide <- entropy_results_tide %>%
  pivot_wider(
    id_cols     = c(speciesEN, Band.ID),
    names_from  = tideHighLow,
    values_from = c(H, J, exp_H, S),
    names_glue  = "{.value}_{tideHighLow}") %>%
  filter(!is.na(H_High), !is.na(H_Low))   # retain only complete pairs

wilcox_tide_results <- data.frame()

for (sp in unique(entropy_tide_wide$speciesEN)) {
  df_sp <- entropy_tide_wide %>% filter(speciesEN == sp)
  if (nrow(df_sp) < 5) next   # minimum 5 complete pairs (mirrors previous threshold)

  for (metric in c("H", "J", "exp_H", "S")) {
    high_vals <- df_sp[[paste0(metric, "_High")]]
    low_vals  <- df_sp[[paste0(metric, "_Low")]]

    # Only use pairs where both values are non-NA (e.g. J=NA when S=1)
    valid     <- !is.na(high_vals) & !is.na(low_vals)
    high_vals <- high_vals[valid]
    low_vals  <- low_vals[valid]

    if (length(high_vals) < 3) next   # need >= 3 pairs for any power

    wt <- wilcox.test(high_vals, low_vals, paired = TRUE, exact = FALSE)

    wilcox_tide_results <- rbind(wilcox_tide_results, data.frame(
      Species = sp,
      Metric  = metric,
      N_pairs = sum(valid),
      W       = round(wt$statistic, 3),
      p_value = round(wt$p.value,   4),
      Sig     = case_when(
        wt$p.value < 0.001 ~ "***",
        wt$p.value < 0.01  ~ "**",
        wt$p.value < 0.05  ~ "*",
        wt$p.value < 0.1   ~ ".",
        TRUE               ~ "ns"),
      stringsAsFactors = FALSE))
  }
}


tbl_wilcox_tide <- wilcox_tide_results %>%
  gt(groupname_col = "Species") %>%
  tab_header(
    title    = md("**Paired Wilcoxon Test — Tide Effect on Entropy Metrics**"),
    subtitle = "High tide vs Low tide; paired within individual") %>%
  cols_label(
    Metric  = "Metric",
    N_pairs = "N pairs",
    W       = "W",
    p_value = "p-value",
    Sig     = "Sig.") %>%
  fmt_number(columns = W,       decimals = 3) %>%
  fmt_scientific(columns = p_value, decimals = 3) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = Sig, rows = p_value < 0.05)) %>%
  tab_style(
    style     = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = p_value < 0.05)) %>%
  tab_source_note(
    source_note = md(
      "**Paired Wilcoxon signed-rank test** (exact = FALSE for ties).
       Only birds with both High and Low tide observations included (complete pairs).
       Only species with ≥ 5 complete pairs shown. J excluded for birds where S = 1.
       **Significance:** *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, ns ≥ 0.1")) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>%
  opt_table_font(font = "Times New Roman") %>%
  tab_options(
    table.font.size            = px(13),
    heading.title.font.size    = px(16),
    data_row.padding           = px(4),
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    table.width                = pct(100))

tbl_wilcox_tide


# ╔════════════════════════════════════════════════════════════════════════════╗
# ║                 PART 2 — JENSEN-SHANNON DIVERGENCE                      ║
# ╚════════════════════════════════════════════════════════════════════════════╝
#
# Shannon entropy tells us how broadly each individual uses sites, but not
# whether conspecifics use the *same* sites. Jensen-Shannon Divergence (JSD)
# compares the site-use probability distributions of two individuals:
#   JSD ~ 0 → identical site-use patterns
#   JSD ~ 1 → completely different site-use patterns
#
# Inferential tests use PERMANOVA (adonis2) and PERMDISP (betadisper) which
# are permutation-based and valid for non-independent pairwise distance data.
# Parametric tests (ANOVA, Kruskal-Wallis) on pairwise JSD values are invalid
# because each individual contributes to many pairs (pseudoreplication).

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
n_individuals  <- length(individual_ids)

jsd_results <- data.frame()

for (i in 1:(n_individuals - 1)) {
  for (j in (i + 1):n_individuals) {
    p <- as.numeric(site_use_matrix[i, -1])
    q <- as.numeric(site_use_matrix[j, -1])
    jsd_results <- rbind(jsd_results, data.frame(
      Band.ID_1  = individual_ids[i],
      Band.ID_2  = individual_ids[j],
      JSD        = round(calculate_jsd(p, q), 3),
      Similarity = round(1 - calculate_jsd(p, q), 3)))
  }
}

# Add species information
jsd_results <- jsd_results %>%
  left_join(df.alltags %>%
              mutate(Band.ID = as.character(Band.ID)) %>%
              select(Band.ID, speciesEN) %>% distinct(),
            by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(df.alltags %>%
              mutate(Band.ID = as.character(Band.ID)) %>%
              select(Band.ID, speciesEN) %>% distinct(),
            by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  mutate(same_species = species_1 == species_2)


## ---- JSD Boxplot ----

jsd_for_plot <- jsd_results %>%
  filter(same_species == TRUE) %>%
  mutate(metric = "JSD", speciesEN = species_1) %>%
  select(speciesEN, JSD, metric) %>%
  rename(value = JSD)

plot_jsd_box <- ggplot(jsd_for_plot, aes(x = speciesEN, y = value, fill = speciesEN)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = species_colors) +
  labs(
    title    = "Jensen-Shannon Divergence by Species",
    subtitle = "Pairwise inter-individual site-use similarity within species",
    x        = "Species",
    y        = "JSD Value",
    fill     = "Species") +
  theme_minimal() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank())

plot_jsd_box


## ---- JSD Summary Table ----

jsd_results_tab <- jsd_results %>%
  filter(same_species == TRUE) %>%
  group_by(species_1) %>%
  summarise(
    n_comparisons    = n(),
    mean_JSD         = round(mean(JSD),        3),
    sd_JSD           = round(sd(JSD),          3),
    mean_Similarity  = round(mean(Similarity), 3),
    sd_Similarity    = round(sd(Similarity),   3),
    .groups = "drop") %>%
  rename(speciesEN = species_1)

n_indiv_jsd <- df.alltags %>%
  group_by(speciesEN) %>%
  summarise(n_indiv = n_distinct(Band.ID), .groups = "drop")

jsd_results_tab <- jsd_results_tab %>%
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
       **JSD ≈ 1** → low similarity (conspecifics use different sites).
       Inferential testing via PERMANOVA / PERMDISP below.")) %>%
  tab_options(
    row_group.font.weight      = "bold",
    row_group.background.color = "#f0f4f8",
    heading.align              = "left",
    table.font.size            = 13) %>%
  opt_row_striping()

tbl_jsd_summary


# ==== JSD — Distance Matrix ====
#
# Reconstruct the full n×n JSD distance matrix from jsd_results for use in
# PERMANOVA and PERMDISP. Symmetric; 0 on diagonal.

jsd_mat_full <- matrix(0,
                       nrow     = n_individuals,
                       ncol     = n_individuals,
                       dimnames = list(individual_ids, individual_ids))

for (row_i in seq_len(nrow(jsd_results))) {
  id1 <- as.character(jsd_results$Band.ID_1[row_i])
  id2 <- as.character(jsd_results$Band.ID_2[row_i])
  jsd_mat_full[id1, id2] <- jsd_results$JSD[row_i]
  jsd_mat_full[id2, id1] <- jsd_results$JSD[row_i]
}

jsd_dist_full <- as.dist(jsd_mat_full)

# Species metadata in the same order as individual_ids
species_for_dist <- data.frame(Band.ID = individual_ids, stringsAsFactors = FALSE) %>%
  left_join(
    df.alltags %>%
      mutate(Band.ID = as.character(Band.ID)) %>%
      select(Band.ID, speciesEN) %>%
      distinct(),
    by = "Band.ID")


# ==== JSD — PERMANOVA Across Species ====
#
# Tests whether site-use composition (as captured by JSD) differs between
# species. PERMANOVA (adonis2) is permutation-based and therefore valid for
# distance matrices where pairwise values are non-independent.

set.seed(123)
permanova_jsd_species <- adonis2(
  jsd_dist_full ~ speciesEN,
  data         = species_for_dist,
  permutations = 999)

permanova_jsd_species_df <- as.data.frame(permanova_jsd_species) %>%
  tibble::rownames_to_column("Term") %>%
  filter(Term != "Total") %>%
  mutate(
    R2      = round(R2,         3),
    F       = round(F,          3),
    p_value = `Pr(>F)`,
    Sig     = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE            ~ ""))

tbl_permanova_species <- permanova_jsd_species_df %>%
  gt(rowname_col = "Term") %>%
  tab_header(
    title    = md("**PERMANOVA — JSD Across Species**"),
    subtitle = md("*adonis2(JSD ~ speciesEN, permutations = 999)*")) %>%
  cols_label(
    Df        = "df",
    SumOfSqs  = "Sum of Sqs",
    R2        = "R²",
    F         = "F",
    `Pr(>F)`  = "p-value",
    Sig       = "") %>%
  fmt_number(columns = c(SumOfSqs, R2, F), decimals = 3) %>%
  fmt_scientific(columns = `Pr(>F)`, decimals = 3) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(columns = c(`Pr(>F)`, Sig),
                           rows = !is.na(p_value) & p_value < 0.05)) %>%
  tab_source_note(
    source_note = "Significance: *** p<0.001  ** p<0.01  * p<0.05  . p<0.1") %>%
  tab_options(heading.align = "left", table.font.size = 13)

tbl_permanova_species


## ---- JSD Pairwise PERMANOVA ----
#
# Pairwise species comparisons; p-values adjusted with Benjamini-Hochberg (BH)
# correction for multiple comparisons. Bonferroni available as an alternative
# via p.adjust(method = "bonferroni").

species_unique     <- unique(species_for_dist$speciesEN)
species_pairs_mat  <- combn(species_unique, 2)
pairwise_permanova <- data.frame()

for (k in seq_len(ncol(species_pairs_mat))) {
  sp1 <- species_pairs_mat[1, k]
  sp2 <- species_pairs_mat[2, k]

  idx      <- which(species_for_dist$speciesEN %in% c(sp1, sp2))
  sub_dist <- as.dist(as.matrix(jsd_dist_full)[idx, idx])
  sub_df   <- species_for_dist[idx, , drop = FALSE]

  if (n_distinct(sub_df$speciesEN) < 2) next

  set.seed(123)
  ad <- adonis2(sub_dist ~ speciesEN, data = sub_df, permutations = 999)

  pairwise_permanova <- rbind(pairwise_permanova, data.frame(
    Species_1 = sp1,
    Species_2 = sp2,
    F_value   = round(ad["speciesEN", "F"],       3),
    R2        = round(ad["speciesEN", "R2"],       3),
    p_value   = round(ad["speciesEN", "Pr(>F)"],  4),
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
    F_value   = "F",
    R2        = "R²",
    p_value   = "p unadj",
    p_adj     = "p adj (BH)",
    Sig       = "Sig.") %>%
  fmt_number(columns = c(F_value, R2), decimals = 3) %>%
  fmt_scientific(columns = c(p_value, p_adj), decimals = 3) %>%
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
# differs between species. Complements PERMANOVA (which tests centroid location).
# High dispersion in a species = conspecifics use very different sites.

betadisp_jsd_species <- betadisper(jsd_dist_full, group = species_for_dist$speciesEN)

set.seed(123)
permutest_betadisp_species <- permutest(betadisp_jsd_species, permutations = 999)

# permutest.betadisper$tab column order: Df(1), SS(2), MeanSqs(3), F(4), N.Perm(5), Pr(>F)(6)
# Column *names* vary across vegan versions so access by position except for Pr(>F)
.perm_tab_sp  <- permutest_betadisp_species$tab
.pval_col_sp  <- grep("Pr\\(", colnames(.perm_tab_sp), value = TRUE)[1]

betadisp_summary_df <- data.frame(
  Term    = rownames(.perm_tab_sp),
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
    Df      = "df",
    SS      = "SS",
    MeanSqs = "Mean Sq",
    F       = "F",
    p_value = "p-value",
    Sig     = "") %>%
  fmt_number(columns = c(SS, MeanSqs, F), decimals = 3) %>%
  fmt_scientific(columns = p_value, decimals = 3) %>%
  tab_source_note(
    source_note = md(
      "PERMDISP tests homogeneity of multivariate dispersion (within-group spread
       in JSD space). Significant result = species differ in how variable
       conspecific site use is — *not* in where they forage (that is PERMANOVA).
       **Significance:** *** p<0.001  ** p<0.01  * p<0.05  . p<0.1")) %>%
  tab_options(heading.align = "left", table.font.size = 13)

tbl_permdisp_species


# ==== JSD — By Tide Pairwise Computation ====
#
# Computes within-tide pairwise JSD (High-High and Low-Low pairs only) to
# describe how similar conspecifics are within each tidal state.
# Only species with >= 5 *individuals* (n_distinct(Band.ID)) are included.
# Full-precision proportions used (no rounding).

site_use_matrix_tide <- df.alltags %>%
  filter(!is.na(Band.ID), !is.na(recvDeployName)) %>%

  # Filter to species with >= 5 tagged individuals (not raw detections)
  group_by(speciesEN) %>%
  filter(n_distinct(Band.ID) >= 5) %>%
  ungroup() %>%

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

individual_tide_ids <- site_use_matrix_tide$id_tide
n_individuals_tide  <- length(individual_tide_ids)

jsd_results_tide <- data.frame()

for (i in 1:(n_individuals_tide - 1)) {
  for (j in (i + 1):n_individuals_tide) {

    # Only compare within the same tide state
    if (site_use_matrix_tide$tideHighLow[i] == site_use_matrix_tide$tideHighLow[j]) {
      p <- as.numeric(site_use_matrix_tide[i, -c(1:3)])
      q <- as.numeric(site_use_matrix_tide[j, -c(1:3)])

      jsd_results_tide <- rbind(jsd_results_tide, data.frame(
        Band.ID_1   = site_use_matrix_tide$Band.ID[i],
        Band.ID_2   = site_use_matrix_tide$Band.ID[j],
        tideHighLow = site_use_matrix_tide$tideHighLow[i],
        JSD         = round(calculate_jsd(p, q), 3),
        Similarity  = round(1 - calculate_jsd(p, q), 3)))
    }
  }
}

jsd_results_tide <- jsd_results_tide %>%
  left_join(df.alltags %>%
              mutate(Band.ID = as.character(Band.ID)) %>%
              select(Band.ID, speciesEN) %>% distinct(),
            by = c("Band.ID_1" = "Band.ID")) %>%
  rename(species_1 = speciesEN) %>%
  left_join(df.alltags %>%
              mutate(Band.ID = as.character(Band.ID)) %>%
              select(Band.ID, speciesEN) %>% distinct(),
            by = c("Band.ID_2" = "Band.ID")) %>%
  rename(species_2 = speciesEN) %>%
  mutate(same_species = species_1 == species_2) %>%
  filter(same_species == TRUE)


## ---- JSD by Tide — Summary Table ----

jsd_results_tab_tide <- jsd_results_tide %>%
  filter(same_species == TRUE) %>%
  group_by(species_1, tideHighLow) %>%
  summarise(
    n_comparisons     = n(),
    mean_JSD          = round(mean(JSD),           3),
    median_JSD        = round(median(JSD),         3),
    sd_JSD            = round(sd(JSD),             3),
    mean_Similarity   = round(mean(Similarity),    3),
    median_Similarity = round(median(Similarity),  3),
    sd_Similarity     = round(sd(Similarity),      3),
    .groups = "drop") %>%
  rename(speciesEN = species_1)

n_indiv_tide <- df.alltags %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_indiv  = n_distinct(Band.ID), .groups = "drop")
n_detect_tide <- df.alltags %>%
  group_by(speciesEN, tideHighLow) %>%
  summarise(n_detect = n(), .groups = "drop")

jsd_results_tab_tide <- jsd_results_tab_tide %>%
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
       tide state. Tide effect tested inferentially via PERMANOVA/PERMDISP below.
       Low tide rows blue, High tide rows yellow.")) %>%
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

  tab_row_group(label = "Bar-tailed Godwit",    rows = speciesEN == "Bar-tailed Godwit") %>%
  tab_row_group(label = "Pied Stilt",           rows = speciesEN == "Pied Stilt") %>%
  tab_row_group(label = "Pacific Golden-Plover",rows = speciesEN == "Pacific Golden-Plover") %>%
  tab_row_group(label = "Red-necked Avocet",    rows = speciesEN == "Red-necked Avocet") %>%
  tab_row_group(label = "Curlew Sandpiper",     rows = speciesEN == "Curlew Sandpiper") %>%
  tab_row_group(label = "Eurasian Whimbrel",    rows = speciesEN == "Eurasian Whimbrel") %>%

  cols_hide(columns = speciesEN) %>%

  tab_style(style = cell_text(weight = "bold"),     locations = cells_column_labels()) %>%
  tab_style(style = cell_text(weight = "bold"),     locations = cells_row_groups()) %>%
  tab_style(style = cell_text(whitespace = "nowrap"),locations = cells_body()) %>%
  tab_style(style = cell_fill(color = "#F0F8FF"),   locations = cells_body(rows = tideHighLow == "Low")) %>%
  tab_style(style = cell_fill(color = "#FFF8DC"),   locations = cells_body(rows = tideHighLow == "High")) %>%

  fmt_number(columns = c(mean_JSD, median_JSD, sd_JSD,
                          mean_Similarity, median_Similarity, sd_Similarity), decimals = 3) %>%
  fmt_number(columns = c(n_indiv, n_detect, n_comparisons), decimals = 0, use_seps = TRUE) %>%

  tab_options(
    table.font.size            = px(13),
    heading.title.font.size    = px(16),
    data_row.padding           = px(3),
    table.width                = pct(100),
    row_group.font.weight      = "bold")

tbl_jsd_summary_tide


## ---- JSD by Tide — Boxplot ----

jsd_for_plot_tide <- jsd_results_tide %>%
  pivot_longer(
    cols      = c(JSD, Similarity),
    names_to  = "metric",
    values_to = "value")

species_list_jsd <- unique(jsd_for_plot_tide$species_1)

plot_jsd_box_tide <- jsd_for_plot_tide %>%
  filter(metric == "JSD") %>%
  ggplot(aes(x     = species_1,
             y     = value,
             fill  = species_1,
             alpha = tideHighLow,
             group = interaction(species_1, tideHighLow))) +

  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.shape = 16, outlier.size = 1.5) +

  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 1.1) +

  scale_fill_manual(values  = species_colors) +
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

plot_jsd_box_tide


# ==== JSD by Tide — PERMANOVA and PERMDISP ====
#
# Tests whether tide condition shifts site-use composition within each species.
# Approach: for each species, compute a full JSD distance matrix between all
# Bird×Tide observations (including cross-tide pairs), then run:
#   - adonis2(dist ~ tideHighLow) with permutations stratified within Band.ID
#     (accounts for the paired/within-individual structure)
#   - betadisper + permutest to check if dispersion differs by tide
#
# This is the valid replacement for the ANOVA on pairwise JSD values
# (which had pseudoreplication: each bird appeared in many pairs).

# Proportion columns in site_use_matrix_tide (exclude id/grouping cols)
tide_prop_colnames <- setdiff(names(site_use_matrix_tide),
                              c("id_tide", "Band.ID", "tideHighLow"))

# Add species to the tide metadata
site_use_tide_meta <- site_use_matrix_tide %>%
  select(id_tide, Band.ID, tideHighLow) %>%
  mutate(Band.ID = as.character(Band.ID)) %>%
  left_join(
    df.alltags %>%
      mutate(Band.ID = as.character(Band.ID)) %>%
      select(Band.ID, speciesEN) %>%
      distinct(),
    by = "Band.ID")

results_jsd_tide_permanova <- data.frame()
results_jsd_tide_betadisp  <- data.frame()

for (sp in unique(site_use_tide_meta$speciesEN)) {

  sp_idx  <- which(site_use_tide_meta$speciesEN == sp)
  sp_meta <- site_use_tide_meta[sp_idx, ]
  sp_prop <- as.matrix(site_use_matrix_tide[sp_idx, tide_prop_colnames])

  n_sp    <- nrow(sp_prop)
  n_tides <- n_distinct(sp_meta$tideHighLow)

  if (n_sp < 4 || n_tides < 2) {
    message("Skipping ", sp, ": < 4 obs or < 2 tide levels for tide PERMANOVA")
    next
  }

  # Full JSD distance matrix for this species (all cross-tide pairs included)
  sp_jsd_mat <- matrix(0, n_sp, n_sp)
  for (i in 1:(n_sp - 1)) {
    for (j in (i + 1):n_sp) {
      v <- calculate_jsd(as.numeric(sp_prop[i, ]), as.numeric(sp_prop[j, ]))
      sp_jsd_mat[i, j] <- v
      sp_jsd_mat[j, i] <- v
    }
  }
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
    perm_bd      <- permutest(bd_sp, permutations = 999)
    .pval_col_t  <- grep("Pr\\(", colnames(perm_bd$tab), value = TRUE)[1]
    results_jsd_tide_betadisp <- rbind(results_jsd_tide_betadisp, data.frame(
      Species = sp,
      F       = round(perm_bd$tab["Groups", 4],            3),
      Df      = perm_bd$tab["Groups", 1],
      SS      = round(perm_bd$tab["Groups", 2],            4),
      p_value = round(perm_bd$tab["Groups", .pval_col_t],  4),
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


if (nrow(results_jsd_tide_permanova) > 0) {
  tbl_jsd_permanova_tide <- results_jsd_tide_permanova %>%
    gt(groupname_col = "Species") %>%
    tab_header(
      title    = md("**PERMANOVA — Tide Effect on JSD Composition (per species)**"),
      subtitle = md("*adonis2(JSD ~ tideHighLow), permutations stratified within Band.ID*")) %>%
    cols_label(
      Df      = "df",
      SS      = "Sum of Sqs",
      R2      = "R²",
      F       = "F",
      p_value = "p-value",
      Sig     = "") %>%
    fmt_number(columns = c(SS, R2, F), decimals = 3) %>%
    fmt_scientific(columns = p_value, decimals = 3) %>%
    tab_style(
      style     = cell_text(color = "#c0392b", weight = "bold"),
      locations = cells_body(columns = c(p_value, Sig),
                             rows = p_value < 0.05)) %>%
    tab_source_note(
      source_note = md(
        "Permutations stratified within Band.ID to account for within-individual
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


if (nrow(results_jsd_tide_betadisp) > 0) {
  tbl_jsd_permdisp_tide <- results_jsd_tide_betadisp %>%
    gt(groupname_col = "Species") %>%
    tab_header(
      title    = md("**PERMDISP — Tide Effect on Within-Group JSD Dispersion (per species)**"),
      subtitle = md("*betadisper + permutest, permutations = 999*")) %>%
    cols_label(
      F       = "F",
      Df      = "df",
      SS      = "SS",
      p_value = "p-value",
      Sig     = "") %>%
    fmt_number(columns = c(F, SS), decimals = 3) %>%
    fmt_scientific(columns = p_value, decimals = 3) %>%
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
