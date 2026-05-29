# ==== Signal Strength Variation ====
#
# Preliminary analysis of signal strength as a proxy for movement behaviour.
#
# Large signal strength variation (high SD) within a tide category suggests
# substantial movement within a station's coverage — potentially foraging.
# Small variation (low SD) suggests limited movement — potentially roosting.
#
# Approach:
#   1. For each individual x tide category x station, compute the SD of
#      sigPositive (the positive-shifted RSSI value from ch1_1).
#   2. Boxplot the SD distribution per species, faceted by station, with
#      tide category (DH, DN, LH, LN) on the x-axis.
#
# Requires: globals.R (constants, paths), ch1_1 detection data .rds,
#           receiver info .rds.

source(here::here("qmd", "chapter_1", "R", "globals.R"))


# ==== Packages ====

library(dplyr)
library(here)
library(forcats)
library(lubridate)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)


# ==== Load Data ====

# Birds
df.alltags <- readRDS(path_detection_data)

# Receivers info
recv <- readRDS(path_recv_info)

# Selecting upon our needs
df.alltags <- df.alltags %>%
  select(Band.ID, recvDeployName, recv, speciesEN, sig, sigsd, sigPositive, tideCategory, timeAus)


# ==== Compute Signal Strength Summary Statistics ====
#
# sigPositive is a derivative of raw RSSI (`sig`) shifted to positive values
# (see ch1_1). We compute per-individual SD within each tide category and
# station combination. The SD captures how much the signal fluctuated during
# that condition — a behavioural proxy.
#
# Also derive short-form labels: speciesINIT (e.g. "FEC" for Far Eastern
# Curlew) and tideCat (e.g. "DH" for Diurnal_High) for compact axis labels.

# Mean signal strength per individual, site and time categories (HD, HN, LD, LN)
df.alltags <- df.alltags %>%
  group_by(Band.ID, tideCategory, recvDeployName) %>%
  mutate(sig_indiv_tideCat_mean = mean(sigPositive),
         sig_indiv_tidecat_sd = sd(sigPositive)) %>%
  ungroup()  %>%
  mutate(speciesINIT = str_split(speciesEN, " ") %>%
           map_chr(~ str_c(str_sub(.x, 1, 1), collapse = "")) %>%
           toupper(),
         tideCat = str_split(tideCategory, "_") %>%
           map_chr(~ str_c(str_sub(.x, 1, 1), collapse = "")) %>%
           toupper())


# ==== Signal Strength SD Boxplots ====
#
# One plot per species, faceted by Motus station. Each box shows the
# distribution of individual-level SD values for a given tide category.

# One plot per species for better understanding
plots_list <- df.alltags %>%

  split(.$speciesEN) %>%

  purrr::imap(~ {

    species_full <- .y  # The split name (full species name)

    ggplot(.x, aes(x = tideCat, y = sig_indiv_tidecat_sd, fill = speciesEN)) +
      geom_boxplot() +
      facet_wrap(~ recvDeployName) +

      scale_fill_manual(values = species_colors, guide = "none") +
      theme_bw() +
      labs(title = paste0("Variation of Signal Strength SD for ", species_full, " over Motus stations"),
           x = "Tide Category",
           y = "Variation of Signal Strength SD (RSSI)",
           fill = "Species")
    })

# Rename list elements to initials using the inverse mapping
names(plots_list) <- names(species_init_colors)[match(names(plots_list), names(species_colors))]


## ---- Render Per-Species Tabs ----

# Create tabs dynamically
for (species_init in names(plots_list)) {
  species_full <- names(species_colors)[match(species_init, names(species_init_colors))]
  cat("###", species_full, "\n\n")
  print(plots_list[[species_init]])
  cat("\n\n")
}
