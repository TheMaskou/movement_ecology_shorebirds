source(here::here("R_callum", "globals.R"))


## ----packages,  message = FALSE, warning = FALSE, eval = TRUE, echo = TRUE----
library(dplyr)
library(here)
library(forcats) 
library(lubridate)
library(purrr) 
library(lubridate)
library(ggplot2)
library(stringr)


## ----my data, message = FALSE, warning = FALSE, eval = TRUE, echo = FALSE, results = 'hide', include = FALSE----
# Birds
data_all <- readRDS(path_detection_data)

# Receivers info
recv <- readRDS(path_recv_info)

# Selecting upon our needs
data_all <- data_all %>%
  select(Band.ID, recvDeployName, recv, speciesEN, sig, sigsd, sigPositive, tideCategory, timeAus)


## ----compute variables, echo = TRUE, eval = TRUE------------------------------
# Mean signal strength per individual, site and time categories (HD, HN, LD, LN)
data_all <- data_all %>%
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


## ----plot list, echo = TRUE, eval = TRUE, results = 'asis'--------------------
# One plot per species for better understanding
plots_list <- data_all %>%
  
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



## ----final tabs, echo = FALSE, eval = TRUE, results = 'asis', fig.width = 10, fig.height = 6----
# Create tabs dynamically
for (species_init in names(plots_list)) {
  species_full <- names(species_colors)[match(species_init, names(species_init_colors))]
  cat("###", species_full, "\n\n")
  print(plots_list[[species_init]])
  cat("\n\n")
}

