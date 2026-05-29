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