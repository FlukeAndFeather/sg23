library(tidyverse)

# Just using port side
sightings <- read_csv(dir("data/Sightings/Standardized", full.names = TRUE),
                id = "File") %>%
  drop_na(Spp) %>%
  filter(!Spp %in% c("END", "SFS.", "STS.")) %>%
  mutate(Spp = case_when(
    Spp == "ANPE" ~ "ANPR",
    Spp == "CAP" ~ "CAPT",
    Spp == "KEPN" ~ "KIPN",
    Spp == "SODP" ~ "CODP",
    TRUE ~ Spp
  ))




