library(sf)
library(tidyverse)

# Just for east transects (waypoints 11-20)
sightings <- readRDS(here::here("data", "sightings.rds")) %>%
  filter(transect %in% c("1112", "1314", "1516", "1718", "1920"))
waypts <- read_csv(here::here("data", "waypoints.csv")) %>%
  filter(Waypoint >= 11)

sighting_bins <- sightings %>%
  group_by(transect) %>%
  mutate()


