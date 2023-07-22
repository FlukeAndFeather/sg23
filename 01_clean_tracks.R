library(sf)
library(tidyverse)
source(here::here("R", "tracks.R"))

# DLOG3 recorded latitude in reverse i.e. 54 not -54 deg lat
track1920 <- here::here("data", "DLOG3", "PORT", "11JULY23_trk.shp") %>%
  # Read shapefile
  st_read(quiet = TRUE) %>%
  # Make feature simpler
  # Instead of 1000s of 10s segments, 1 simplified feature
  simplify_track(dtol = 0.005) %>%
  # Fix DLOG3 latitude error
  correct_lat() %>%
  # Project to South Georgia Lambert Conformal
  st_transform("epsg:3762")
