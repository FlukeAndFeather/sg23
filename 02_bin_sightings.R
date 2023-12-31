library(sf)
library(tidyverse)
library(tmap)
source(here::here("R", "tracks.R"))

valid_transects <- sprintf("%02d%02d", seq(1, 19, 2), seq(2, 20, 2))
sightings <- readRDS(here::here("data", "sightings.rds")) %>%
  filter(transect %in% valid_transects)
intervals <- readRDS(here::here("data", "intervals.rds")) %>%
  filter(transect %in% valid_transects)

# Bin sightings, retaining empty intervals
binned_sightings <- sightings %>%
  assign_intervals(intervals) %>%
  group_by(transect, interval, species) %>%
  summarize(count = sum(count), .groups = "drop") %>%
  right_join(intervals, by = c("transect", "interval")) %>%
  st_sf()

# Save
saveRDS(binned_sightings, here::here("data", "sightingsbins.rds"))

# Diagnostic map of most common species w/in 10 nmi bins
recode_interval <- function(i, sz) floor((i - 1) / sz) + 1
interval_locs <- intervals %>%
  mutate(interval = recode_interval(interval, 10)) %>%
  group_by(transect, interval) %>%
  summarize(.groups = "drop") %>%
  st_centroid()
most_common <- binned_sightings %>%
  as.data.frame() %>%
  mutate(interval = recode_interval(interval, 10)) %>%
  drop_na(species) %>%
  group_by(transect, interval, sp = species) %>%
  summarize(count = sum(count, na.rm = TRUE),
            .groups = "drop_last") %>%
  reframe(species = if (all(is.na(sp))) NA else first(sp[count == max(count)]),
            count = if (is.na(species)) 0 else count[species == sp],
            .groups = "drop") %>%
  right_join(interval_locs, by = c("transect", "interval")) %>%
  st_sf() %>%
  arrange(transect, interval)
sgmap <- terra::rast("data/South Georgia Navigation Map/SouthGeorgiaNavMap.tif")
bbox <- st_bbox(c(xmin = -39.5, ymin = -55.5, xmax = -34.5, ymax = -52.5),
                crs = "epsg:4326")

if (interactive()) {
  png(here::here("figs", "mostcommonsp.png"), width = 800, height = 800)
  tm_shape(sgmap, bbox = bbox) +
    tm_rgb() +
    tm_shape(most_common) +
    tm_symbols(size = "count", col = "species",
               scale = 2,
               palette = "Set3",
               border.col = "black",
               border.lwd = 1) +
    tm_layout(legend.bg.color = "white",
              legend.position = c("left", "bottom"))
  dev.off()
}

