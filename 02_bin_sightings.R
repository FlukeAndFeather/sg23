library(sf)
library(tidyverse)

# Just for east transects (waypoints 11-20)
sightings <- readRDS(here::here("data", "sightings.rds")) %>%
  filter(transect %in% c("1112", "1314", "1516", "1718", "1920"))
waypts <- read_csv(here::here("data", "waypoints.csv")) %>%
  filter(Waypoint >= 11)

track1920_df <- here::here("data", "DLOG3", "PORT", "11JULY23.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  filter(Type == "GPS") %>%
  transmute(lon = Longitude,
            lat = parse_number(Latitude),
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            transect = "1920",
            dist_m = c(0, geodist::geodist(cbind(lon, lat), sequential = TRUE)),
            cum_dist_m = cumsum(dist_m),
            cum_dist_nmi = cum_dist_m / 1000 / 1.6 / 1.15,
            # There's a bit of extra that shouldn't get left out
            lead_dist_nmi = lead(dist_m, default = 0) / 1000 / 1.6 / 1.15,
            interval = 1 + floor(cum_dist_nmi)) %>%
  group_by(interval) %>%
  summarize(
    interval_start = min(datetime),
    interval_end = max(datetime),
    dist_nmi = max(cum_dist_nmi) - min(cum_dist_nmi) + last(lead_dist_nmi)
  )

sightings_1920 <- sightings %>%
  filter(transect == "1920") %>%
  mutate(interval = approx(track1920_df$interval_start,
                           track1920_df$interval,
                           xout = datetime,
                           method = "constant",
                           yright = max(track1920_df$interval))$y) %>%
  left_join(select(track1920_df, interval, dist_nmi), by = "interval")

filter(sightings_1920, species == "CODP") %>%
  group_by(interval, lon, lat) %>%
  summarize(count = sum(count), .groups = "drop") %>%
  ggplot(aes(lon, lat)) +
  geom_point(aes(size = count), alpha = 0.5, color = "hotpink") +
  coord_sf(crs = "epsg:4326", xlim = c(-35.0, -36.0)) +
  theme_minimal()
