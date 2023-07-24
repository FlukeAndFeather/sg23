library(sf)
library(tidyverse)

# Tracks are messy, neither port nor starboard are complete. But we can stitch
# them together. Don't forget: coords must be swapped NS and EW.

# 1112: port
# 1314: starboard then port
# 1516: starboard
# 1718: port
# 1920: starboard

track_files <- list(
  `1112` = here::here("data", "DLOG3", "PORT", "11JULY23.csv"),
  `1314` = c(here::here("data", "DLOG3", "STAR", "12jul23.csv"),
             here::here("data", "DLOG3", "PORT", "12July_2.csv")),
  `1516` = here::here("data", "DLOG3", "STAR", "13jul23.csv"),
  `1718` = here::here("data", "DLOG3", "PORT", "14 jULY_2.csv"),
  `1920` = here::here("data", "DLOG3", "STAR", "15jul23.csv")
)
transects <- names(track_files)
effort <- read_csv(here::here("data", "Effort.csv")) %>%
  transmute(
    transect = as.character(transect),
    transect_start = ISOdatetime(YYYY_start, MM_start, DD_start,
                                 hh_start, mm_start, ss.s_start,
                                 tz = "UTC"),
    transect_end = ISOdatetime(YYYY_end, MM_end, DD_end,
                               hh_end, mm_end, ss.s_end,
                               tz = "UTC")
  )

# Cut tracks into 1 nmi intervals
read_track <- function(x) {
  read_csv(x, col_types = cols_only(
    Longitude = "d",
    Latitude = "d",
    YYYY = "d",
    MM = "d",
    DD = "d",
    hh = "d",
    mm = "d",
    ss.s = "d",
    Type = "c"
  )) %>%
    filter(Type == "GPS") %>%
    transmute(
      lon = -Longitude,
      lat = -Latitude,
      datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC")
    )
}
intervals <- map2(
  track_files, transects,
  function(f, tr) {
    if (length(f) == 1) {
      track <- read_track(f)
    } else {
      track1 <- read_track(f[1])
      track2 <- read_track(f[2])
      track <- rbind(track1, filter(track2, datetime > last(track1$datetime)))
    }
    track %>%
      mutate(
        dist_m = c(0, geodist::geodist(cbind(lon, lat), sequential = TRUE)),
        cum_dist_m = cumsum(dist_m),
        cum_dist_nmi = cum_dist_m / 1000 / 1.6 / 1.15,
        # There's a bit of extra that shouldn't get left out
        lead_dist_nmi = lead(dist_m, default = 0) / 1000 / 1.6 / 1.15,
        interval = 1 + floor(cum_dist_nmi),
        transect = tr,
      ) %>%
      left_join(select(effort, transect, transect_start, transect_end),
                by = "transect") %>%
      filter(datetime >= transect_start,
             datetime <= transect_end) %>%
      group_by(transect, interval) %>%
      summarize(
        interval_start = min(datetime),
        dist_nmi = max(cum_dist_nmi) - min(cum_dist_nmi) + last(lead_dist_nmi),
        lon = mean(lon),
        lat = mean(lat),
        .groups = "drop"
      )
  }) %>%
  list_rbind() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")

ggplot(intervals) + geom_sf(aes(color = transect))
