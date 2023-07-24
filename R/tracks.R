find_intervals <- function(dlog3_path, transect_name) {
  result <- read_csv(dlog3_path, show_col_types = FALSE) %>%
    filter(Type == "GPS") %>%
    transmute(lon = Longitude,
              lat = parse_number(Latitude),
              datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
              transect = "1112",
              dist_m = c(0, geodist::geodist(cbind(lon, lat), sequential = TRUE)),
              cum_dist_m = cumsum(dist_m),
              cum_dist_nmi = cum_dist_m / 1000 / 1.6 / 1.15,
              # There's a bit of extra that shouldn't get left out
              lead_dist_nmi = lead(dist_m, default = 0) / 1000 / 1.6 / 1.15,
              interval = 1 + floor(cum_dist_nmi)) %>%
    group_by(interval) %>%
    summarize(
      interval_start = min(datetime),
      dist_nmi = max(cum_dist_nmi) - min(cum_dist_nmi) + last(lead_dist_nmi)
    )
  result[["transect"]] <- transect_name
  result
}

assign_intervals <- function(sightings, intervals) {
  transects <- unique(sightings$transect)
  map_dfr(transects, function(tr) {
    tr_intervals <- filter(intervals, transect == tr)
    tr_sightings <- filter(sightings, transect == tr)
    tr_sightings %>%
      mutate(interval = approx(tr_intervals$interval_start,
                               tr_intervals$interval,
                               xout = datetime,
                               method = "constant",
                               yright = max(tr_intervals$interval))$y) %>%
      left_join(select(tr_intervals, interval, dist_nmi), by = "interval")
  })
}
