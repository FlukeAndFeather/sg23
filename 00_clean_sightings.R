library(lubridate)
library(tidyverse)


# July 11 -----------------------------------------------------------------

# PORT
jul11_port <- read_csv("data/Sightings/11JULY23_2_ED.obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1920",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END")

# STARBOARD
jul11_star <- read_csv("data/Sightings/11Jul23ED_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1920",
            side = "STAR") %>%
  drop_na(species) %>%
  # UNP could be UNPT, UNPN, UNPR...
  filter(!species %in% c("START", "END", "UNP")) %>%
  mutate(species = case_when(
    species == "NOGP" ~ "NGPT",
    species == "SOGP" ~ "SGPT",
    TRUE ~ species
  ))

# July 12 -----------------------------------------------------------------
# Starboard obs missing

# PORT
jul12_port <- read_csv("data/Sightings/12July_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1718",
            side = "PORT") %>%
  drop_na(species)

# July 13 -----------------------------------------------------------------

# PORT
# Warnings from hidden 13th column
jul13_port <- read_csv("data/Sightings/13jULY23_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1516",
            side = "PORT") %>%
  drop_na(species) %>%
  # SODP = CODP? KEPN = KIPN or KEPT?
  filter(!species %in% c("SODP", "KEPN", "END"))

# STAR
jul13_star <- read_csv("data/Sightings/13jul23ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = parse_number(Latitude),
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1516",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_when(
    species == "COPD" ~ "CODP",
    species == "ANTP" ~ "ANPT",
    TRUE ~ species
  ))

# July 14 -----------------------------------------------------------------
# Starboard obs missing

# PORT
jul14_port <- read_csv("data/Sightings/14 jULY_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1314",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "SFS.", "STS."))

# July 15 -----------------------------------------------------------------

# PORT
# 15july23_2.csv had an "embedded null" error on one of the GPS rows, so
# I made a copy and deleted it.
jul15_port <- read_csv("data/Sightings/15july23_2_nonulls.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = parse_number(Latitude),
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1112",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = ifelse(species == "CAP", "CAPT", species))

# STAR
jul15_star <- read_csv("data/Sightings/15jul23_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1112",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_when(
    species == "KERG" ~ "KEPT",
    species == "UNGPT" ~ "UGPT",
    species == "CAPE" ~ "CAPT",
    TRUE ~ species
  ))

# July 16 -----------------------------------------------------------------

# PORT
jul16_port <- read_csv("data/Sightings/16jULY23_2_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = parse_number(Latitude),
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1516b",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = ifelse(species == "ANPE", "ANPT", species))

# STAR
# Starts at 0930 local time - missing data?
jul16_star <- read_csv("data/Sightings/16July231_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1516b",
            side = "STAR") %>%
  drop_na(species) %>%
  mutate(species = ifelse(species == "ARTE", "ANTE", species))

# All dates ---------------------------------------------------------------

sightings <- rbind(
  jul11_port,
  jul11_star,
  jul12_port,
  jul13_port,
  jul13_star,
  jul14_port,
  jul15_port,
  jul15_star,
  jul16_port,
  jul16_star
) %>%
  mutate(lon = -lon, lat = -lat)
saveRDS(sightings, "data/sightings.rds")

