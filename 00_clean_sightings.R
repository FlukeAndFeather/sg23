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
            transect = "1112",
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
            transect = "1112",
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

# PORT
# First two hours were recorded to separate file, without location or time.
# Tried to align the "warmup" file to the starboard obs, but couldn't find
# observations in common.
# For now, randomizing time
gap_start <- ymd_hms("2023-07-12 10:56:44", tz = "UTC")
gap_end <- ymd_hms("2023-07-12 12:02:53", tz = "UTC")
gap_dur <- as.numeric(gap_end - gap_start, units = "secs")
set.seed(1928)
jul12_port_warmup <- read_csv("data/Sightings/12July_warmup.csv") %>%
  transmute(lon = Longitude,
            lat = parse_number(Latitude),
            datetime = gap_start + sort(runif(n(), max = gap_dur)),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1314",
            side = "PORT")
jul12_port <- read_csv("data/Sightings/12July_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1314",
            side = "PORT") %>%
  drop_na(species)

# STAR
jul12_star <- read_csv("data/Sightings/12Jul23_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1314",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "COPD" ~ "CODP",
    "KEGP" ~ "KEPT",
    "KERH" ~ "KEPT",
    "NOGP" ~ "NGPT",
    "SOGP" ~ "SGPT",
    "UNGP" ~ "UGPT",
    "UNGPT" ~ "UGPT",
    .default = species
  ))

jul12_port_warmup$lon <- approx(jul12_star$datetime,
                                jul12_star$lon,
                                xout = jul12_port_warmup$datetime)$y
jul12_port_warmup$lat <- approx(jul12_star$datetime,
                                jul12_star$lat,
                                xout = jul12_port_warmup$datetime)$y
jul12_port <- rbind(jul12_port_warmup, jul12_port)

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

# PORT
jul14_port <- read_csv("data/Sightings/14 jULY_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1718",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "SFS.", "STS."))

# STAR
jul14_star <- read_csv("data/Sightings/14July23_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1718",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "START")) %>%
  # There's a UNPI - unknown pinniped? Probably FUSE given count.
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "SCGO" ~ "SGCO",
    "UNGP" ~ "UGPT",
    .default = species
  ))

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
            transect = "1920",
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
            transect = "1920",
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
  filter(species != "END") %>%
  mutate(species = ifelse(species == "ARTE", "ANTE", species))


# July 17 -----------------------------------------------------------------

# PORT
jul17_port <- read_csv("data/Sightings/17jULY23_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1112b", # Filling in oceanography on 1112
            side = "PORT") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "FLOCK")) %>% # What's FLOCK??
  mutate(species = case_match(
    species,
    "ARTE" ~ "ANTE",
    "GNPN" ~ "GEPN",
    "KEPN" ~ "KIPN",
    "KGEPN" ~ "KIPN",
    "SGO" ~ "SGCO",
    .default = species
  ))

# STAR
jul17_star <- read_csv("data/Sightings/17Jul23_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "1112b", # Filling in oceanography on 1112
            side = "STAR") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "START")) %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "COPD" ~ "CODP",
    "SCGO" ~ "SGCO",
    "UNGP" ~ "UGPT",
    .default = species
  ))

# July 18 -----------------------------------------------------------------

# No obs; visit to Grytviken

# July 19 -----------------------------------------------------------------

# PORT
jul19_port <- read_csv("data/Sightings/19July_2_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0910",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END")

# STAR
jul19_star <- read_csv("data/Sightings/19Jul23_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0910",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "KERG" ~ "KEPT",
    "KERH" ~ "KEPT",
    "KGPT" ~ "KEPT",
    "SPGT" ~ "SGPT",
    "SRWH" ~ "RIWH",
    "UNGP" ~ "UGPT",
    .default = species
  ))


# July 20 -----------------------------------------------------------------

# PORT
jul20_port <- read_csv("data/Sightings/20July23_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0708",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = ifelse(species == "KERG", "KEPT", species))

# STAR
jul20_star <- read_csv("data/Sightings/20jul23_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0708",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "KEGP" ~ "KEPT",
    "KERG" ~ "KEPT",
    "WALL" ~ "WAAL",
    .default = species
  ))


# July 21 -----------------------------------------------------------------

# PORT
jul21_port <- read_csv("data/Sightings/21July23_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0506",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = ifelse(species == "KEPA", "KEPT", species))

# STAR
jul21_star <- read_csv("data/Sightings/21july23_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0506",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "KERG" ~ "KEPT",
    "UNGP" ~ "UGPT",
    .default = species
  ))

# July 22 -----------------------------------------------------------------

# PORT
jul22_port <- read_csv("data/Sightings/22July_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0304",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(species != "END")

# STAR
jul22_star <- read_csv("data/Sightings/22jul23_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0304",
            side = "STAR") %>%
  drop_na(species) %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "COPD" ~ "CODP",
    "KERG" ~ "KEPT",
    .default = species
  ))

# July 23 -----------------------------------------------------------------

# PORT
jul23_port <- read_csv("data/Sightings/23July_2_obs.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Lon,
            lat = Lat,
            datetime = mdy_hms(paste(Date, Time), tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0102",
            side = "PORT") %>%
  drop_na(species) %>%
  filter(!species %in% c("END", "RABO")) # RABO?? Rainbow??

# STAR
jul23_star <- read_csv("data/Sightings/23JulACTUAL_ED.csv",
                       show_col_types = FALSE) %>%
  transmute(lon = Longitude,
            lat = Latitude,
            datetime = ISOdatetime(YYYY, MM, DD, hh, mm, ss.s, tz = "UTC"),
            species = Spp,
            count = Count,
            behavior = Behavior,
            transect = "0102",
            side = "STAR") %>%
  drop_na(species) %>%
  filter(species != "END") %>%
  mutate(species = case_match(
    species,
    "CAPE" ~ "CAPT",
    "COPD" ~ "CODP",
    .default = species
  ))

# All dates ---------------------------------------------------------------

# Names of data frames
dates <- rep(c(11:15, 19:23), each = 2)
sides <- rep(c("port", "star"), 10)
all_obs <- str_glue("jul{dates}_{sides}")

# Name -> eval -> rbind
sightings <- map(all_obs, ~ eval(sym(.x))) %>%
  list_rbind()%>%
  mutate(lon = -lon, lat = -lat)

saveRDS(sightings, "data/sightings.rds")

# TODO --------------------------------------------------------------------
# Missing 2 hours of data at the start of Jul 12 on the port side (RRV investigating).
  # Found observations, but time missing. Currently randomizing them? Seems
  # like a bad long-term solution.
# JWSM have additional sightings for Jul 12 morning starboard. See file "JW&SM_MissingDataForStartOfJul12Transect.xlsx"

