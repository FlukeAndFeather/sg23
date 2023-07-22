simplify_track <- function(track, dtol) {
  track %>%
    # Combine many features into 1
    sf::st_combine() %>%
    # Convert multiline to line
    sf::st_line_merge() %>%
    # Simplify to 250 m tolerance
    sf::st_simplify(dTolerance = dtol)
}

correct_lat <- function(track) {
  coords <- sf::st_coordinates(track)
  coords[,2] <- coords[,2] * -1
  coords %>%
    sf::st_linestring() %>%
    sf::st_sfc(crs = "epsg:4326") %>%
    {data.frame(geom = .)} %>%
    sf::st_sf()
}
