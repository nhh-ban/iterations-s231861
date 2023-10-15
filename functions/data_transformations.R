# Transformation of 'stations_metadata' to data frame:

stations_metadata[[1]][[1]] %>%
  as_tibble()

transform_metadata_to_df <- function(stations_metadata) {
# Apply this for every station in the list:
# Specify in data "stations_metadata" that we are looking at 1 = remember it's a list
stations_metadata[[1]] %>%
  # function map(as_tibble) to traverse in the stations list and transform
  # to a data frame
  map(as_tibble) %>%
  # Combine the list of data frames to single data frame by using list_rbind()
  list_rbind() %>%
  # Both latestData and location is still lists. Fix by:
  # map_chr will try to return chr vector.
  # Below it will try to return the first item of each sub list in latestData
  # '.default=NA_character_' is necessarily!!!!
  mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) %>%
  mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin")) %>%
  # To fix location, we unlist first
  mutate(location = map(location, unlist)) %>%
  mutate(
    lat = map_dbl(location, "latLon.lat"),
    lon = map_dbl(location, "latLon.lat")
  ) %>%
  select(-location)
}
