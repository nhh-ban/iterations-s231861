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
  mutate(latestData = as_datetime(latestData, tz = "UTC")) %>%
  # To fix location, we unlist first
  mutate(location = map(location, unlist)) %>%
  mutate(
    lat = map_dbl(location, "latLon.lat"),
    lon = map_dbl(location, "latLon.lat")
  ) %>%
  select(-location)
}


#### Assignment 4 getting volume data ----
## Add a function called 'to_iso8601':
# Assign two argument; date-time and offset measured in days:
to_iso8601 <- function(input_datetime, offset_days) {
  # What the first argument 
  datetime <- input_datetime + days(offset_days)
  iso8601_date <- format(datetime, format = "%Y-%m-%dT%H:%M:%SZ")
  return(iso8601_date)
}

to_iso8601(as_datetime("2016-09-01 10:11:12"), 0)
to_iso8601(as_datetime("2016-09-01 10:11:12"), -4)


## Add function to change from- and to times:
# Use mutate and to_iso8601 function to update the correct date-format
stations_metadata_df <-
stations_metadata_df %>%
  mutate(
    latestData = to_iso8601(latestData, 0)
  )


## Function that creates volume queries.
# Arguments: Station id, from-  and to dates:

# Didn't manage to do anything more here. Trouble with creating a function
# that takes the arguments "id", "from" and "to". 
# Wait for the solution to update my work!




