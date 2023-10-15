# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(stations_metadata_df) {
# Fill in expected col. names in the new df:
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
# Check if expected col. names corresponds to col. names in df
    if (all(colnames(stations_metadata_df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }

test_stations_metadata_nrows <-
  function(stations_metadata_df) {
# Add constraints for min and max rows in the df
    min_expected_rows <- 5000
    max_expected_rows <- 10000
# Check if the df fulfills the contraints and print result.
    if (nrow(stations_metadata_df) > min_expected_rows & nrow(stations_metadata_df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(stations_metadata_df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }

test_stations_metadata_coltypes <-
  function(stations_metadata_df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
# Fill in expected types for each column in df
    if (all(stations_metadata_df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }
  
test_stations_metadata_nmissing <-
  function(stations_metadata_df) {
    max_miss_vals <- 200
# Constraint to check if the df has an acceptable amount of missing values
    if (stations_metadata_df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }

test_stations_metadata_latestdata_timezone <-
  function(stations_metadata_df) {
# Constraint to check if the df has the acceptable time zone or not
    if (attr(stations_metadata_df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }


test_stations_metadata <- 
  function(stations_metadata_df){
    test_stations_metadata_colnames(stations_metadata_df)
    test_stations_metadata_coltypes(stations_metadata_df)
    test_stations_metadata_nmissing(stations_metadata_df)
    test_stations_metadata_nrows(stations_metadata_df)
    test_stations_metadata_latestdata_timezone(stations_metadata_df)
  }





