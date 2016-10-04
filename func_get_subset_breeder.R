
# single trials
get_subset_breeder <- function(x = all_breeder_info, trial_name) {
  return( droplevels( subset(x, x$trial == trial_name) ) ) }


get_subset_breeder2 <- function(x = all_breeder_data, y = all_breeder_info, trial_name) {
  return( droplevels( subset(x, y$trial == trial_name) ) ) }

# single locations
get_subset_location <- function(x = all_breeder_info, loc_name) {
  return( droplevels( subset(x, x$location == loc_name) ) ) }

get_subset_location2 <- function(x = all_breeder_data, y = all_breeder_info, loc_name) {
  return( droplevels( subset(x, y$location == loc_name) ) ) }