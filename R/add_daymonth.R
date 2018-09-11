#' Function to calculate memory used by all objects in the global environment.
#'
#' @param type defines whether you want the sum of all objects or a sorted data frame
#'
#'
#' @export



add_daymonth <- function(df){
	df %>%
		mutate(Day = lubridate::day(Date)) %>%
		mutate(Month = lubridate::day(Month))

}
