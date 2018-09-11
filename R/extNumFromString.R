#' Extract the numbers from a string
#'
#' Takes a string and extracts the numbers that appear in it. Useful for cases where you want to pull out years
#'
#' @param x A string
#' @return the numbers that appear in the string
#'
#' @export



extNumFromString <- function (x) {

return(unique(na.omit(as.numeric(unlist(strsplit(unlist(x), "[^0-9]+"))))))
}


#' Create a vector of days of the year from a data frame
#'
#' Takes a string and extracts the numbers that appear in it. Useful for cases where you want to pull out years
#'
#' @param df. A data frame which has Year, Month and Day in named columns
#' @return A vector with the days of the year in each row
#'
#' @export


doyFromDF <- function (df, yy) {
	if(missing(yy))
      return(lubridate::yday(lubridate::ymd(paste0(df$Year, "-", df$Month, "-", df$Day)))) else
    return(lubridate::yday(lubridate::ymd(paste0(yy, "-", df$Month, "-", df$Day))))

}
