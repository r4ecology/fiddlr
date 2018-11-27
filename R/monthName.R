#' Convert number of month to name of month
#'
#' Takes the number of a month and converts it to the name of that month
#'
#' @param mm A number of the month
#' @return MM Name of month to return
#'
#' @export



month_name <- function (mm) {
	mm <- as.integer(mm)
	months <- return(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")[mm])

}


#' Find the nearest value to x in vector Y
#'
#' Takes a number and calculates the nearest value to it in a vector
#'
#' @param x A number
#' @return Nearest value to x in vector Y
#'
#' @export

