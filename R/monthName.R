#' Convert number of month to name of month
#'
#' Takes the number of a month and converts it to the name of that month
#'
#' @param mm A number of the month
#' @return MM Name of month to return
#'
#' @export



monthName <- function (mm) {
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



nearestValue <- function (x, Y) {
	return(Y[which(abs(Y - x) == min(abs(Y - x)))[1]])
}




#' Bin a value within a vector
#'
#' Takes a number and works out where it lies in another vector
#'
#' @param x A number
#' @return Nearest value to x in vector Y
#'
#' @export



binValue <- function (X, Y, method = "lower") {

	binned <- function (x, Y, method = "lower") {

  	if(method == "lower")
  		return( Y[ ifelse(Y[which(abs(Y - x) == min(abs(Y - x)))[1]] > x, -1,0) + which(abs(Y - x) == min(abs(Y - x)))[1]   ])

  	if(method == "upper")
  		return( Y[ ifelse(Y[which(abs(Y - x) == min(abs(Y - x)))[1]] < x, 1,0) + which(abs(Y - x) == min(abs(Y - x)))[1]   ])

	}

		return(apply(as.matrix(X), MARGIN = 1, FUN = binned, Y = Y))
}


