

#' Function to check if a value is in a set
#'
#' @param x vector to check
#' @param y set to check if data is in
#'
#'
#' @export


`%nin%` <- function(x,y){

	x %in% y == FALSE

}
