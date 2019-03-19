

#' Function to calculate memory used by all objects in the global environment.
#'
#' @param x The variable you want to bin
#' @param bin_res The resolution for binning
#'
#'
#' @export
# bin_coord.numeric <- function(x, bin_res = 1) {
bin_value.numeric <- function(x, bin_res) {
	floor((x + bin_res / 2) / bin_res + 0.5) * bin_res - bin_res / 2
}



#' Function to calculate memory used by all objects in the global environment.
#'
#' @param x The variable you want to bin
#' @param bin_res The resolution for binning
#'
#'
#' @export

# function to bin coordinates

bin_value <- function(x, bin_res) {
	UseMethod("bin_value")
}

