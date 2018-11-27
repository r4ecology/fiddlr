#' Extract the numbers from a string
#'
#' Takes a string and extracts the numbers that appear in it. Useful for cases where you want to pull out years
#'
#' @param x A string
#' @return the numbers that appear in the string
#'
#' @export



num_from_string <- function (x) {

return(unique(na.omit(as.numeric(unlist(strsplit(unlist(x), "[^0-9]+"))))))
}

