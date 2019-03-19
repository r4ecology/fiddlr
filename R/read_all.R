#' Read all files in a database
#'
#' Takes the number of a month and converts it to the name of that month
#'
#' @param df data frame
#' @param File Column name for files
#' @param read_function name of function to use for reading in
#' @return nested data frame
#'
#' @export




read_all <- function(df, File, read_function){
	File <- dplyr::enquo(File)

	df %>%
		mutate(data = purrr::map(!!File, read_function))
}
