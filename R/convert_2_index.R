#' Theme used for blog posts
#'
#' Theme used for blogs
#'
#' @param i Number you want to convert to 1st, 2nd etc.
#'
#' @export

int_to_ranking <- function(i){

	whereinf <- is.infinite(i)
	wherena <- is.na(i)
	whereneg <- sign(i) == -1L
	i <- suppressWarnings(as.integer(i))
	if(any(is.na(i) & (!whereinf) & (!wherena))) stop('could not convert some inputs to integer')

	last_digit <- as.numeric(substring(i, nchar(i)))
	ending <- sapply(last_digit + 1, switch, 'th', 'st', 'nd', 'rd', 'th', 'th', 'th', 'th', 'th', 'th')
	second_last_digit <- as.numeric(substring(i, nchar(i) - 1, nchar(i) - 1))
	ending[second_last_digit == 1L] <- 'th'
	out <- paste(i, ending, sep = '')

	out[whereinf] <- 'infinitieth'
	out[whereinf & whereneg] <- '-infinitieth'
	out[wherena] <- 'missingith'

	return(out)
}