
'%>%' <- magrittr::`%>%`



#' Function to calculate memory used by all objects in the global environment.
#'
#' @param x Directory you want to look in. This must be stated
#' @param recursive Do you want to do a recursive search?
#' @param extension filter by specified extension
#'
#' @return list of files
#'
#' @export


list_files <- function(x, recursive = TRUE, extension = NULL, full_path = FALSE){

	if(is.null(extension)){
	df <- setdiff(list.files(x, recursive = recursive), list.dirs(x, recursive = recursive, full.names = FALSE))
	df <- dplyr::as_tibble(df)
	df <- dplyr::rename(df, File = value)
	}

	if(!is.null(extension)){
	df <- setdiff(list.files(x, recursive = recursive), list.dirs(x, recursive = recursive, full.names = FALSE))
	df <- dplyr::as_tibble(df)
	df <- dplyr::rename(df, File = value)
	df <- dplyr::filter(df, ends_with(File, extension, ignore_case = TRUE))
	}

	if(full_path)
		df <- df %>%
			dplyr::mutate(File = stringr::str_c(x,"/", File)) %>%
			dplyr::mutate(File = stringr::str_replace_all(File, "//", "/"))


	return(df)

}








