#' Function to calculate memory used by all objects in the global environment.
#'
#' @param type defines whether you want the sum of all objects or a sorted data frame
#'
#'
#' @export


global_memory <- function(){

	 total <- pryr::object_size(sapply(ls(envir=.GlobalEnv), get))

	 all_obj <- ls(envir=.GlobalEnv) %>%
	 	dplyr::tbl_df() %>%
	 	dplyr::rename(Object = value) %>%
	 	dplyr::group_by(Object) %>%
	 	dplyr::mutate(Size = pryr::object_size(get(Object))) %>%
	 	dplyr::arrange(dplyr::desc(Size))

	 return(list(Total = total, All_Objects = all_obj))
}
