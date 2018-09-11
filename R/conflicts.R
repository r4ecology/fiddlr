
library(tidyverse)

order_conflicts <- function(fun, df){
		find(fun) %>%
		stringr::str_replace_all("package:", "")

}

conflictr <- function(){

list_functions <- function(package){
	lsf.str(stringr::str_c("package:", package)) %>%
		as.character() %>%
		dplyr::as_tibble() %>%
		dplyr::mutate(Package = package)
}
loaded_packages <-(.packages())
functions <- purrr::map(loaded_packages, list_functions)

conflicts <- functions %>%
	dplyr::bind_rows() %>%
	dplyr::group_by(value) %>%
	tidyr::nest() %>%
	dplyr::mutate(conflicts = purrr::map(data, nrow)) %>%
	tidyr::unnest(conflicts) %>%
	dplyr::filter(conflicts > 1) %>%
	dplyr::select(-conflicts) %>%
	dplyr::mutate(conflicts = purrr::map2(value,data , order_conflicts)) %>%
	dplyr::select(-conflicts) %>%
	tidyr::unnest() %>%
	dplyr::group_by(value) %>%
	dplyr::mutate(Row = row_number() - 1) %>%
	tidyr::spread(Row, Package) %>%
	dplyr::rename(fun = value) %>%
	dplyr::ungroup()
names(conflicts)[1] <- "Function"
names(conflicts)[2] <- "Package_Used"
if(ncol(conflicts) > 2)
names(conflicts)[3:ncol(conflicts)] <-  stringr::str_c("conflict_",names(conflicts)[3:ncol(conflicts)])
conflicts

}

conflictr()

