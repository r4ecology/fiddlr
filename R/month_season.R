#' Convert number of month to season
#'
#' Takes the number of a month and converts it to the season
#'
#' @param mm A number of the month
#' @return MM Name of season to return
#'
#' @export



month_season <- function(mm){
	ifelse(mm %in% c(12,1,2), "Dec-Feb",
				 ifelse(mm %in% 3:5, "Mar-May",
				 			 ifelse(mm %in% 6:8, "Jun-Aug","Sep-Nov"))) %>%
		factor( levels = c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov"))
}

