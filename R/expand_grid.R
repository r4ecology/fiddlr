#' tibble version of expand.grid
#'
#' Takes the number of a month and converts it to the name of that month
#' @params ... vectors, factors or a list containing these
#' @params KEEP.OUT.ATTRS	a logical indicating the "out.attrs" attribute (see below) should be computed and returned.
#' @params stringsAsFactors	 logical specifying if character vectors are converted to factors.
#' @return A tibble containing all possible combinations of the given variables
#'
#' @export


expand_grid <- function (..., KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE)
{
	nargs <- length(args <- list(...))
	if (!nargs)
		return(as.data.frame(list()))
	if (nargs == 1L && is.list(a1 <- args[[1L]]))
		nargs <- length(args <- a1)
	if (nargs == 0L)
		return(as.data.frame(list()))
	cargs <- vector("list", nargs)
	iArgs <- seq_len(nargs)
	nmc <- paste0("Var", iArgs)
	nm <- names(args)
	if (is.null(nm))
		nm <- nmc
	else if (any(ng0 <- nzchar(nm)))
		nmc[ng0] <- nm[ng0]
	names(cargs) <- nmc
	rep.fac <- 1L
	d <- lengths(args)
	if (KEEP.OUT.ATTRS) {
		dn <- vector("list", nargs)
		names(dn) <- nmc
	}
	orep <- prod(d)
	if (orep == 0L) {
		for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
	}
	else {
		for (i in iArgs) {
			x <- args[[i]]
			if (KEEP.OUT.ATTRS)
				dn[[i]] <- paste0(nmc[i], "=", if (is.numeric(x))
					format(x)
					else x)
			nx <- length(x)
			orep <- orep/nx
			x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac,
																									nx)), orep)]
			if (stringsAsFactors && is.character(x) && !is.factor(x))
				x <- factor(x, levels = unique(x))
			cargs[[i]] <- x
			rep.fac <- rep.fac * nx
		}
	}
	if (KEEP.OUT.ATTRS)
		attr(cargs, "out.attrs") <- list(dim = d, dimnames = dn)
	rn <- .set_row_names(as.integer(prod(d)))
	dplyr::as_tibble(structure(cargs, class = "data.frame", row.names = rn))
}




