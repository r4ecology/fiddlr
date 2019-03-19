
ends_with <- function (x, char, ignore_case = FALSE)
{
	if (ignore_case) {
		x <- toupper(x)
		char <- toupper(char)
	}
	substr(x, start = nchar(x) - nchar(char) + 1, stop = nchar(x)) ==
		char
}

