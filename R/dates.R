
dateFromDoy <- function(doy, year) {
	year <- round(year)
	doy <- round(doy)
	return(as.Date(doy, origin=paste(year-1, "-12-31", sep='')))
}

dateFromYearDoy <- function(x) {
	year <- as.integer(substr(x, 1, 4))
	doy <- as.integer(substr(x, 5, 8))
	return(as.Date(doy, origin=paste(year-1, "-12-31", sep='')))
}

ymdFromDate <- function(x) {
	x <- as.Date(as.character(x))
	dm <- format(x, "%m")
	dy <- format(x, "%Y")
	dd <- format(x, "%d")
	cbind(year=dy, month=dm, day=dd)
}

