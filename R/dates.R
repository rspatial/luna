
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

