
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

monthFromDate <- function(date) {
	date <- as.character(date)
	as.numeric(format(as.Date(date), "%m"))
}

yearFromDate <- function(date) {
# date is a string like "2007-7-10"    YYYY-M-D
# to avoid date shifts because of your local time zone if date is a POSIX. 
	date <- as.character(date)
	as.numeric(format(as.Date(date), "%Y"))
}

