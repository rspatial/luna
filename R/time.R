
.yearFromDate <- function(date) {
	date <- as.character(date)
	as.numeric(format(as.Date(date), "%Y"))
}

.doyFromDate <- function(date) {
	date <- as.character(date)
	as.numeric(format(as.Date(date), "%j"))
}

.dateFromDoy <- function(doy, year) {
	year <- round(year)
	doy <- round(doy)
	return(as.Date(doy, origin=paste(year-1, "-12-31", sep='')))
}
