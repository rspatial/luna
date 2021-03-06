# List and Download MODIS products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

getModis <- function(product, start_date, end_date, aoi, download=FALSE, path, username, password,
                     version = "006", limit = 100000, overwrite=FALSE, ...) {
  
    server = "LPDAAC_ECS"
	
	if(missing(product)) stop("provide a product name")
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	if(missing(aoi)) stop("provide an area of interest")

	pp <- .humanize()
	pp <- pp[pp$short_name == product & pp$version == version & pp$provider == server, ]
  
	if(nrow(pp) < 1) {
		stop("The requested product is not available through this function")
	} else if (nrow(pp) > 1) {
		warning("Multiple sources available, using first one")
		print(pp)
		pp <- pp[1, ]
	}
	
  
  # find product urls, does not require credentials
	results <- .searchGranules(product = product, start_date = start_date, end_date = end_date, extent = aoi, limit = limit)
	urls <- unique(results[, "Online Access URLs"])

	if (length(urls) > 0) {
		if (download){
			path <- .getPath(path)
			if(missing(username)) stop("provide a username")
			if(missing(password)) stop("provide a password")

			ff <- .cmr_download(urls, path, username, password, overwrite)			

			ff <- file.path(path, basename(urls))	
			return(ff)		 
		} else {
			return(basename(urls))
		}
	} else {
		print("No results found")
		return(NULL)
	}
}



.dateFromYearDoy <- function(x) {
	year <- as.integer(substr(x, 1, 4))
	doy <- as.integer(substr(x, 5, 8))
	return(as.Date(doy, origin=paste(year-1, "-12-31", sep='')))
}


modisDate <- function(filename) {
  ff <- basename(filename)
  dot <- sapply(strsplit(ff, "\\."), '[', 2)
  dates <- gsub("[aA-zZ]", "", dot)
  dates <- substr(basename(filename), 10, 16)
  dates <- .dateFromYearDoy(dates)
  dm <- format(dates, "%m")
  dy <- format(dates, "%Y")
  dd <- format(dates, "%d")
  data.frame(filename=filename, date=dates, year=dy, month=dm, day=dd, stringsAsFactors = FALSE)
}
