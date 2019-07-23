# List and Download MODIS products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3


getModis <- function(product, start_date, end_date, aoi, download=FALSE, path,
                            version = "006", limit = 100000, server = "LP DAAC", ...) {
  
	if(missing(product)) stop("provide a product name")
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	if(missing(aoi)) stop("provide an area of interest")
	if(download && missing(path)) stop("provide a path for download")
	path = trimws(path)
	if (path == "") stop("provide a valid folder name (path)")

	overwrite = FALSE

	pp <- .humanize(path=path)
	pp <- pp[pp$short_name == product & pp$version == version & pp$original_value == server, ]
  
	if(nrow(pp) < 1) {
		stop("The requested product is not available through this function")
	} else if (nrow(pp) > 1) {
		stop("Multiple sources availble for the requested product; \n p rovide a unique product")
	} else {
		print(paste("Searching", product, "for the requested specifications"))
		utils::flush.console()
	}
  # set/check credentials
	cred <- getCredentials(url = "https://urs.earthdata.nasa.gov/users/new")
  
  # find product urls
	fileurls <- searchGranules(product = product, start_date = start_date, end_date = end_date, extent = aoi, limit = limit)

  # TODO: need a better try-error message for the function
	if ((length(fileurls) > 0) & download){
		files <- cmr_download(urls = fileurls, path = path, 
                 username = cred$user, password = cred$password,
                 overwrite = overwrite)
		return(files)		 
	} else if (!download) {
		return(basename(fileurls))
	} else {
		print("No results found")
		return(NULL)
	}
}


# TODO move to a generic function


# include descripton of products from 
# url <- paste0("https://cmr.earthdata.nasa.gov/search/concepts/", pp$concept_id)
# if (url == 1){browseURL(url)}