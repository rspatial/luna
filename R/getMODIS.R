# List and Download MODIS products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3


getModis <- function(product, start_date, end_date, aoi, download=FALSE, path="",
                     version = "006", limit = 100000, server = "LPDAAC_ECS", overwrite=FALSE, ...) {
  
	#stopifnot(require(readr))
	#stopifnot(require(httr))
	
	if(missing(product)) stop("provide a product name")
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	if(missing(aoi)) stop("provide an area of interest")
	path <- .getCleanPath(path)
	

	pp <- .humanize(path=path)
	pp <- pp[pp$short_name == product & pp$version == version & pp$provider == server, ]
  
	if(nrow(pp) < 1) {
		stop("The requested product is not available through this function")
	} else if (nrow(pp) > 1) {
		warning("Multiple sources available")
		print(pp)
		#stop()
	}
	
  
  # find product urls, does not require credentials
	results <- searchGranules(product = product, start_date = start_date, end_date = end_date, extent = aoi, limit = limit)
	
	# Select out the urls and remove duplicates
	fileurls <- simplify_urls(results, server="MODIS")
	
  # TODO: need a better try-error message for the function
	if (length(fileurls) > 0) {
		if (download){
			cred <- getCredentials(url="https://urs.earthdata.nasa.gov/users/new", ...)
			files <- cmr_download(urls = fileurls, path = path, 
					 username = cred$user, password = cred$password,
					 overwrite = overwrite)			
			# rh: is there a better way? 
			ff <- file.path(path, basename(fileurls))	
			return(ff)		 
		} else {
			return(basename(fileurls))
		}
	} else {
		print("No results found")
		return(NULL)
	}
}


