# List and Download Landsat products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3


getLandsat <- function(product="Landsat_8_OLI_TIRS_C1", start_date, end_date, aoi, download=FALSE, path="",
                     version = "1", limit = 100000, server = "AWS", overwrite=FALSE, ...) {
  
  # Search the CMR for Landsat scenes
  # Currently only supports the download of Landsat 8 from AWS
  # TODO: Implement alternate download from Google or EROS USGS for 4,5,7
  # Takes argument about which bands to download, will vary by Landsat version.
  
	#stopifnot(require(readr))
	#stopifnot(require(httr))
	
	if(missing(product)) stop("provide a product name")
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	if(missing(aoi)) stop("provide an area of interest")
	path <- .getCleanPath(path)
	

	pp <- .humanize(path=path)
	pp <- pp[pp$short_name == product & pp$version == version, ]
  
	if(nrow(pp) < 1) {
		stop("The requested product is not available through this function")
	} else if (nrow(pp) > 1) {
		warning("Multiple sources available")
		print(pp)
		#stop()
	}
	
  
  # find product, does not require credentials, returns data frame of csv
	results <- searchGranules(product = product, start_date = start_date, end_date = end_date, extent = aoi, limit = limit)
	
	# Select out the urls and remove duplicates
	# TODO: Pass server through to indicate AWS, GCP or USGS - only does AWS now.
	# TODO: For ERS results hit the find_durls_ers to get the actual file urls
	fileurls <- simplify_urls(results, sat="L8")

	
  # TODO: need a better try-error message for the function
	if (length(fileurls) > 0) {
		if (download){
		  # rh: is there a better way? 
		  if(server=="AWS"){
		    files <- cmr_download(urls = fileurls, path = path,
		                          overwrite = overwrite
		                          )			
		  } else if (server=="ERS"){
		    files <- download_ers(scenes = fileurls, path = path, 
		                          overwrite = overwrite
		                          )
			}
			
		  # TODO: This is going to return junk names for things from ERS 
			ff <- file.path(path, basename(fileurls))	
			return(ff)		 
		} else {
		  # TODO: return the dataframe of search results instead of just urls?
			return(basename(fileurls))
		}
	} else {
		print("No results found")
		return(NULL)
	}
}


