# List and Download Landsat products
# Authors: Alex Mandel, Aniruddha Ghosh, Robert J. Hijmans
# July 2019
# Version 0.1
# Licence GPL v3


split_landsat <- function(scene){
  # Split a Landsat scene ID into components
  
  scene <- "LC08_L1TP_167056_20190517_20190521_01_T1"
  scene_parts <- unlist(strsplit(scene, "_"))
  path <- substr(scene_parts[3],1,3)
  row <- substr(scene_parts[3],4,6)
  sensor <- scene_parts[1]
  collection <- scene_parts[6] # Not sure if this is the folder part
  
  return(c(collection=collection,path=path,row=row,sensor=sensor))
}

.find_aws <- function(scene, bands=seq(1:11)){
  # Given a Landsat scene ID generate the matching AWS public link to the files
  # https://landsatonaws.com/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1
  # http://landsat-pds.s3.amazonaws.com/c1/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1/LC08_L1TP_168060_20190625_20190705_01_T1_B1.TIF
  # scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  
  #Stop if scene not provided
  
  root_url <- "https://landsat-pds.s3.amazonaws.com"
  scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  url_parts <- split_landsat(scene)
  
  band_names <- paste0("_B", bands,".TIF")
  
  final_urls <- file.path(root_url,
                          "c1",
                          "L8", 
                          url_parts["path"], 
                          url_parts["row"], 
                          scene, 
                          paste0(scene, band_names)
  )
  
  return(final_urls)
}

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
	fileurls <- simplify_urls(results, server)

	# TODO: Apply filters if you only want to get certain files from the results
	# Example: only download TOA or SR, or L1
	
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


