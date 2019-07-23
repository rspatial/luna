# List and Download MODIS products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

getModisTiles <- function(product = "",
                             start_date = "",
                             end_date = "",
                             aoi = "",
                             download = FALSE,
                             downdir = "",
                             overwrite = TRUE,
                             version = "006",
                             limit = 100000,
                             server = "LP DAAC", ...){
  
  # first check if the product exists
  pp <- .humanize()
  pp <- pp[pp$short_name == product & pp$version == version & pp$original_value == server, ]
  
  if(nrow(pp) < 1) {
    stop("The requested product is not available through this package")
  } else if (nrow(pp) > 1) {
    stop("Multiple sources availble for the requested product; \n please provide a unique product")
  } else {
    print(paste("Searching", product, "for the requested specifications"))
  }
  
  # specify where to donwload the files
  if(download & downdir == ""){
    stop("The path can not be blank,please provide the location to store the downloaded file")
  }
  
  downdir <- raster:::.getDataPath(downdir)
  
  # set/check credentials
  cred <- getCredentials(url = "https://urs.earthdata.nasa.gov/users/new")
  
  if (inherits(aoi, 'Spatial')) {
    aoi <- extent(aoi)
  }
  
  # find product urls
  fileurls <- searchGranules(product = product, 
                             start_date = start_date, end_date = end_date, 
                             extent = aoi, limit = limit)

  # TODO: need a better try-error message for the function
  if (length(fileurls) > 0 & download){
   files <- cmr_download(urls = fileurls, path = downdir, 
                 username = cred$user, password = cred$password,
                 overwrite = overwrite)
  } else if (!download) {
    return(basename(fileurls))
  } else {
    print("No results found, make sure the you supplied the correct parameters")
  }
  return(files)
}


# TODO move to a generic function


# include descripton of products from 
# url <- paste0("https://cmr.earthdata.nasa.gov/search/concepts/", pp$concept_id)
# if (url == 1){browseURL(url)}