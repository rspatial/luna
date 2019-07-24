# Authors: Robert J. Hijmans, Aniruddha Ghosh, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

# List different satellite data products that can be searched through CMR

# List all unique products available through CMR
getProducts <- function(product = NULL, download = TRUE,...){
   d <- .humanize()
   
   if (length(product) > 1){
     paste0(product, collapse = "|")
   }
   
   if (!is.null(product)){
     pp <- d[grep(product,d$short_name), ]
   } else {
     return(unique(d$short_name))
   }
}


# humanizers report for the list of dataset available through CMR https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html
# use this file to get an updated list of dataset that can searched (not always downloadable) via cmr

.humanize <- function(download = TRUE, path="", ...) {
  
  #do not add dependencies on raster; less so to functions that are not exported
  # copy the function if we must
  path <- .getCleanPath(path)
  
	filename <- file.path(path,"nasa_earthdata_products.csv")
  
  # could also check the time stamp of the file, download again if too  old
	if (!file.exists(filename)) {
		if (download) {
			theurl <- paste0("https://cmr.earthdata.nasa.gov/search/humanizers/report")
			.download(theurl, filename)
			if (!file.exists(filename)) {
				message("\nCould not download file -- perhaps it does not exist, check if the output looks good") 
			}
		} else {
			message("File not available locally. Use 'download = TRUE'")
		}
	}	
	if (file.exists(filename)) {
		data <- utils::read.csv(filename, stringsAsFactor=FALSE)
		return(data)
	} 	
}


# setup credentials for different services
# ag: this function should be exposed with a help file
getCredentials <- function(url = NULL, username = NULL, password = NULL, credfile = NULL, savecred=TRUE, ...) {

	saveCrd <- function(credInfo) {
		credfile <- path.expand("~/luna_cred.rds")
		if (file.exists(credfile)) {
			d <- readRDS(credfile)
			i <- which(url == d$url) 
			credInfo <- rbind(credInfo, d)
			if (length(i) > 0) {
				credInfo <- credInfo[-i, ]
			}
		}
		saveRDS(credInfo, credfile)
	}
  
	if (!is.null(credfile)) { # it is useful to be able to point to a file
		credInfo <- readRDS(credfile)
		usr = credInfo$user
		pswd = credInfo$password
	} else if ((!is.null(username)) && (!is.null(password))) {
		usr <- username
		pswd <- password
		credInfo <- data.frame(url = url, user = usr, password = pswd, stringsAsFactors = FALSE)
		if (savecred) saveCrd(credInfo)
	} else {
		credfile <- path.expand("~/luna_cred.rds")
		ok <- TRUE
		if (file.exists(credfile)) {
			credInfo <- readRDS(credfile)
			credInfo <- credInfo[credInfo$url == url, ]
			if (nrow(credInfo) == 1) {
				usr <- credInfo$user
				pswd <- credInfo$password
			} else {
				ok <- FALSE
			}
		}
		if (!ok) {
			paste("Credentials for: ", url)
			usr <- readline(paste("username: \n"))
			pswd <- readline(paste("password: \n"))
			credInfo <- data.frame(url = url, user = usr, password = pswd, stringsAsFactors = FALSE)
			if (savecred) saveCrd(credInfo)
		}
	}
	return(credInfo)
}




# Open the product information in a browser

showInfo <- function(product, version, server, ...){
  
  pp <- .humanize()
  
  # get the unique set of information for the product
  pp <- pp[pp$short_name == product, ]
  
  # for MODIS
  # ok <- grepl("^MOD|^MYD|^MCD", product)
  
  # include descripton of products from
  # first condition is specific to MODIS
  if (missing(server) | missing(version)){
    url <- paste0("https://cmr.earthdata.nasa.gov/search/concepts/", unique(pp$concept_id))
    
  } else if (server == "LPDAAC_ECS" & version == "006") {
    pp <- pp[pp$version == version, ]
    url <- paste0("https://lpdaac.usgs.gov/products/", tolower(unique(pp$short_name)),"v",version)

  } else {
    stop("Can not find the requested webpage")
  }
  
  # if multiple urls returned
  if (length(url) == 1 ){
    print(paste0("opening product description webpage for ", unique(pp$short_name)))
    browseURL(url)
      
  } else if (length(url) > 1){
      print("More than one product found; refine the product search with additional arguments")
  
  } else {
    print("Can not find the specified product description webpage requested")
  }
}


# for (i in 1:length(url)){
#   print(paste0("opening product description webpage of ", pp$short_name[i]))
#   browseURL(url[i])
#   # for multiple webpages; fail-safe otherwise it may create serious issues
#   invisible(readline(prompt="Press [enter] to open the webpage of the next product or "))
# }
