# Authors: Robert J. Hijmans, Aniruddha Ghosh, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

# List different satellite data products that can be searched through CMR

# List all unique products available through CMR
getProducts <- function(product = NULL, ...){
   d <- .humanize()
   
   if (length(product) > 1){
     paste0(product, collapse = "|")
   }
   
   if (!is.null(product)){
     d <- d[grep(product,d$short_name), ]
   }
   
   return(d)
}


# humanizers report for the list of dataset available through CMR https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html
# use this file to get an updated list of dataset that can searched (not always downloadable) via cmr


.humanize <- function(...) {
	f <- system.file("cmr/cmr_sources.rds", package="luna")
	d <- readRDS(f)
	return(d)
}


.oldhumanize <- function(download=TRUE, path="", ...) {
  
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
		d <- utils::read.csv(filename, stringsAsFactor=FALSE)
		return(d)	
	} 	
}


# setup credentials for different services
# ag: this function should be exposed with a help file
getCredentials <- function(url=NULL, username = NULL, password = NULL, credfile = NULL, savecred=TRUE, removecred=FALSE, ...) {

	defcredfile <- path.expand("~/luna_cred.rds")

	if (removecred) {
		if (is.null(credfile)) {
			credfile <- defcredfile
		}
		if (file.exists(credfile)) {
			file.remove(credfile)
		} 
		return()
	}

	saveCrd <- function(credInfo, crdfile) {
		if (file.exists(crdfile)) {
			d <- readRDS(crdfile)
			i <- which(url == d$url) 
			credInfo <- rbind(credInfo, d)
			if (length(i) > 0) {
				credInfo <- credInfo[-i, ]
			}
		}
		credInfo <- unique(credInfo)
		saveRDS(credInfo, crdfile)
	}
  
	if (!is.null(credfile)) { # it is useful to be able to point to a file
		credInfo <- readRDS(credfile)
		credInfo <- credInfo[credInfo$url == url, , drop=FALSE]
		if (nrow(credInfo) == 0) {
			stop("no record for url in supplied credfile")
		}
	} else if ((!is.null(username)) && (!is.null(password))) {
		credInfo <- data.frame(url = url, user = username, password = password, stringsAsFactors = FALSE)
		if (savecred) saveCrd(credInfo, defcredfile)
	} else {
		ok <- FALSE
		if (file.exists(defcredfile)) {
			credInfo <- readRDS(defcredfile)
			credInfo <- credInfo[credInfo$url == url, ]
			if (nrow(credInfo) > 0) {
				ok <- TRUE
			}
		}
		if (!ok) {
			paste("Credentials for: ", url)
			usr <- readline(paste("username: \n"))
			pswd <- readline(paste("password: \n"))
			credInfo <- data.frame(url = url, user = usr, password = pswd, stringsAsFactors = FALSE)
			if (savecred) saveCrd(credInfo, defcredfile)
		}
	}
	return(credInfo)
}


# Open the product information in a browser

productInfo <- function(product, ...){
	pp <- .humanize()
 
  # get the unique set of information for the product
	pp <- pp[pp$short_name == product, ]
	if (nrow(pp) < 1) {
		stop("Cannot find the product")
	}
	
  # for MODIS
	modis <- grepl("^MOD|^MYD|^MCD", product)
	if (modis) {
		#server = "LPDAAC_ECS"
		url <- paste0("https://lpdaac.usgs.gov/products/", tolower(product),"v006")
	} else {
		url <- paste0("https://cmr.earthdata.nasa.gov/search/concepts/", unique(pp$concept_id))
	} 
  
	if (length(url) > 0 ){
		print(paste0("opening product description web page for ", unique(pp$short_name[1])))
		utils::browseURL(url)	  
	}
}


# for (i in 1:length(url)){
#   print(paste0("opening product description webpage of ", pp$short_name[i]))
#   browseURL(url[i])
#   # for multiple webpages; fail-safe otherwise it may create serious issues
#   invisible(readline(prompt="Press [enter] to open the webpage of the next product or "))
# }

simplify_urls <- function(response_table, server, ...){
  # Depending on the type of data requested the url formatting will vary
  # MODIS the download url is the `Online Access URLs`
  # Landsat the `Online Access URLs` is a webpage listing options
  #  1. We could read the page and load those options
  #  2. We can construct the AWS or Google URL to the same thing
  #   a. AWS and Google might be per band files instead of a single archive (tar.gz)
  
  if (server == "MODIS"){
    # TODO: Handle errors from the TryCatch
    catcher <- tryCatch(urls <- response_table$`Online Access URLs`,error=function(e){e})
    urls <- catcher
  } else if (server == "AWS"){
    # TODO: Handle errors from the TryCatch
    #"https://landsatonaws.com/L8/025/023/LC08_L1TP_025023_20190717_20190717_01_RT/"
    catcher <- tryCatch(urls <- response_table$`Granule UR`,error=function(e){e})
    sceneID <- catcher[grep("T1$", catcher)]
    urls <- unlist(lapply(sceneID, .find_aws))
  } else if (server == "ERS"){
    sceneID <- tryCatch(urls <- response_table$`Online Access URLs`,error=function(e){e})
    urls <- unlist(lapply(sceneID, find_durls_ers))
  } 
  else {
    # TODO: What to do if no urls are found?
    urls <- NULL
  }

  return(unique(urls))
}