# Authors: Robert J. Hijmans, Aniruddha Ghosh, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

# List different satellite data products that can be searched through CMR


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

# many of the functions are repeated from raster <https://github.com/cran/raster>

# humanizers report for the list of dataset available through CMR https://cmr.earthdata.nasa.gov/search/site/docs/search/api.html
# use this file to get an updated list of dataset that can searched (not always downloadable) via cmr

.humanize <- function(download=TRUE, path="", ...) {
  
  #do not add dependencies on raster; less so to functions that are not exported
  # copy the function if we must
  #path <- raster:::.getDataPath(path)
  
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

getCredentials <- function(url = NULL, user = NULL, password = NULL, credfile = NULL, savecrd=FALSE, ...) {

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
	} else if ((!is.null(user)) && (!is.null(password))) {
		usr <- user
		pswd <- password
		credInfo <- data.frame(url = url, user = usr, password = pswd, stringsAsFactors = FALSE)
		if (savecrd) saveCrd(credInfo)
	} else {
		paste("Credentials for: ", url)
		usr <- readline(paste("username: \n"))
		pswd <- readline(paste("password: \n"))
		credInfo <- data.frame(url = url, user = usr, password = pswd, stringsAsFactors = FALSE)
		if (savecrd) saveCrd(credInfo)
	}
	return(credInfo)
}


# list name of the unique products
getProducts <- function(product){
	pp <- .humanize()
	if (length(product) > 1){
		product <- paste0(product, collapse = "|")
	}
	pp <- pp[grep(product, pp$short_name), ]
	return(unique(pp$short_name))
}

