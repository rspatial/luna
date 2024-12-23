# List and Download NASA products
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

modisExtent <- function(f=NULL, h, v) {
	r <- rast(xmin=-20015109, xmax=20015109, ymin=-10007555, ymax=10007555, nrow=18, ncol=36, crs="+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m")
	
	hv <- NULL
	if (!is.null(f)) {
		f <- gsub("hdf$", "", basename(f))
		f <- sapply(strsplit(f, "\\."), function(i) grep("^h", i, value=TRUE)[1])
		f <- unlist(strsplit(gsub("^h", "", f), "v"))
		f <- as.numeric(f)
		hv <- matrix(f, ncol=2, byrow=TRUE)
	} else {
		hv <- cbind(h, v)
	}
	
	hv[(hv[,1] < 0) | (hv[,1] > 35), 1] <- NA
	hv[(hv[,2] < 0) | (hv[,2] > 17), 2] <- NA
	hv <- hv + 1
	x <- lapply(1:nrow(hv), function(i) {
		if (any(is.na(hv[i,]))) {
			c(NA,NA,NA,NA)
		} else {
			as.vector(ext(r[hv[i,2], hv[i,1], drop=FALSE]))
		}
	})
	do.call(rbind, x)
}


getNASA <- function(product, start_date, end_date, aoi, version=NULL, download=FALSE, path, 
			username, password, server = "LPDAAC_ECS", limit = 100000, overwrite=FALSE, ...) {
 
	
	if(missing(product)) stop("provide a product name")
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	if(missing(aoi)) stop("provide an area of interest")

	h <- luna:::.humanize()
	h <- h[h$short_name == product, ]
	
	if (!is.null(version)) {
		pp <- h[h$version == version & h$provider == server, ]
	} else {
		pp <- h[h$provider == server, ]	
	}
	
	if (nrow(pp) < 1) {
		if (nrow(h) < 1) {
			stop("The requested product is not available for this through this function")
		} else {
			cat("Options for this product:\n")
			print(head(h, 10))
			cat("\n")
			stop("The requested product is not available for this product version or server")
		}
	} else if (nrow(pp) > 1) {
		warning("Multiple sources available, using first one")
		print(pp)
		pp <- pp[1, ]
	}
	  
  # find product urls, does not require credentials
	results <- .searchGranules(product = product, version=version, start_date = start_date, end_date = end_date, extent = aoi, limit = limit)
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
		if (!is.null(version)) {
			print(paste("No downloadable results found for this version:", version))
		} else {
			print("No results found")
		}
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
  #dates <- substr(basename(filename), 10, 16)
  dates <- .dateFromYearDoy(dates)
  dm <- format(dates, "%m")
  dy <- format(dates, "%Y")
  dd <- format(dates, "%d")
  data.frame(filename=filename, date=dates, year=dy, month=dm, day=dd, stringsAsFactors = FALSE)
}


getModis <- function(...) {
	warning("this method has been replaced by getNASA. It will be removed in future versions")
	getNASA(...)
}
