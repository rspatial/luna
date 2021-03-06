# Functions to help with downloading data
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

.download <- function(aurl, filename) {
	fn <- paste(tempfile(), ".download", sep="")
	res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption("warn")
		on.exit(options("warn" = w))
		options("warn"=-1) 
		if (! file.rename(fn, filename) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop("could not download the file" )
	}
}


.getPath <- function(path) {
	path <- trimws(path)
	stopifnot(dir.exists(path))
	return(path)
}




.getExtent <- function(aoi) {
  # Return the extent of an Area Of Interest in a character format for Web API requests
	if (inherits(aoi, "SpatExtent") || inherits(aoi, "Extent")) {
		aoi <- as.vector(aoi)
	} else if (!is.vector(aoi)) {
		aoi <- as.vector(ext(aoi))
	} else if (length(aoi) != 4) {
		stop("not a valid aoi")
	}
	# xmin, ymin, xmax, ymax
	b <- as.vector(t(matrix(aoi, ncol=2)))
	paste(b, collapse=",")
}

