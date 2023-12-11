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
	aoi <- as.vector(ext(aoi))
	# xmin, ymin, xmax, ymax
	b <- as.vector(t(matrix(aoi, ncol=2)))
	paste(b, collapse=",")
}




.downloadDirect <- function(url, filename, unzip=FALSE, quiet=FALSE, mode="wb", cacheOK=FALSE, remove=TRUE,  ...) {
	if (!file.exists(filename)) {
		ok <- try(
			suppressWarnings(
				utils::download.file(url=url, destfile=filename, quiet=quiet, mode=mode, cacheOK=cacheOK, ...)), silent=TRUE
		)
		if (inherits(ok, "try-error")) {
			if (file.exists(filename)) file.remove(filename)
			message("download failed")
			return(FALSE)
		}
		if (!file.exists(filename)) {
			message("download failed")
			return(FALSE)
		}
	}
	if (unzip) {
		zok <- try(utils::unzip(filename, exdir=dirname(filename)), silent=TRUE)
		if (remove) try(file.remove(filename), silent=TRUE)
		if (inherits(zok, "try-error")) {
			message("download failed")
			return(FALSE)
		}
	}
	TRUE	
}	


.donwload_url <- function(url, filepath, ...) {
	if (!(file.exists(filepath))) {
		if (.downloadDirect(url, filepath, ...)) {
			r <- try(rast(filepath), silent=TRUE)
			if (inherits(r, "try-error")) {
				try(file.remove(filepath), silent=TRUE)
				message("download failed")
				return(NULL)
			}
		} else {
			return(NULL)
		}
	} else {
		r <- rast(filepath)
	}
	r
}

