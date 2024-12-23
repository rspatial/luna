# List all AVHRR files available for download
# This could be an internal function to download AVHRR files

.listAVHRR <- function(path, update=FALSE, baseurl) {
	# Two-day delay in ingestion
	filename <- paste0("avhrr_files_", Sys.Date(),".rds")
	filename <- file.path(path, filename)
	
	if (!file.exists(filename) | update){
		message("Creating index of available AVHRR files on", as.character(Sys.Date()))
		startyear <- 1981
		endyear <- format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y")
		years <- seq(startyear, endyear)
		
		ff <- list()
		for (i in 1:length(years)){
			url <- file.path(baseurl, years[i])
			wp <- xml2::read_html(url)
			dvns <- rvest::html_attr(rvest::html_nodes(wp, "a"), "href")
			ds <- grep("^AVHRR-Land_*.*.nc", dvns, value = TRUE)
			ff[[i]] <- ds
		}
		ff <- unlist(ff)
		dates <- sapply(strsplit(ff,"_"), "[[", 5)
		dates <- as.Date(dates, format = "%Y%m%d")
		ff <- data.frame(filename = ff, date = dates, stringsAsFactors = FALSE, row.names = NULL)
		saveRDS(ff, filename)
	} else {
		ff <- readRDS(filename)
	}
	return(ff)
}


getAVHRR <- function(start_date, end_date, path, download=FALSE, overwrite=FALSE, update=FALSE, quiet=FALSE, ...) {
	
	if(missing(start_date)) stop("provide a start_date")
	if(missing(end_date)) stop("provide an end_date")
	
	baseurl <- "https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access"
	# url to access 8 different ways of downloading the data
	# baseurl <- "https://www.ncei.noaa.gov/thredds/catalog/cdr/ndvi/files"
	path <- .getPath(path)
	
	# list of AVHRR files
	pp <- .listAVHRR(path=path, baseurl=baseurl, update=FALSE)
	
	# TODO: alternate search through CMR
	# https://cmr.earthdata.nasa.gov/search/concepts/C1277746140-NOAA_NCEI
	
	# subset the files by dates
	pp <- pp[pp$date >= start_date & pp$date <= end_date, ]
	
	if(nrow(pp) == 0) {
		warning("No AVHRR file available for the date range provided")
		return(NULL)
	}
	if (!download) return(pp$filename)

	# to store output file names
	
	failed <- rep(FALSE, nrow(pp))

	year <- .yearFromDate(pp$date)
	furl <- file.path(baseurl, year, pp$filename)
	filenames <- file.path(path, pp$filename)

	for (i in 1:nrow(pp)){
		
		# is ok, if file exists or overwrite is TRUE
		ok <- file.exists(filenames[i]) && (!overwrite)
		
		if (file.exists(filenames[i])) {
			x <- try(rast(filenames[i]), silent=TRUE)
			if (inherits(x, "try-error")) {
				ok <- FALSE
			}
		}
		
		if (!ok) {
			if (!quiet) message(pp$filename[i])
			ff <- try(utils::download.file(furl[i], filenames[i], mode="wb", quiet=quiet, ...)) 
			if (inherits(ff, "try-error")) {
			failed[i] <- TRUE
			}
		}
	}
	if (any(failed)) {
		message(paste("download failed for", sum(failed), "file(s)"))
	}
	filenames
}

# TODO
# parallel download
# processAVHRR <- function(ff){
#	 ndvi <- rast(ff, varname = "NDVI")
#	 qa <- rast(ff, varname = "QA")
#	 # Quality unpack --- page 29 of 
#	 # https://www1.ncdc.noaa.gov/pub/data/sds/cdr/CDRs/Normalized%20Difference%20Vegetation%20Index/AlgorithmDescription_01B-20b.pdf
# }
# Create composite
# Should it be a general reduce-type function