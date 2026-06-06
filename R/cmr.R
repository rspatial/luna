# Authors: Alex Mandel, Aniruddha Ghosh, Robert J. Hijmans 
# July 2019
# Version 0.1
# Licence GPL v3

# Converted from the NASA official pyCMR
# https://github.com/nasa/pyCMR

#AUTH_HOST = 'urs.earthdata.nasa.gov'


.get_search_results <- function(url, limit, kwargs){
  #  Search the CMR granules
  #:param limit: limit of the number of results
  #:param kwargs: search parameters
  #:return: list of results (<Instance of Result>)
  
	page_num <- 1
	results <- NULL

	while (length(results) < limit){
    #print(page_num)
		response <- httr::GET(
			url=url,
		# TODO: fix next line to take all possible args passed via ...
			httr::add_headers(Accept="text/csv"),
			query=c(kwargs, page_num=page_num) 
		#, page_size=self._PAGE_SIZE),
		# headers=self._SEARCH_HEADER # what is the header passed from parent function?
		)
    
		# Check for a valid response
		httr::stop_for_status(response)
    
		#unparsed_page = content(response,parsed="application/json")
		# parsing without messages
		# http://r.789695.n4.nabble.com/httr-content-without-message-td4747453.html
		if (httr::http_type(response) == "text/csv"){
      
		# Per httr docs testing for expected type and parsing manually
		#unparsed_page = readr::read_csv(httr::content(response, as="text"))
			p <- utils::read.csv(text=httr::content(response, as="text"), check.names=FALSE, stringsAsFactors=FALSE)
    
			#Check the URL column is not empty
			catcher <- tryCatch(urls <- p[["Online Access URLs"]], error=function(e){e})
  
			if(!inherits(catcher, "error")){
				if(length(urls)==0){
					break
				}
				# Append the full table of results
				results <- rbind(results, p)
				page_num <- page_num + 1
			} else { 
				break
			}  
		} else {
		#The response was not a csv, we should throw and error?
			break
		}
	}
    return(results)
}


.searchCollection <- function(cmr_host="https://cmr.earthdata.nasa.gov", limit=100, ...){
  # Search the CMR collections
  # :param limit: limit of the number of results
  # :param kwargs ...: search parameters
  # :return: dataframe of results
	SEARCH_COLLECTION_URL = paste0(cmr_host,"/search/collections")
	results <- .get_search_results(url=SEARCH_COLLECTION_URL, limit=limit, ...)
	return(results)
}

.cmr_outfile <- function(url, path) {
  # Strip URL query/fragment so signed S3 URLs (?X-Amz-...) do not leak into filenames
	clean <- sub("[?#].*$", "", url)
	file.path(path, basename(clean))
}

.cmr_auth_error <- function(msg, status = NA_integer_) {
	structure(
		class = c("luna_auth_error", "error", "condition"),
		list(message = msg, call = sys.call(-1), status = status)
	)
}

.cmr_validate_response <- function(f, outfile) {
  # Returns NULL on success; an error condition on failure.
	status <- httr::status_code(f)
	hdrs   <- httr::headers(f)
	ctype  <- hdrs[["content-type"]]
	if (status == 401 || status == 403) {
		return(.cmr_auth_error(
			sprintf("Earthdata authentication failed (HTTP %d). Check your `username`/`password` and that you have accepted the data product's EULA at https://urs.earthdata.nasa.gov/.", status),
			status = status))
	}
	if (status != 200) {
		return(simpleError(sprintf("HTTP %d for %s", status, f$url)))
	}
	if (!is.null(ctype) && grepl("html", ctype, ignore.case = TRUE)) {
		return(simpleError(sprintf("server returned an HTML page (likely a login or error page) instead of data for %s", f$url)))
	}
	if (file.exists(outfile) && isTRUE(file.info(outfile)$size < 1)) {
		return(simpleError(sprintf("downloaded file is empty: %s", outfile)))
	}
	NULL
}

.cmr_download_one <- function(url, path, USERNAME, PASSWORD, overwrite, cookie_file, netrc_file, verbose=TRUE, ...){
  # Download a single granule. Validates the response so that an HTML auth/error
  # page is never silently saved under a .hdf filename (issue #38).
	outfile <- .cmr_outfile(url, path)

  # If a previous run saved a tiny file (almost certainly a stale auth-failure
  # page), retry it even when overwrite=FALSE.
	if (file.exists(outfile) && !overwrite) {
		fsz <- file.info(outfile)$size
		if (isTRUE(fsz < 1024)) {
			file.remove(outfile)
		} else {
			return(outfile)
		}
	}

	if (!is.null(USERNAME)) {
		cfg <- httr::config(
			netrc = TRUE,
			netrc_file = netrc_file,
			followlocation = TRUE,
			ssl_verifypeer = 0,
			cookiefile = cookie_file,
			cookiejar = cookie_file
		)
		args <- list(url, cfg, httr::write_disk(outfile, overwrite = TRUE))
		if (verbose) args <- c(args, list(httr::progress()))
		f <- do.call(httr::GET, args)

		err <- .cmr_validate_response(f, outfile)
		if (!is.null(err)) {
			if (file.exists(outfile)) file.remove(outfile)
			stop(err)
		}
	} else {
		f <- utils::download.file(url, outfile, mode = "wb", quiet = !verbose)
		return(f)
	}
	outfile
}


.cmr_download <- function(urls, path, username, password, overwrite, verbose=TRUE, ...){
  # Given a list of results, download all of them. Aborts on the first
  # authentication failure so we do not produce N broken files.

	files <- rep("", length(urls))
	cookie_file <- tempfile("luna_cookies_", fileext = ".txt")
	file.create(cookie_file)
	netrc_file <- tempfile("luna_netrc_",  fileext = ".txt")
	writeLines(
		paste("machine urs.earthdata.nasa.gov login", username, "password", password),
		netrc_file
	)
	on.exit(file.remove(c(netrc_file, cookie_file)), add = TRUE)

	for (i in seq_along(urls)) {
		f <- tryCatch(
			.cmr_download_one(urls[i], path, username, password, overwrite,
			                  cookie_file, netrc_file, verbose=verbose),
			luna_auth_error = function(e) e,
			error = function(e) e
		)
		if (inherits(f, "luna_auth_error")) {
		  # Same credentials apply to every URL, so further attempts will all fail.
			stop(conditionMessage(f), call. = FALSE)
		}
		if (inherits(f, "error")) {
			warning("failure: ", urls[i], ": ", conditionMessage(f), call. = FALSE)
		} else {
			files[i] = urls[i]
		}
	}
	if (verbose) cat("\n")
	return(files)
}

.searchGranules <- function(product, version=NULL, start_date, end_date, extent, limit=100, datesuffix = "T00:00:00Z", ...){
  #Search the CMR granules
  #:param limit: limit of the number of results
  #:param kwargs: search parameters
  #:return: urls
  
	e <- .getExtent(extent)
	  
	  # for testing validity
	start_date <- as.Date(start_date)
	end_date <- as.Date(end_date)
		
	temporal <- paste0(start_date, datesuffix, ",", end_date, datesuffix)
	  
	params <- list(
		short_name=product,	temporal=temporal, downloadable="true", bounding_box=e
	)
	params$version <- version

	pars <- list(...) 
	if (length(pars) > 0) {
		params <- c(params, pars)
	}
	  
	cmr_host="https://cmr.earthdata.nasa.gov"
	url <- file.path(cmr_host, "search/granules")
	.get_search_results(url=url, limit=limit, kwargs=params)
}

# CMR download attempt
