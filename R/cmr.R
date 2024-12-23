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

.cmr_download_one <- function(url, path, USERNAME, PASSWORD, overwrite, cookie_file, ...){
  # Download a single result
  # TODO check if file exists
# Make the request with cookies and follow redirects
	outfile <- file.path(path, basename(url))
	if ((!file.exists(outfile)) | overwrite){
		if(!is.null(USERNAME)){
			f <- httr::GET(url, 
                httr::add_headers(Cookie = readLines(cookie_file)),
                httr::set_cookies(file = cookie_file),
                httr::config(
					netrc = TRUE, 
					followlocation = TRUE, 
					ssl_verifypeer = 0
				),
				httr::progress(), 
				httr::write_disk(outfile, overwrite = overwrite)
			)
						
#			httr::authenticate(USERNAME, PASSWORD), httr::progress(), httr::write_disk(outfile, overwrite = overwrite))
		} else {
			f <- utils::download.file(url, outfile, mode = "wb") 
			return(f)
		}
	}
	return(outfile)
} 


.cmr_download <- function(urls, path, username, password, overwrite, ...){
  # Given a list of results, download all of them
  
	files <- rep("", length(urls))
	cookie_file <- file.path(tempdir(), "cookies.XXXXXXXXXX")
	writeLines("", cookie_file)
	netrc_file <- file.path(tempdir(), "netrc.XXXXXXXXXX")
	txt <- paste("machine urs.earthdata.nasa.gov login", username, "password", password)
	writeLines(txt, netrc_file)
	httr::set_config(httr::config(netrc_file = netrc_file))		
	on.exit(file.remove(c(netrc_file, cookie_file)))
	
	for (i in 1:length(urls)) {
		f <- tryCatch(
				.cmr_download_one(urls[i], path, username, password, overwrite, cookie_file, ...), 
				error = function(e){e}
			)
		if (inherits(f, "error")) {
			warning("failure: ", urls[i])
			f <- file.path(path, urls[i])
			if ( isTRUE(file.info(f)$size < 1) ) file.remove(f)
		} else {
			files[i] = urls[i]
		}
	}
	cat("\n")
	return(files)
}

.searchGranules <- function(product="MOD09A1", start_date, end_date, extent, limit=100, datesuffix = "T00:00:00Z", ...){
  #Search the CMR granules
  #:param limit: limit of the number of results
  #:param kwargs: search parameters
  #:return: dataframe of results
  
	e <- .getExtent(extent)
	  
	  # for testing validity
	start_date <- as.Date(start_date)
	end_date <- as.Date(end_date)
		
	temporal <- paste0(start_date, datesuffix, ",", end_date, datesuffix)
	  
	params <- list(
		short_name=product,	temporal=temporal, downloadable="true", bounding_box=e
	)
	  
	pars <- list(...) 
	if (length(pars) > 0) {
		params <- c(params, pars)
	}
	  
	cmr_host="https://cmr.earthdata.nasa.gov"
	url <- file.path(cmr_host, "search/granules")
	results <- .get_search_results(url=url, limit=limit, kwargs=params)
	return(results) 
}

# CMR download attempt
