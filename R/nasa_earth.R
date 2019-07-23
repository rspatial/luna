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
  
  #logging.info("======== Waiting for response ========")
  
  page_num <- 1
  results <- vector('character')  #is this needed?
  while (length(results) < limit){
    # Debug
    print(page_num)
    response <- httr::GET(
      url=url,
      # TODO: fix next line to take all possible args passed via ...
      add_headers(Accept="text/csv"),
      query=c(kwargs, page_num=page_num) #, page_size=self._PAGE_SIZE),
      #headers=self._SEARCH_HEADER # what is the header passed from parent function?
    )
    
    # Check for a valid response
    stop_for_status(response)
    
    #unparsed_page = content(response,parsed="application/json")
    unparsed_page = content(response)
    
    #TODO: suppress parsing message
    catcher <- tryCatch(urls <- unparsed_page$`Online Access URLs`,error=function(e){e})
  
    if(!inherits(catcher, "error")){
      if(length(urls)==0){
        break
      }
      results <- c(results,urls)
      page_num <- page_num+1
    } else { 
      break
    }
    
  }
  return(results)
  
}


searchCollection <- function(cmr_host="https://cmr.earthdata.nasa.gov", limit=100, ...){
  # Search the CMR collections
  # :param limit: limit of the number of results
  # :param kwargs ...: search parameters
  # :return: dataframe of results
  SEARCH_COLLECTION_URL = paste0(cmr_host,"/search/collections")
  results <- .get_search_results(url=SEARCH_COLLECTION_URL, limit=limit, ...)
  return(results)
}

.cmr_download_one <- function(url, path, USERNAME, PASSWORD, overwrite, ...){
  # Download a single result
  # TODO verify outdir exists if not make folder
  # TODO check if file exists
  ofile <- paste0(path,basename(url))
  if (!file.exists(ofile) | overwrite){
    file <- GET(url, authenticate(USERNAME, PASSWORD), progress(), write_disk(ofile, overwrite = overwrite)) 
  }
} 


cmr_download <- function(urls, path, username=NULL, password=NULL, overwrite, ...){
  # Given a list of results, download all of them
  # TODO allow in parallel
  # TODO re-use a session
  
  files <- rep("", length(urls))
  for (i in 1:length(urls)) {
    files <- tryCatch(.cmr_download_one(urls[i], path, 
                                        USERNAME = username, PASSWORD = password,
                                        overwrite = overwrite), 
                      error = function(e){e})
    if (inherits(files, "error")) {
      warning("failure:", urls[i])
    } else {
      files[i] = x
    }
  }
  return(files)
}

searchGranules <- function(product="MOD09A1", start_date, end_date, extent, limit=100, datesuffix = "T00:00:00Z", ...){
  #Search the CMR granules
  #:param limit: limit of the number of results
  #:param kwargs: search parameters
  #:return: dataframe of results
  
  e <- as.vector(t(matrix(as.vector(extent), ncol=2)))
  e <- paste0(e, collapse=",")
  
  # for testing validity
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
    
  temporal <- paste0(start_date, datesuffix, ",", end_date, datesuffix)
  
  params <- list(
    short_name=product,	temporal=temporal,
	  downloadable="true", bounding_box=e
  )
  
  pars <- list(...)
  
  if (length(pars) > 0) {
	  params <- c(params, pars)
  }
  
  cmr_host="https://cmr.earthdata.nasa.gov"
  url <- paste0(cmr_host,"/search/granules")
  results <- .get_search_results(url=url, limit=limit, kwargs=params)
  return(results) 
}

# CMR download attempt
