# Functions for getting data from Earth Explorer API
# Requires machine to machine API approval
# https://ers.cr.usgs.gov/profile/access

library(httr)
library(jsonlite)
#library(sf)

URL <- "https://earthexplorer.usgs.gov/inventory/json/v/1.4.0"
LOGIN_URL <- file.path(URL, "login")
LOGOUT_URL <- file.path(URL, "logout")
SEARCH_URL <- file.path(URL, "search")
DOWNLOADOPT_URL <- file.path(URL, "downloadoptions")

.make_params <- function(params){
  # EE API requires a very specific format for parameters to the api, 
  # json inside a query object or body
  json_params <- paste("jsonRequest=",toJSON(params, auto_unbox = TRUE),sep="")
  return(json_params)
}

.login_ee <- function(USERNAME, PASSWORD){
  # Login and return the apiKey auth token
  params <- list(
    username = USERNAME,
    password = PASSWORD,
    catalogId = "EE"
  )
  auth <- httr::POST(LOGIN_URL, body=.make_params(params), content_type("application/x-www-form-urlencoded"))
  
  #TODO: Check the http status
  token <- content(auth)$data  
  return(token)
}

.logout_ee <- function(token){
  # Logout

  params <- list(
    apiKey=token
  )
  
  logout <- httr::GET(LOGOUT_URL, query=.make_params(params))
  #TODO: Check the http status
  # TODO: If error api_version is 1.3.0, if works 1.4.0
}

.get_ee_downurl <- function(datasetName, entityIds, token){
  # Queries EE for the what the actual download url of a particular file is.
  params <- list(
    datasetName = "ARD_TILE",
    entityIds = "LC08_CU_004010_20140101_C01_V01",
    apiKey = token
  )
  
  durl <- httr::GET(DOWNLOADOPT_URL, query=.make_params(params))
  #durl_post <- httr::POST(DOWNLOADOPT_URL, body=.make_params(params), content_type("application/x-www-form-urlencoded"), verbose(info=TRUE, ssl=TRUE))
  
  djson <- .is_json(durl)
  record <- djson[["data"]][[1]][["downloadOptions"]]
  list_results <- lapply(record, .jsonlist_sf)
  results <- do.call(rbind, list_results)
  
  #results$url will get you the url, however check the $available first
  return(results)
}


.download_ee <- function(fileurl, token){
  # Try downloading a file given the downloadOptions$url from get_ee_downurl
  
  params <- list(
    apiKey=token
  )
  
  #fileurl <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  #check <- httr::POST(fileurl, body=json_params, content_type("application/x-www-form-urlencoded"), httr::progress())
  # TODO: Set the file name based on the scene name, and the content type in the headers
  name_file <- "/tmp/LC08api.tar"
  check <- httr::GET(fileurl, 
                     #verbose(info=TRUE),
                     query=.make_params(params), 
                     httr::progress(), 
                     httr::write_disk(name_file)
                     )
  # TODO: verify the size of the file
  return(name_file)
}


.search_ee <- function(token, datasetName, ){
  # Search Earth Explorer for data
  
  # TODO: implement a function to build up the temporal, spatial and other filters to the correct format
  params <- list(
    apiKey = token, 
    datasetName = "ARD_TILE",
    temporalFilter = list(
      startDate= "2014-01-01",
      endDate= "2014-02-01"
    ),
    maxResults = 5
  )
  
  # GET works now that the query is used to send params
  sdata <- httr::GET(SEARCH_URL,
                     #verbose(info=TRUE),
                     query=.make_params(params)
  )
  # POST works
  #sdata <- httr::POST(search_url, body=json_params, content_type("application/x-www-form-urlencoded"), verbose(info=TRUE, ssl=TRUE))
  
  # TODO: check if download is TRUE, otherwise might need to submit an order
  
  # TODO: Return the entityID to be passed to downloadoptions
  djson <- .is_json(sdata)
  records <- djson[["data"]][["results"]]
  list_results <- lapply(records, .jsonlist_sf)
  #entities <- sapply(records, function(x){ x[["entityId"]]})
  results <- do.call(rbind, list_results)
  
  # Use the $entityId for the next step
  return(results)
}

.is_json <- function(response){
  #Check that a response is json and parse it
  if (httr::http_type(response) == "application/json")  {
    parsed <- httr::content(response, as="parsed")    
  } else {
    print("Response was not json")
    # TODO: handle things like errors.
  }
}

.jsonlist_sf <- function(jslist){
  # Give a nested list item, convert to a single dataframe row
  items <- names(jslist)
  footprint_sel <- items=="spatialFootprint"
  # TODO: Convert footprint to spatial object if you want to know the actual geom
  dfitems <- as.data.frame(t(jslist[!footprint_sel]), stringsAsFactors = FALSE)
  
  return(dfitems)
}

