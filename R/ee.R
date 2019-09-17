# Functions for getting data from Earth Explorer API
# Requires machine to machine API approval
# https://ers.cr.usgs.gov/profile/access
# Documnentation of API (requires login)
# https://earthexplorer.usgs.gov/inventory/documentation/json-api

.EE_API_URL <- "https://earthexplorer.usgs.gov/inventory/json/v/1.4.0"
.EE_API_LOGIN_URL <- file.path(.EE_API_URL, "login")
.EE_API_LOGOUT_URL <- file.path(.EE_API_URL, "logout")
.EE_API_SEARCH_URL <- file.path(.EE_API_URL, "search")
.EE_API_DOWNLOADOPT_URL <- file.path(.EE_API_URL, "downloadoptions")

.make_params <- function(params){
  # EE API requires a very specific format for parameters to the api, 
  # json inside a query object or body
  json_params <- paste("jsonRequest=",jsonlite::toJSON(params, auto_unbox = TRUE),sep="")
  return(json_params)
}

.login_ee <- function(USERNAME, PASSWORD){
  # Login and return the apiKey auth token
  params <- list(
    username = USERNAME,
    password = PASSWORD,
    catalogId = "EE"
  )
  auth <- httr::POST(.EE_API_LOGIN_URL, body=.make_params(params), httr::content_type("application/x-www-form-urlencoded"))
  
  #TODO: Check the http status
  # If you ar approved 
  auth_data <- .is_json(auth)
  if (!(auth_data$access_level == "approved")){
      print("You need API access approval, opening website to request")
      utils::browseURL("https://ers.cr.usgs.gov/profile/access")
      # TODO: return error and exit
      # Other possible checks, api version is 1.3.0, access_level is guest
  }
  token <- auth_data$data
  return(token)
}

.logout_ee <- function(token){
  # Logout

  params <- list(
    apiKey=token
  )
  
  logout <- httr::GET(.EE_API_LOGOUT_URL, query=.make_params(params))
  #TODO: Check the http status
  # TODO: If error api_version is 1.3.0, if works 1.4.0
}

.get_ee_downurl <- function(entityIds, datasetName, token){
  # Queries EE for the what the actual download url of a particular file is.
  params <- list(
    datasetName = "ARD_TILE",
    entityIds = "LC08_CU_004010_20140101_C01_V01",
    apiKey = token
  )
  
  durl <- httr::GET(.EE_API_DOWNLOADOPT_URL, query=.make_params(params))
  
  djson <- .is_json(durl)
  record <- djson[["data"]][[1]][["downloadOptions"]]
  list_results <- lapply(record, .jsonlist_sf)
  results <- do.call(rbind, list_results)
  
  #results$url will get you the url, however check the $available first
  return(results)
}


.download_ee <- function(fileurl, entityId, token, size){
  # Try downloading a file given the downloadOptions$url from get_ee_downurl
  
  params <- list(
    apiKey=token
  )
  
  #fileurl <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  #check <- httr::POST(fileurl, body=json_params, content_type("application/x-www-form-urlencoded"), httr::progress())
  # TODO: Set the file name based on the scene name, and the content type in the headers
  name_file <- file.path("/tmp", entityId) # TODO, how to know the extension
  check <- httr::GET(fileurl, 
                     #verbose(info=TRUE),
                     query=.make_params(params), 
                     httr::progress(), 
                     httr::write_disk(name_file)
                     )
  if (file.size(name_file)==size){
    # TODO: verify the size of the file
    # Move the file to the final destination
    # TODO: Many of these files will by .tar, should we open those?
  }

  # return the final save location of the file
  return(name_file)
}


.search_ee <- function(token, datasetName, ...){
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
  sdata <- httr::GET(.EE_API_SEARCH_URL,
                     #verbose(info=TRUE),
                     query=.make_params(params)
  )
  
  
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

download_ee <- function(product, entityIds_all, user, passw){
  # Given the Scenes are identified via CMR, lookup the url and download

  # TODO: Make sure you have a valid session token
  # They are good for about an hour without usage, otherwise they time out
  token <- .login_ee(USERNAME=user, PASSWORD=passw)
  
  # Then query the downloadOptions
  all_urls <- lapply(entityIds_all, .get_ee_downurl, datasetName = product, token = token)
  
  for (each in all_urls){
    .download_ee(fileurl = each$url, token = token, size = each$filesize)  
  }
  
}
