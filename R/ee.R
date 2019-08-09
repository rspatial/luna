# Functions for getting data from Earth Explorer API
# Requires machine to machine API approval
# https://ers.cr.usgs.gov/profile/access

library(httr)
library(jsonlite)

URL <- "https://earthexplorer.usgs.gov/inventory/json/v/1.4.0"
LOGIN_URL <- file.path(url, "login")
LOGOUT_URL <- file.path(url, "logout")
SEARCH_URL <- file.path(url, "search")
DOWNLOADOPT_URL <- file.path(url, "downloadoptions")

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
  auth <- httr::POST(login_url, body=.make_params(params), content_type("application/x-www-form-urlencoded"))
  
  #TODO: Check the http status
  token <- content(auth)$data  
  return(token)
}

.logout_ee <- function(token){
  # Logout

  params <- list(
    apiKey=token
  )
  
  logout <- httr::GET(logout_url, query=.make_params(params))
  #TODO: Check the http status
  # TODO: If error api_version is 1.3.0, if works 1.4.0
}

.get_ee_downurl <- function(datasetName, entityIds, token){
  params <- list(
    datasetName = "ARD_TILE",
    entityIds = "LC08_CU_002008_20190503_20190523_C01_V01",
    apiKey = token
  )
  
  durl_get <- httr::GET(downloadopt_url, query=.make_params(params)) #Does this work?
  #durl_post <- httr::POST(downloadopt_url, body=json_params, content_type("application/x-www-form-urlencoded"), verbose(info=TRUE, ssl=TRUE))
}


.download_ee <- function(fileurl, token){
  # Try downloading a file
  
  params <- list(
    apiKey=token
  )
  
  fileurl <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  #check <- httr::POST(fileurl, body=json_params, content_type("application/x-www-form-urlencoded"), httr::progress())
  # TODO: Set the file name based on the scene name, and the content type in the headers
  check <- httr::GET(fileurl, 
                     #verbose(info=TRUE),
                     query=.make_params(params), 
                     httr::progress(), 
                     httr::write_disk("/tmp/LC08api.tar")
                     )
}


.search_ee <- function(token){
  # Search Earth Explorer for data
  
  # TODO: implement a function to build up the temporal, spatial and other filters to the correct format
  params <- list(
    apiKey = token, 
    datasetName = "ARD_TILE",
    temporalFilter = list(
      startDate= "2006-01-01",
      endDate= "2006-02-01"
    ),
    maxResults = 5
  )
  
  # GET works now that the query is used to send params
  sdata <- httr::GET(search_url,
                     #verbose(info=TRUE),
                     query=.make_params(params)
  )
  # POST works
  #sdata <- httr::POST(search_url, body=json_params, content_type("application/x-www-form-urlencoded"), verbose(info=TRUE, ssl=TRUE))
  
  # TODO: check if download is TRUE, otherwise might need to submit an order
}




