library(httr)
library(xml2)

LOGIN_URL <- "https://ers.cr.usgs.gov"
LOGOUT_URL <- "https://ers.cr.usgs.gov/logout"
HANDLE <- httr::handle("https://usgs.gov")

.verify <- function(response){
  # Verify a response by saving the html so a person can look at it
  xml2::write_html( content(response, as="parsed"), "local_test/check.html")
  browseURL("local_test/check.html")
}

find_token <- function(){
  # Scrape a csrf token from the login page to allow login
  
  #TODO: not reuse an existing session, since we need a fresh csrf token
  response <- httr::GET("https://ers.cr.usgs.gov", verbose(), config(forbid_reuse = 1))

  if (response$status_code == 200){
    # Search the response content for the csrf
    contents <- content(response, as = "parsed", type = "text/html")
    inputs <- xml2::xml_find_all(contents, "//input")
    csrf <- xml_attr(inputs[grep("csrf_token", inputs)], "value")
    return(csrf)
    
  } else {
    print("Web Page not working try again later")
    return(NULL)
  }

}

login_ers <- function(user, passw, csrf){
  # Login to USGS ERS system and return an authentication cookie
  csrf <- find_token()
  
  params <- list(
    username = user,
    password = passw,
    csrf_token = csrf
  )
  response <- httr::POST(LOGIN_URL, body=params, encode="form", verbose(info=TRUE, ssl=TRUE), handle = HANDLE)
  #response <- httr::POST(LOGIN_URL, body=params, handle = HANDLE, verbose(info=TRUE, ssl=TRUE))
}

logout_ers <- function(){
  # Logout function
  response <- httr::GET(LOGOUT_URL, verbose(), handle = HANDLE)
}

find_url <- function(scene){
  # Scrape the Download URL for the actual urls to files
}

get_files <- function(scene_url){
  # Download a scene from Earth Explorer
  # How to reuse the session cookie? set a handle?
  scene_url <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  response <- httr::GET(scene_url, httr::progress(), httr::write_disk("/tmp/LC08api2.tar"), verbose(), handle = HANDLE)
  
}