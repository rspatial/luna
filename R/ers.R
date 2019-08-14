# Functions for gettings data from Earth Explorer with regular login

library(httr)
library(xml2)

MAIN_URL <- "https://ers.cr.usgs.gov"
LOGIN_URL <- file.path(MAIN_URL, "login/")
LOGOUT_URL <- file.path(MAIN_URL, "logout")
DOMAIN <- "https://usgs.gov"
HANDLE <- httr::handle(DOMAIN)

.verify <- function(response){
  # Verify a response by saving the html so a person can look at it
  tmp_html <- "tmp/check.html" 
  xml2::write_html( content(response, as="parsed"), tmp_html)
  browseURL(tmp_html)
}

.find_token <- function(){
  # Scrape a csrf token from the login page to allow login
  
  # We reset the cookies every time we login, to make sure we get a fresh session.
  handle_reset(DOMAIN)
  
  response <- httr::GET("https://ers.cr.usgs.gov/", verbose(), handle=HANDLE)

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

.login_ers <- function(user, passw, csrf){
  # Login to USGS ERS system and return an authentication cookie
  csrf <- find_token()
  
  params <- list(
    csrf_token = csrf,
    username = user,
    password = passw
  )
  
  response <- httr::POST(LOGIN_URL, body=params, encode="form", handle = HANDLE)
  
  if (response$status_code == 200){
    html <- content(response, as="text")
    # Check that it logged in by looking for the Sign Out button
    if (grep("Sign Out", html)){
      
      return(TRUE)
    } else {
      print("Login failed, please verify you have a working account.")
      return(FALSE)
    }
  } else {
    print("Please verify the website is up.")
    return(FALSE)
  }
  
}

.logout_ers <- function(){
  # Logout function
  response <- httr::GET(LOGOUT_URL, verbose(), handle = HANDLE)
}

find_durls <- function(scene_browse){
  # Scrape the Download URL for the actual urls to files
  # TODO: How do we want to take in the items that need to found, results of CMR or EE search 
  #list_url <- "https://earthexplorer.usgs.gov/download/external/options/LANDSAT_8_C1/LC81710582019149LGN00/INVSVC/"
  response <- httr::GET(scene_browse)
  if (response$status_code == 200){
    # Search the response content for the csrf
    contents <- content(response, as = "parsed", type = "text/html")
    inputs <- xml2::xml_find_all(contents, "//div[@id='optionsPage']/div/div")
    onclicks <- xml_attr(inputs, "onclick")
    durls <- gsub("'","",sapply(strsplit(onclicks, "="), `[`, 2))
    # Usually there is more than 1 file, the user needs to decide which files they want
    return(durls)
  }
}

get_files <- function(scene_url){
  # Download a scene from Earth Explorer
  # TODO: Download to tmp directory, rename file to correct name upon moving to final location
  # TODO: get the file type and size from the header
  #scene_url <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  response <- httr::GET(scene_url, httr::progress(), httr::write_disk("/tmp/LC08api2-2.tar"), handle = HANDLE)
  
  size_check <- httr::headers(response)$`content-length`
  content_check <- httr::http_type(response) #Note a zip from USGS could mean a .tar or tar.gz
  
  # TODO: return the path to the actual files final location
  return(TRUE)
}
