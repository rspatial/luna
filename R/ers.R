library(httr)
library(xml2)

LOGIN_URL <- "https://ers.cr.usgs.gov/login"

find_token <- function(){
  # Scrape a csrf token from the login page to allow login
  response <- httr::GET(LOGIN_URL, verbose())

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
  params <- list(
    username = user,
    password = passw,
    csrf_token = find_token()
  )
  response <- httr::POST(LOGIN_URL, body=params, encode="form", verbose(info=TRUE, ssl=TRUE))
  
  # Verify
  #xml2::write_html( content(response, as="parsed"), "local_test/check.html")
  
}

find_url <- function(scene){
  # Scrape the Download URL for the actual urls to files
}

get_files <- function(scene_url){
  # Download a scene from Earth Explorer
  scene_url <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  response <- httr::GET(scene_url, httr::progress(), httr::write_disk("/tmp/LC08api.tar"))
  
}