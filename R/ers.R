library(httr)
library(xml2)

LOGIN_URL <- "https://ers.cr.usgs.gov/login"

find_token <- function(){
  # Scrape a csrf token from the login page to allow login
  response <- httr::GET(LOGIN_URL)

  if (response$status_code == 200){
    # Search the response content for the csrf
    contents <- content(response, as = "parsed", type = "text/html")
    inputs <- xml2::xml_find_all(contents, "//input")
    token <- xml_attr(inputs[grep("csrf_token", inputs)], "value")
    return(token)
    
  } else {
    print("Web Page not working try again later")
    return(NULL)
  }

}

login_ers <- function(user, passw, token){
  # Login to USGS ERS system and return an authentication cookie
  params <- list(
    username = user,
    password = passw,
    csrf_token = token
  )
  #reponse <- httr::POST(LOGIN_URL, data=params, verbose())
  reponse <- httr::POST(LOGIN_URL, data=params, verbose())
  
}

find_url <- function(){
  # Scrape the Download URL for the actual urls to files
}

get_files <- function(){
  # Download a scene from Earth Explorer
}