# Functions for gettings data from Earth Explorer with regular login

# TODO: Move these globals into a package env
.ERS_MAIN_URL <- "https://ers.cr.usgs.gov"
.ERS_LOGIN_URL <- file.path(.ERS_MAIN_URL, "login/")
.ERS_LOGOUT_URL <- file.path(.ERS_MAIN_URL, "logout")
.ERS_DOMAIN <- "https://usgs.gov"
.ERS_HANDLE <- httr::handle(.ERS_DOMAIN)

.verify <- function(response){
  # Verify a response by saving the html so a person can look at it
  tmp_html <- "tmp/check.html" 
  xml2::write_html(httr::content(response, as="parsed"), tmp_html)
  utils::browseURL(tmp_html)
}

.find_token <- function(){
  # Scrape a csrf token from the login page to allow login
  
  # We reset the cookies every time we login, to make sure we get a fresh session.
  httr::handle_reset(.ERS_DOMAIN)
  
  response <- httr::GET("https://ers.cr.usgs.gov/", handle=.ERS_HANDLE)

  if (response$status_code == 200){
    # Search the response content for the csrf
    contents <- xml2::read_html(httr::content(response, as = "text"))
    inputs <- xml2::xml_find_all(contents, "//input")
    csrf <- xml2::xml_attr(inputs[grep("csrf_token", inputs)], "value")
    return(csrf)
    
  } else {
    print("Web Page not working try again later")
    return(NULL)
  }

}

.login_ers <- function(user, passw){
  # Login to USGS ERS system and return an authentication cookie
  csrf <- .find_token()
  
  params <- list(
    csrf_token = csrf,
    username = user,
    password = passw
  )
  
  response <- httr::POST(.ERS_LOGIN_URL, body=params, encode="form", handle = .ERS_HANDLE)
  
  if (response$status_code == 200){
    html <- httr::content(response, as="text")
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
  response <- httr::GET(.ERS_LOGOUT_URL, handle = .ERS_HANDLE)
  httr::handle_reset(.ERS_DOMAIN)
  return(response)
}

find_durls_ers <- function(scene_browse){
  # Scrape the Download URL for the actual urls to files
  # TODO: How do we want to take in the items that need to found, results of CMR or EE search 
  #list_url <- "https://earthexplorer.usgs.gov/download/external/options/LANDSAT_8_C1/LC81710582019149LGN00/INVSVC/"
  response <- httr::GET(scene_browse)
  if (response$status_code == 200){
    # Search the response content for the csrf
    contents <- xml2::read_html(httr::content(response, as = "text"))
    inputs <- xml2::xml_find_all(contents, "//div[@id='optionsPage']/div/div/input")
    onclicks <- xml2::xml_attr(inputs, "onclick")
    durls <- gsub("'","",sapply(strsplit(onclicks, "="), `[`, 2))
    # Usually there is more than 1 file, the user needs to decide which files they want
    return(durls)
  }
}

.get_files_ers <- function(scene_url, path="", ...){
  # Download a scene from Earth Explorer, must call .login_ers before using
  #scene_url <- "https://earthexplorer.usgs.gov/download/14320/LC08_CU_002008_20190503_C01_V01/BT/EE"
  
  tmp <- tempfile()
  
  response <- httr::GET(scene_url, httr::progress(), httr::write_disk(tmp), handle = .ERS_HANDLE)
  
  # Get the file type, size, and name from the header
  filename <- unlist(strsplit(httr::headers(response)$`content-disposition`, "="))[2]
  size_check <- httr::headers(response)$`content-length`
  content_check <- httr::http_type(response) #Note a zip from USGS could mean a .tar or tar.gz
  # TODO: check the size and content
  
  fullpath <- "/tmp" # The outpu directory, should come from user.
  final_path <- file.path(path, filename)
  
  # Download to tmp directory, rename file to correct name upon moving to final location
  file.rename(tmp, final_path )
  
  return(final_path)
}

download_ers <- function(scenes, path, overwrite, ...){
  # Downloads the files from ERS using basic account login
  
  # Make sure to get the urls with the find_durls_ers function
  
  # Should credentials be passed in?
  cred <- getCredentials(url=.ERS_MAIN_URL, path)
  
  # login an get a session
  .login_ers(user=cred$user, passw=cred$password)
  
  # TODO: Wrap in a Try to catch errors from downloads
  outfiles <- lapply(durls, .get_files_ers)
  
  # logout of the session
  .logout_ers()
  
  return(outfiles)
}