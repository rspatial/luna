# Functions to help with downloading data
# Authors: Aniruddha Ghosh, Robert J. Hijmans, Alex Mandel
# July 2019
# Version 0.1
# Licence GPL v3

.download <- function(aurl, filename) {
	fn <- paste(tempfile(), ".download", sep="")
	res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
	if (res == 0) {
		w <- getOption("warn")
		on.exit(options("warn" = w))
		options("warn"=-1) 
		if (! file.rename(fn, filename) ) { 
			# rename failed, perhaps because fn and filename refer to different devices
			file.copy(fn, filename)
			file.remove(fn)
		}
	} else {
		stop("could not download the file" )
	}
}


.getCleanPath <- function(path) {
	
	if (missing(path)) {
		path <- tempdir()
	} 
  
  if (path == "") {
    path <- tempdir()
  } 
  
  path <- trimws(path)
  
  if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
		p <- substr(path, 1, nchar(path)-2)		
	} else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
		p <- substr(path, 1, nchar(path)-1)
	} else {
		p <- path
	}
	if (!file.exists(p) & !file.exists(path)) {
		stop('path does not exist: ', path)
	}
	if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
		path <- paste(path, "/", sep="")
	}
	return(path)
}




.getExtent <- function(aoi) {
  # Return the extent of an Area Of Interest in a character format for Web API requests
	if (!is.vector(aoi)) {
		if (inherits(aoi, 'SpatRaster')) {
			aoi <- ext(aoi)
		} else if (inherits(aoi, 'SpatVector')) {
			aoi <- ext(aoi)
		} else {
			aoi <- extent(aoi) #TODO remove raster dependency
		}
		aoi <- as.vector(aoi)
	} else {
		stopifnot(length(aoi) == 4)
	}	
	v <- as.vector(t(matrix(aoi, ncol=2)))
	paste(v, collapse=",")
}


split_landsat <- function(scene){
  # Split a Landsat scene ID into components
  
  scene <- "LC08_L1TP_167056_20190517_20190521_01_T1"
  scene_parts <- unlist(strsplit(scene, "_"))
  path <- substr(scene_parts[3],1,3)
  row <- substr(scene_parts[3],4,6)
  sensor <- scene_parts[1]
  collection <- scene_parts[6] # Not sure if this is the folder part
  
  return(c(collection=collection,path=path,row=row,sensor=sensor))
}

.find_aws <- function(scene, bands=seq(1:11)){
  # Given a Landsat scene ID generate the matching AWS public link to the files
  # https://landsatonaws.com/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1
  # http://landsat-pds.s3.amazonaws.com/c1/L8/168/060/LC08_L1TP_168060_20190625_20190705_01_T1/LC08_L1TP_168060_20190625_20190705_01_T1_B1.TIF
  # scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  
  #Stop if scene not provided
  
  root_url <- "landsat-pds.s3.amazonaws.com"
  scene = "LC08_L1TP_167056_20190517_20190521_01_T1"
  url_parts <- split_landsat(scene)
  
  band_names <- paste0("_B", bands,".TIF")
  
  final_urls <- file.path(root_url,
                          "c1",
                          "L8", 
                          url_parts["path"], 
                          url_parts["row"], 
                          scene, 
                          paste0(scene, band_names)
  )
  
  return(final_urls)
}