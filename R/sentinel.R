# download data provided by the Copernicus Global Land Service
# based on code by Willemijn Vroege, ETH Zurich. (Last update  : 12.06.2020)

getCGLS <- function(product, start_date, end_date, resolution="1km", version=1, path, username, password, ...) {

	stopifnot(resolution %in% c("1km", "300m", "100m"))
	if (resolution == "300m") {
		resolution <- "333m"
		product <- paste0(product, "300")
	}
	version <- paste0("v", version)
	collection <- paste(product, version, resolution, sep="_")
	path <- file.path(path, collection)
	dir.create(path, FALSE, FALSE)

# data provided in the data manifest of the Copernicus service.

	manif <- paste0("https://land.copernicus.vgt.vito.be/manifest/", collection, "/manifest_cgls_", collection, "_latest.txt" )
	urls <- readLines(manif)
	if (any(grepl("does not exist", urls))) {
		stop("This product is not available or misspecified")
	}
	
#	urls <- paste0("https://", paste(username, password, sep=":"), "@", sub(".*//", "", urls))
	urls <- gsub("^https://", paste0("https://", username, ":", password, "@"), urls)
	
	d <- tolower(basename(urls))
	d <- sapply(strsplit(d, paste0(tolower(product), "_")), function(i)i[2])
	d <- sapply(strsplit(d, "_"), function(i) substr(i[1], 1, 8))
	d <- as.Date(d, "%Y%m%d")

	start_date <- as.Date(start_date)
	end_date <- as.Date(end_date)
	i <- d >= start_date & d <= end_date
	urls <- urls[i]
	if (length(urls) == 0) {
		stop("no data found for this time period")
	}

	outf <- file.path(path, basename(urls))
	for (i in 1:length(urls)) {
		utils::download.file(urls[i], outf[i], mode = "wb")
	}
	outf
}



#start_date  <- "2019-06-01"
#end_date <- "2019-06-15"
#product <- "fapar" 
#resolution <- "1km" #1km, 300m or 100m
#version = 1
#getCGLS("fapar", "2019-06-01", "2019-06-15", resolution="1km", version=1, ".", username, password)

