# download data provided by the Copernicus Global Land Service
# based on code by Willemijn Vroege, ETH Zurich. (Last update  : 12.06.2020)

getCGLS <- function(product, start_date, end_date, resolution="1km", version=1, path, username, password, ...) {

	stopifnot(resolution %in% c("1km", "300m", "100m"))
	if (resolution == "300m") {
		resolution <- "333m"
		product <- paste0(product, "300")
	}
	
	collection <- paste(product, version, resolution, sep="_")
	dir.create(file.path(path, collection), FALSE)

# data provided in the data manifest of the Copernicus service.

	plink <- paste0("@land.copernicus.vgt.vito.be/manifest/", collection, "/manifest_cgls_", collection, "_latest.txt" )

	qurl <- paste0("https://", paste(username, password, sep=":"), plink)
	qurl <- paste0("https://user:pwd", plink)

	urls <- RCurl::getURL(qurl, ftp.use.epsv = FALSE, dirlistonly = TRUE, crlf = TRUE)

	urls <- unlist(strsplit(urls, "\n"))
	urls <- paste0("https://", paste(username, password, sep=":"), "@", sub(".*//", "", urls))
	if (any(grepl("does not exist", urls))) {
		stop("This product is not available or misspecified")
	}
	
	d <- tolower(basename(urls))
	d <- sapply(strsplit(d, paste0(tolower(product), "_")), function(i)i[2])
	d <- sapply(strsplit(d, "_"), function(i) substr(i[1], 1, 8))
	d <- as.Date(bn, "%Y%m%d")

	start_date <- as.Date(start_date)
	end_date <- as.Date(end_date)
	i <- d >= start_date & d <= end_date
	urls <- urls[i]
	if (length(urls) == 0) {
		stop("no data found for this time period")
	}

	outf <- file.path(path, basename(urls))

	for (i in 1:length(urls)) {
		utils::download.file(urls[i], outf[i], mode = "wb", ...)	 
	}
	outf
}
