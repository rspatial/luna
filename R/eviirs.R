
.eviirs_ndvi <- function(region="africa/east", start_date, end_date, path) {
	url = paste0("https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/", region, 
			"pentadal/eviirs/ndvi/temporallysmoothedndvi/downloads/pentadal/")

	s <- as.Date(start_date)
	e <- as.Date(end_date)
	x <- seq(s, e, 1)
	xd <- pmin(pmax(1, round(.doyFromDate(x) / 10)), 72)
	xd <- sprintf("%02d", xd)
	xy <- as.character(.yearFromDate(x))
	xyd <- unique(paste0(xy, xd))
	d <- gsub("^0", "", substr(xyd,3, 6))
	uu <- paste0(url, "ea", d, ".zip")
	zf <- file.path(path, basename(uu))
	ff <- gsub(".zip", ".tif", zf)
	if (all(file.exists(ff))) {
		return(rast(ff))
	}
	for (i in 1:length(uu)) {
		.downloadDirect(uu[i], zf[i], unzip=TRUE, remove=TRUE)
		utils::flush.console()
	}
	x <- rast(ff)
	tm <- .dateFromDoy(as.integer(substr(xyd, 5, 6)) * 10 - 5, as.integer(substr(xyd, 1, 4))) 
	time(x) <- tm
	x
}

