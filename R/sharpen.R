# Gap-fill and filter
# Author: Robert J. Hijmans
# November 2021
# Version 0.1
# Licence GPL v3


pansharpen <- function(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...) {
	if (!is.null(weights)) {
		stopifnot(length(weights) == nlyr(x))
	}
	if (method == "Brovey") {
		x <- resample(x, p)
		if (is.null(weights)) {
			(x * p) / mean(x)	
		} else {
			(x * p) / sum(x * weights)			
		}
	} else if (method == "HSI") {
		x <- resample(x, p)
		opt <- spatOptions()
		x <- colorize(x, "hsi")
		x[[3]] <- pan
		colorize(x, "rgb")
	} else {
		stop("unknown method")
	}
}




