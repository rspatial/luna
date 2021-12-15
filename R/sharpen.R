# Gap-fill and filter
# Author: Robert J. Hijmans
# November 2021
# Version 0.1
# Licence GPL v3


sharpen <- function(x, p, method, weights=NULL, filename="", overwrite=FALSE, ...) {
	if (is.null(weights)) {
		stopifnot(length(weights) == nlyr(x))
	}
	if (method == "Brovey") {
		x <- resample(x, p)
		if (is.null(weights)) {
			(x * p) / mean(x)	
		} else {
			(x * p) / sum(x * weights)			
		}
	} else if (method == "IHS") {
		x <- resample(x, p)
		opt = SpatOptions()
		# x must be RGB
		x@ptr = x@ptr$rgb2hsx("hsi", opt)
		x[[3]] = pan
		x@ptr <- x@ptr$hsx2rgb("hsi", opt)
		messages(x)	
	} 
}




