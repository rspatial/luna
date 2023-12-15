# Gap-fill and filter
# Author: Robert J. Hijmans
# November 2021
# Version 0.1
# Licence GPL v3

if (!isGeneric("panSharpen")) {setGeneric("panSharpen", function(x, p, ...) standardGeneric("panSharpen"))}

setMethod("panSharpen", signature(x="SpatRaster", p="SpatRaster"),
function(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...) {

		method <- match.arg(tolower(method), c("brovey", "hsi"))
		if (!is.null(weights)) {
			stopifnot(length(weights) == nlyr(x))
		}
		if (method == "brovey") {
			x <- resample(x, p)
			if (is.null(weights)) {
				(x * p) / mean(x)	
			} else {
				(x * p) / sum(x * weights)			
			}
		} else { #if (method == "hsi") {
			x <- resample(x, p)
			x <- colorize(x, "hsi")
			x[[3]] <- p
			colorize(x, "rgb")
		}
	}
)



