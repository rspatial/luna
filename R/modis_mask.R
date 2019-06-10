
unpack_reject <- function(val, bits=32, se, reject) {
	v <- rep(1, length(val))
	for (i in 1:length(val)) {
		bin_value <- rev(as.integer(intToBits(val[i]))[1:bits])
		for (j in 1:nrow(se)) {
			x <- paste(bin_value[se[j,1]:se[j,2]], collapse="")
			if (x %in% reject[[j]]) {
				v[i] <- NA
				break
			}
		}
	}
	return (v)
}


modis_mask <- function(x, bits, se, reject, ...) {
# create a quality mask
# x     : SpatRaster quality band
# bits  : number of bits (16 or 32?)
# se    : matrix with start and end of the bits considered
# reject: list with the values to be rejected; matching the rows in se

	u <- terra::unique(x)
	v <- unpack_reject(u, bits, se, reject)
	uv <- cbind(u,v)
	terra::reclassify(x, uv, ...)
}

