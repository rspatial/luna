# Authors: Robert J. Hijmans, Alex Mandel, Aniruddha Ghosh
# July 2019
# Version 0.1
# Licence GPL v3

..unpack_reject <- function(val, bits=32, se, reject) {
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


.unpack_reject <- function(val, bits=32, qmat) {
	from <- as.integer(as.character(qmat[,1]))
	to <- as.integer(as.character(qmat[,2]))
	stopifnot((min(from) > 0) & (max(from) <= bits))
	stopifnot((min(to) > 0) & (max(to) <= bits))
	stopifnot(all(to >= from))

	rej <- as.character(qmat[,3])
	reject <- lapply(rej, function(i) unlist(strsplit(i, ",")))

	n <- 1+to-from
	test <- sapply(1:length(reject), function(i) any(nchar(unlist(reject[i])) != n[i]))

	if (any(test)) {
		stop(paste("length of 'reject' does not match number of bytes in row(s):", paste(which(test), collapse=",")))
	}
	

	v <- rep(1, length(val))
	for (i in 1:length(val)) {
		bin_value <- rev(as.integer(intToBits(val[i]))[1:bits])
		for (j in 1:nrow(qmat)) {
			x <- paste(bin_value[from[j]:to[j]], collapse="")
			if (x %in% reject[[j]]) {
				v[i] <- NA
				break
			}
		}
	}
	return (v)
}

modis_mask <- function(x, bits, qmat, ...) {
# create a quality mask
# x     : SpatRaster quality band
# bits  : number of bits (16 or 32?)
# qmat   : matrix with start and end of the bits considered and the values to be rejected

	stopifnot(terra::nlyr(x) == 1)
	u <- terra::unique(x)[,1]
	v <- .unpack_reject(u, bits, qmat)
	uv <- cbind(u,v)
	terra::classify(x, uv, ...)
}

