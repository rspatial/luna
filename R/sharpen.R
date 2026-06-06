# Pan-sharpening
# Author: Robert J. Hijmans
# November 2021; HSI/IHS rewrite June 2026
# Licence GPL v3

if (!isGeneric("panSharpen")) {setGeneric("panSharpen", function(x, p, ...) standardGeneric("panSharpen"))}

setMethod("panSharpen", signature(x="SpatRaster", p="SpatRaster"),
function(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...) {

		method <- match.arg(tolower(method), c("brovey", "hsi", "ihs", "pca"))
		if (nlyr(p) != 1) {
			stop("`p` must be a single-layer panchromatic SpatRaster")
		}
		if (!is.null(weights)) {
			if (length(weights) != nlyr(x)) {
				stop("`weights` must have length equal to nlyr(x) (", nlyr(x), ")")
			}
			if (any(weights < 0)) {
				stop("`weights` must be non-negative")
			}
		}
		if (method == "pca" && !is.null(weights)) {
			warning("`weights` are ignored when method = 'pca'", call. = FALSE)
		}

		x <- resample(x, p)
		Pstats <- as.numeric(global(p, c("mean", "sd"), na.rm = TRUE))
		if (is.na(Pstats[2]) || Pstats[2] <= 0) {
			stop("`p` has zero variance; cannot histogram-match to intensity")
		}

		if (method == "brovey") {
			# Brovey transform:  B_i' = B_i * P / I, where I is per-pixel
			# intensity. With weights, I = sum(w_i * B_i) (the GDAL
			# `gdal_pansharpen.py` default behaviour).
			if (is.null(weights)) {
				out <- (x * p) / mean(x)
			} else {
				w <- weights / sum(weights)
				I <- sum(x * w)
				out <- (x * p) / I
			}
		} else if (method == "pca") {
			# Principal Component Analysis pan-sharpening.
			# 1) Forward-project x onto principal components.
			# 2) Replace PC1 with the pan band, linearly rescaled to match PC1's mean and sd.
			# 3) Inverse-project. sign-flipped if needed
			if (nlyr(x) < 2) {
				stop("PCA pan-sharpening requires at least two multispectral layers")
			}
			pca <- terra::prcomp(x, scale. = FALSE)
			V   <- pca$rotation
			mu  <- pca$center
			flips <- ifelse(colSums(V) < 0, -1, 1)
			V <- sweep(V, 2, flips, "*")
			y <- terra::predict(x, pca)
			for (k in seq_len(nlyr(y))) {
				if (flips[k] == -1) y[[k]] <- -y[[k]]
			}
			s_pc <- as.numeric(global(y[[1]], c("mean", "sd"), na.rm = TRUE))
			y[[1]] <- (p - Pstats[1]) * (s_pc[2] / Pstats[2]) + s_pc[1]
			k <- nlyr(x)
			out <- terra::rast(lapply(seq_len(k), function(j) {
				coefs <- V[j, ]
				e <- coefs[1] * y[[1]]
				for (kk in seq_len(k)[-1]) e <- e + coefs[kk] * y[[kk]]
				e + mu[j]
			}))
			names(out) <- names(x)
		} else {
			# Additive IHS ("fast IHS") pan-sharpening.
			# Substituting the intensity component with a histogram-matched
			# pan band in cylindrical IHS space is algebraically equivalent
			# to adding (pan' - I) to every multispectral band, where pan'
			# is rescaled so its mean and sd match those of I. 
			if (is.null(weights)) {
				I <- mean(x)
			} else {
				w <- weights / sum(weights)
				I <- sum(x * w)
			}
			Istats <- as.numeric(global(I, c("mean", "sd"), na.rm = TRUE))
			pmatch <- (p - Pstats[1]) * (Istats[2] / Pstats[2]) + Istats[1]
			out <- x + (pmatch - I)
			names(out) <- names(x)
		}

		if (nzchar(filename)) {
			out <- terra::writeRaster(out, filename = filename,
			                          overwrite = overwrite, ...)
		}
		out
	}
)
