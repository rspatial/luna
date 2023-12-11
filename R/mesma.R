
if (!isGeneric("mesma")) {setGeneric("mesma", function(x, ...) standardGeneric("mesma"))}

setMethod("mesma", signature(x="SpatRaster"),
	function(x, em, iterate=400, tolerance=0.00000001, ...){
		#method = "NNLS", 
		#method <- match.arg(toupper(method), "NNLS")

		if (!inherits(em, c("matrix", "data.frame"))) {
			stop("'em' needs to be a 'matrix' or 'data.frame'")
		}
		if (inherits(em, "data.frame")) {em <- as.matrix(em)}
		if (anyNA(em)) {
			stop("'em' is not allowed to contain NA values. Spectra must be consistent.")
		}
		
		if(nrow(em) < 2) { 
			stop("'em' must contain at least two endmembers (rows)")
		}
		if(ncol(em) != nlyr(x)) {
			stop("'em' and 'img' have different numbers of spectral features (number of columns in 'em'). Both need to represent the same number of spectral bands for equal spectral resolutions/ranges.")
		}
	  
	 ## hand over to C++ nnls_solver
		sfun <- function(xi, na.rm = FALSE) {
			.nnls_solver(x = xi, A = em, iterate = iterate, tolerance = tolerance)
		}
		probs <- app(x, fun=sfun, wopt=list(...))
	  
		## assign band names
		if(length(rownames(em)) != 0){
			names(probs)[1:(nlyr(probs)-1)] <- rownames(em)
		}
		names(probs)[nlyr(probs)] <- "RMSE"
		probs
	}
)