

kfold <- function(x, k=5, by=NULL) {

	singlefold <- function(obs, k) {
		if (k==1) {
			return(rep(1, obs))
		} else {
			i <- obs / k
			if (i < 1) {
				stop('insufficient records:', obs, ', with k=', k)
			}
			i <- round(c(0, i * 1:(k-1), obs))
			times = i[-1] - i[-length(i)]

			group <- c()
			for (j in 1:(length(times))) {
				group <- c( group, rep(j, times=times[j]) )
			}
		
			r <- order(runif(obs))
			return(group[r]) 
		}
	}

	if (is.vector(x)) {
		if (length(x) == 1) {
			if (x > 1) {
				x <- 1:x
			}
		}
		obs <- length(x)
	} else if (inherits(x, 'Spatial')) {
		if (inherits(x, 'SpatialPoints')) {
			obs <- nrow(coordinates(x))
		} else {
			obs <- nrow(x@data)
		}
	} else {
		obs <- nrow(x)
	}
	if (is.null(by)) {
		return(singlefold(obs, k))
	}
	
	by = as.vector(as.matrix(by))
	if (length(by) != obs) {
		stop('by should be a vector with the same number of records as x')
	}
	un <- unique(by)
	group <- vector(length=obs)
	for ( u in un ) {
		i = which(by==u)
		kk = min(length(i), k)
		if (kk < k) warning('lowered k for by group: ', u  ,'  because the number of observations was  ',  length(i))
		group[i] <- singlefold(length(i), kk)
	} 
	return(group)
}

