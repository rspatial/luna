
evaluate <- function(cmat, stat="overall") {

	stopifnot(stat %in% c("kappa", "overall", "class"))
	n <- sum(cmat)
	d <- diag(cmat)
	if (stat == "kappa") {
		# observed (true) cases per class
		p <- rowSums(cmat) / n 
		# predicted cases per class
		q <- colSums(cmat) / n 
		expAccuracy <- sum(p*q)
		OA <- sum(d) / n
		kappa <- (OA - expAccuracy) / (1 - expAccuracy)
		return(kappa)
	} else if (stat == "overall") {
		OA <- sum(d) / n
		return(OA)
	} else if (stat == "class") {
		rw <- d / rowSums(cmat) 
		cw <- d / colSums(cmat) 
		x <- rbind(rw, cw)
		rownames(x) <- c("row-wise", "col-wise")
		return(x)
	}
}

