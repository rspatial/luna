# Gap-fill and filter
# Authors: Aniruddha Ghosh, Robert J. Hijmans
# November 2019
# Version 0.1
# Licence GPL v3

### Fill NA and fit index values



# Fill NAs by fiting with a smoothing spline
fillVI <- function(x) {
  # To omit pixel if it has a lot of NA values
  if (length(which(is.na(x))) > length(x)*0.75) return(x)
  
  z <- which(is.na(x))
  nz <- length(z)
  nx <- length(x)
  if (nz == 0 | nz == nx) {
    return(x)
  }
  x[z] <- stats::spline(x = 1:nx, y = x, xout = z, method = "natural")$y
  return(x)
}


# Smooth of time-series data using Savitzky-Golay filter
filterVI <- function(x){
  stopifnot(require(signal))
  x <- fillVI(x)
  if (any(is.na(x))) return(x)
  # Smooth x
  evisgfit <- signal::sgolayfilt(x, p = 4, n = 13, m = 0) 
  resu <- x - evisgfit
  dif <- abs(resu)
  mm <- max(dif)
  weights <- c(1:(length(x)))
  index <- which(resu <= 0)
  if (length(index) > 0){
    weights[index] <- 1 - (dif[index]/mm)
  } 
  gdis <- sum(abs(dif * weights))
  ormax <- gdis
  it <- 1
  imax <- 10
  while ((gdis <= ormax) && (it < imax)){
    ra4 <- x
    index <- which(x < evisgfit)
    if (length(index) > 0){
      ra4[index] <- evisgfit[index]
    } 
    evisgfit <- signal::sgolayfilt(ra4, p = 4, n = 7, m = 0) 
    resu <- x - evisgfit
    ormax <- gdis
    gdis <- sum(abs(resu*weights))
  }
  return(evisgfit)
}

