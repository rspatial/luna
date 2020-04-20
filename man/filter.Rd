\name{filterVI}

\alias{filterVI}
\alias{fillVI}

\title{Gapfill and filter time series satellite data}

\description{
  Estimate missing values using smooth gap-filled time series data with a two-stage 
  Savitzky-Golay smoothing filter.
}

\usage{
  filterVI(x, ...)

  fillVI(x, method = "natural")

}

\arguments{
  \item{x}{numeric or SpatRaster representing time series signal to be filtered}
  \item{method}{type of spline to be used. Possible values are "fmm", "natural", "periodic", "monoH.FC" and "hyman"}
  \item{...}{additional arguments:
  \code{method}: as above
  \code{p1}: filter order for first pass
  \code{n1}: filter length for first pass (must be odd)
  \code{p2}: filter order for inner passes
  \code{n2}: filter length for inner passes (must be odd)
  }
}  

\value{
  Filtered signal of length(x)
}   

\details{
Performs smoothing of time series data using a two pass Savitzky-Golay smoothing filter. Second pass is an iterative method to minimize the difference between actual and fitted values that are significantly lower than the actual values. Missing values designated as \code{NA} are automatically interpolated with \code{spline}. For more details on the arguments see \code{\link[stats]{splinefun}} and \code{\link[signal]{sgolayfilt}}. 
}


\examples{  
# apply on single pixel
#vi <- ....
#s <- filterVI(vi)

# for time series raster 
#fevi <- app(evi, filterVI)
}
