\name{filterVI}

\alias{filterVI}

\title{Gap-fill and filter time series satellite data}

\description{
  Estimate missing values using Smooth gap-filled time series data with a two-stage 
  Savitzky-Golay smoothing filter.
  Find and download MODIS data for a specific product, area, and time period. 
  The function returned is list of filenames that may have been downloaded.
}

\usage{
  fillVI(x, method, ...)
}

\arguments{
  \item{x}{numeric or SpatRaster representing time series signal to be filtered}
  \item{method}{type of spline to be used. Possible values are "fmm", "natural", "periodic", "monoH.FC" and "hyman"}
  \item{...}{arguments passed on to signal::sgolayfilt}
}


\value{
vector of length(x)
}

\details{
Performs smoothing of time series data using a two pass Savitzky-Golay smoothing filter. The second pass is an iterative method to minimize the difference between actual and fitted values that are significantly lower than the actual values. Missing values designated as \code{NA} are automatically interpolated with \code{\link[signal]{spline}}. For more details on the arguments see \code{\link[signal]{spline}} and \code{\link[signal]{sgolayfilt}}.
} 

\seealso{\code{\link[luna]{fillVI}}} 

\examples{  
# apply on single pixel
s <- filterVI(vi)

# for time series raster 
fevi <- app(evi, filterVI)
# see \code{\link[terra]{app}} for more options to save the results to disk 
}

