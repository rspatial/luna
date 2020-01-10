\name{filterVI}

\alias{filterVI}

\title{Gapfill and filter time series satellite data}

\description{
  Estimate missing values using Smooth gapfilled time series data with a two-stage 
  Savitzky-Golay smoothing filter.
  Find and download MODIS data for a specific product, area, and time period. 
  The function returned is list of filenames that may have been downloaded.
}

\usage{
  fillVI(x, method, p1, n1, p2, n2)
}

\arguments{
  \item{x}{numeric or SpatRaster representing time series signal to be filtered}
  \item{method}{type of spline to be used. Possible values are "fmm", "natural", "periodic", "monoH.FC" and "hyman".}
  \item{p1}{filter order for first pass}
  \item{n1}{filter length for first pass (must be odd)}
  \item{p2}{filter order for inner passes}
  \item{n2}{filter length for inner passes (must be odd)}
}  

\value{
  Filtered signal of length(x)
}   

\details{
Performs smoothing of time series data using a two pass Savitzky-Golay smoothing filter. Second pass is an iterative method to minimize the difference between actual and fitted values that are significantly lower than the actual values. Missing values designated as \code{NA} are automatically interpolated with \code{\link[signal]{spline}}. For more details on the arguments see \code{\link[signal]{spline}} and \code{\link[signal]{sgolayfilt}}. 
}

\seealso{\code{\link[luna]{fillVI}}} 

\examples{  
# apply on single pixel
s <- filterVI(vi)

# for time series raster 
fevi <- app(evi, filterVI)
# see \code{\link[terra]{app}} for more options to save the results to disk 
}

