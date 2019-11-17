\name{getModis}

\alias{getModis}

\title{Find and download MODIS products}

\description{
Find and download MODIS data for a specific product, area, and time period. 
The function returned is list of filenames that may have been downloaded.
}

\usage{
getModis(product, start_date, end_date, aoi, download = FALSE, path, version="006", limit=100000, server="LP DAAC", ...)
}

\arguments{
  \item{product}{character. Supported products can be found using \code{\link{getProducts}}}
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{aoi}{numeric vector of four elements (minimum longitude, maximum longitude, minimum latitude, maximum latitude) encompassing the area of interest. Or a SpatExtent or Extent object, or any object from which an Extent can be extracted (see examples)}
  \item{download}{logical. If \code{TRUE} data will be downloaded unless it is present in the download directory}
  \item{path}{character. Path name indicating where to store the data}
  \item{version}{character}
  \item{limit}{integer > 0}
  \item{server}{character}
  \item{...}{Additional arguments. These can be product specific. See Details)}
}


\value{
Character vector of file names pointing to the downloaded files (if \code{download=TRUE})
}

\details{
If no data is available between \code{start_date} and \code{end_date}, files for the closest dates are returned. 
}

\seealso{ \code{\link[luna]{getProducts}}, \code{\link[luna]{getModisYMD}}} 

\examples{  
product <- "MOD09A1"
# information about the product
getProducts(product)
sdate <- "2019-05-17" 
edate <- "2019-05-23"
area <- c(33, 34, -5, -4)
dir <- tempdir()
# list available tiles
f <- getModis(product = product,
            start_date = sdate, end_date = edate,
            aoi = area, download = FALSE, path=dir)
# to download the tiles, set download = TRUE
# parse date from downloaded MODIS tiles
getProducts(f)
}

