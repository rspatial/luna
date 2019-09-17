\name{getLandsat}

\alias{getLandsat}

\title{Find and download Landsat products}

\description{
Find and download Landsat data for a specific product, area, and time period. 
The function returned is list of filenames that may have been downloaded.
}

\usage{
getModis(product, start_date, end_date, aoi, download = FALSE, path, version="006", limit=100000, server="LP DAAC", ...)
}

\arguments{
  \item{product}{character. Supported products can be found using \code{\link{getProducts}}, currently limited to Landsat_8_OLI_TIRS_C1}
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{character. end date for the data requested formatted yyyy-m-d}
  \item{aoi}{numeric vector of four elements (minimum longitude, maximum longitude, minimum latitude, maximum latitude) encompassing the area of interest. Or a SpatExtent or Extent object, or any object from which an Extent can be extracted (see examples)}
  \item{download}{logical. If \code{TRUE} data will be downloaded unless it is present in the download directory}
  \item{path}{character. Path name indicating where to store the data}
  \item{version}{character}
  \item{limit}{integer > 0}
  \item{server}{character. Defaults to AWS (Landsat 8 only). See Details}
  \item{...}{Additional arguments. These can be product specific. See Details)}
}


\value{
character vector of file names pointing to the downloaded files (if \code{download=TRUE})
}

\details{
If no data is available between \code{start_date} and \code{end_date}, files for the closest dates are returned. 

Server: AWS, ERS

}

\seealso{ \code{\link[luna]{getProducts}}}

\examples{  
product <- "Landsat_8_OLI_TIRS_C1"
sdate <- "2019-05-01" 
edate <- "2019-05-30"
area <- c(33, 34, -5, -4)
dir <- tempdir()
f <- getLandsat(product = product,
            start_date = sdate, end_date = edate,
            aoi = area, download = FALSE, path=dir)
            
# An example of ARD Tiles from ERS server
product <- "Landsat4-8_ARD_US_C1"
area <- c(-122.422996520996, -121.501968383789, 38.3139801025391, 38.9253616333008)
f1 <- getLandsat(product = product,
            start_date = sdate, end_date = edate,
            aoi = area, download = FALSE, path=dir, 
            server="ERS")
}

