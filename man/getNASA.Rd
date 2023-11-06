\name{getNASA}

\alias{getNASA}
\alias{getModis}

\title{Find and download MODIS products}

\description{
Find and download MODIS data for a specific product, area, and time period. The function returned is list of filenames that may have been downloaded.

It is important to fist consult \code{\link{getProducts}} to discover what products and version numbers are available, and which server to use.
}

\usage{
getNASA(product, start_date, end_date, aoi, version="006", download=FALSE, path, 
		username, password, server="LPDAAC_ECS", limit=100000, overwrite=FALSE, ...)

getModis(product, start_date, end_date, aoi, version="006", download=FALSE, path, 
		username, password, server="LPDAAC_ECS", limit=100000, overwrite=FALSE, ...)
}

\arguments{
  \item{product}{character. Supported products can be found using \code{\link{getProducts}}}
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{aoi}{numeric vector of four elements (minimum longitude, maximum longitude, minimum latitude, maximum latitude) encompassing the area of interest. It can also be a SpatExtent, or any object from which an extent can be extracted}
  \item{version}{character. product version}
  \item{download}{logical. If \code{TRUE} data will be downloaded unless it is present in the download directory}
  \item{path}{character. Path name indicating where to store the data}
  \item{username}{character. EOSDIS user name (see Details)}
  \item{password}{character. EOSDIS password (see Details)}
  \item{server}{character. The server to download the data from}
  \item{limit}{positive integer}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{...}{Additional arguments. These can be product specific. See Details)}
}


\value{
Character vector of file names pointing to the downloaded files (if \code{download=TRUE})
}

\details{
This function is an R interface to the EOSDIS system. Access is free, but it requires a "Earthdata" account. If you do not have an account, you can sign up here: \url{https://urs.earthdata.nasa.gov/users/new}

If no data is available between \code{start_date} and \code{end_date}, files for the closest dates are returned. 
}

\seealso{ \code{\link{getProducts}}, \code{\link{getLandsat}}} 

\examples{  
product <- "MOD09A1"
# information about the product
getProducts(product)
sdate <- "2019-05-17" 
edate <- "2019-05-23"
area <- c(33, 34, -5, -4)
path <- tempdir()
# list available tiles
f <- getNASA(product = product,
            start_date = sdate, end_date = edate,
            aoi = area, download = FALSE, path=path)
f

# get the date from the filenames
modisDate(f)

# to download the tiles, set download = TRUE
# and provide your credentials 

usr <- "your user name"
pwd <- "your password"
#f <- getNASA(product = product, username=urs, password=pwd,
#            start_date = sdate, end_date = edate,
#            aoi = area, download = TRUE, path=path)

}

