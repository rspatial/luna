\name{getModisTiles}

\title{Find and download MODIS various products}

\description{
  Find and download list of MODIS files available for a specific product. The object returned is always list of HDF files that is found using the search criteria.
}

\usage{
  getModisTiles(product, start_date, end_date, aoi)
}

\arguments{
  \item{product}{Character, currently supported products can be found using \code{listProducts()}}
  \item{start_date}{Character. start date for the data requested. currently accepted format Y-m-d}
  \item{end_date}{Character. end date for the data requested. currently accepted format Y-m-d}
  \item{aoi}{Extent object, or any object from which an Extent object can be extracted (see examples)}
  \item{download}{Logical. If \code{TRUE} data will be downloaded if not locally available}
  \item{downdir}{Character. Path name indicating where to store the data. No default path available as the downloaded file could be large, users requested to supply a valid path)}
  \item{overwrite}{Logical. If \code{TRUE} overwrite any local files}
  \item{...}{Additional arguments. These can be product specific. See Details)}
}


\value{
  List of files
}

\details{
 If no MODIS tiles are available between \code{start_date} and \code{end_date}, HDF files for the closest dates are returned. 
}

\seealso{ \code{\link[luna]{listProducts}}}

\examples{
  
  # list of available MODIS products
  listProducts(c("^MOD|^MYD|^MCD"))
  product <- "MOD09A1"
  start_date <- "2019-05-17" 
  end_date <- "2019-05-23"
  aoi <- getData('GADM', country = 'KEN', level = 1)
  downdir <- tempdir()
  
  modfiles <- getModisTiles(product = product,
                        start_date = start_date,
                        end_date = end_date,
                        aoi = aoi,
                        download = FALSE)
}