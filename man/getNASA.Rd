\name{getNASA}

\alias{getNASA}

\title{Find and download NASA products}

\description{
Find and download NASA data for a specific product, area, and time period. The function returned is list of filenames that may have been downloaded.

It is important to fist consult \code{\link{getProducts}} to discover what products and version numbers are available, and which server to use.
}

\usage{
getNASA(product, start_date, end_date, aoi, version=NULL, download=FALSE, path,
	username, password, auth=NULL,
	server="LPDAAC_ECS", limit=100000, overwrite=FALSE,
	pattern=NULL, verbose=TRUE, ...)
}

\arguments{
  \item{product}{character. Supported products can be found using \code{\link{getProducts}}}
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{aoi}{numeric vector of four elements (minimum longitude, maximum longitude, minimum latitude, maximum latitude) encompassing the area of interest. It can also be a SpatExtent, or any object from which an extent can be extracted}
  \item{version}{character. product version number}
  \item{download}{logical. If \code{TRUE} data will be downloaded unless it is present in the download directory}
  \item{path}{character. Path name indicating where to store the data}
  \item{username}{character. NASA Earthdata Login user name. \emph{Deprecated} - prefer creating an \code{auth} session up front via \code{\link{earthdataLogin}} (see Details).}
  \item{password}{character. NASA Earthdata Login password. \emph{Deprecated} - see \code{username}.}
  \item{auth}{an \code{earthdata_session} returned by \code{\link{earthdataLogin}}. Required for downloads unless the deprecated \code{username}/\code{password} arguments are supplied.}
  \item{server}{character. The server to download the data from}
  \item{limit}{positive integer}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{pattern}{optional character vector of regular expressions. Granules whose filename does not match any of the patterns are dropped. The match is case-insensitive and uses \code{\link[base]{grepl}}. When \code{NULL} all granules are kept.}
  \item{verbose}{logical. If \code{FALSE}, progress bars and per-file cache/progress messages printed during downloading are suppressed}
  \item{...}{Additional arguments. These can be product specific. See Details)}
}


\value{
Character vector of file names pointing to the downloaded files (if \code{download=TRUE})
}

\details{
This function is an R interface to the EOSDIS system. Access is free, but it requires a "Earthdata" account. If you do not have an account, you can sign up here: \url{https://urs.earthdata.nasa.gov/users/new}

In addition to having an account, before you can download a particular product you must accept its End User License Agreement (EULA). The EULA is presented when you first try to download a product through the EOSDIS web interface, and can also be reviewed under your profile at \url{https://urs.earthdata.nasa.gov/}. If your credentials are wrong or you have not accepted the EULA, downloads will fail with an authentication error rather than silently saving an HTML error page as if it were data.

\strong{Authentication.} The recommended way to supply credentials is to call \code{\link{earthdataLogin}} once and pass the returned session object via \code{auth}:
\preformatted{auth <- earthdataLogin("my_user", "my_password")
getNASA(..., download = TRUE, auth = auth)}
\code{earthdataLogin()} validates the credentials up front against URS, so authentication errors are caught immediately rather than only when a download is attempted. The session can be reused across many \code{getNASA()} calls without re-authenticating.

The \code{username}/\code{password} arguments are still accepted for backwards compatibility but are deprecated and may emit a warning. They are ignored when \code{auth} is supplied.

\strong{Caching.} When \code{download = TRUE} and a granule already exists in \code{path} (and \code{overwrite = FALSE}), \code{getNASA()} re-uses the local copy and does \emph{not} contact Earthdata. With \code{verbose = TRUE} this is announced as a \code{"reading from cache: ..."} message, so it is clear that no download (and no credential check) was performed for that file. Use \code{\link{earthdataLogin}} to validate credentials independently of the cache.

If no data is available between \code{start_date} and \code{end_date}, files for the closest dates are returned.

NASA periodically retires older product collections in favor of new ones (for example, \code{MOD17A3H} was superseded by \code{MOD17A3HGF}). When the requested \code{product}/\code{version} cannot be found, \code{getNASA} reports the available alternatives in the cached collection list and suggests similar product short names; consult \code{\link{getProducts}} for the full inventory.
}

\seealso{ \code{\link{earthdataLogin}}, \code{\link{getProducts}}, \code{\link{getLandsat}}} 

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
# and provide your credentials via an explicit Earthdata Login session
#auth <- earthdataLogin("your_user", "your_password")
#f <- getNASA(product = product, auth = auth,
#            start_date = sdate, end_date = edate,
#            aoi = area, download = TRUE, path=path)

# Daymet bundles several variables (tmin, tmax, prcp, srad, vp, swe, dayl)
# under a single collection; use `pattern` to download only the ones you
# want instead of all of them:
#f <- getNASA(product = "Daymet_Daily_V4R1_2129", server = "ORNL_CLOUD",
#            start_date = "2020-06-29", end_date = "2020-07-01",
#            aoi = area, pattern = c("tmin", "tmax"),
#            download = TRUE, auth = auth, path = path)
}

