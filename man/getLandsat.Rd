\name{getLandsat}

\alias{getLandsat}

\title{Find and download Landsat products}

\description{
Find and (optionally) download Landsat scenes for a specific product, area, and time period.

The function searches the USGS Landsat STAC catalog at \url{https://landsatlook.usgs.gov/stac-server/}. It is the successor to the old, NASA CMR-based implementation of \code{getLandsat()}: NASA CMR no longer indexes USGS Landsat granules, so the previous default product \code{"Landsat_8_OLI_TIRS_C1"} (Landsat 8 Collection 1) and the AWS public S3 bucket \code{landsat-pds} are both retired.

When \code{download = FALSE} (the default), \code{getLandsat()} returns a character vector of asset URLs that match the search and the optional \code{bands} filter. These can be inspected, filtered, or downloaded with any tool of your choice.

When \code{download = TRUE}, the function logs in to EROS using the supplied authentication session (or, deprecated, raw \code{username}/\code{password}), downloads each asset, and returns a vector of file paths. Downloads are validated against authentication-failure HTML pages so that no broken file is ever saved silently.
}

\usage{
getLandsat(product = "landsat-c2l1", start_date, end_date, aoi,
	download = FALSE, path,
	username, password, auth = NULL,
	bands = NULL,
	limit = 1000, overwrite = FALSE,
	verbose = TRUE, ...)
}

\arguments{
  \item{product}{character. STAC collection id. One of \code{"landsat-c2l1"} (Level-1, all sensors), \code{"landsat-c2l2-sr"} (Level-2 Surface Reflectance), or \code{"landsat-c2l2-st"} (Level-2 Surface Temperature). Legacy CMR short names such as \code{"Landsat_8_OLI_TIRS_C1"} are translated to \code{"landsat-c2l1"} with a message.}
  \item{start_date}{character or Date. Start date for the search, formatted yyyy-m-d.}
  \item{end_date}{character or Date. End date for the search, formatted yyyy-m-d.}
  \item{aoi}{numeric vector of four elements (\code{xmin, xmax, ymin, ymax}) encompassing the area of interest. Or a SpatExtent or any object from which an extent can be extracted.}
  \item{download}{logical. If \code{TRUE} the matching assets are downloaded to \code{path}.}
  \item{path}{character. Directory in which to store downloaded files.}
  \item{username}{character. USGS Earthdata Login user name. \emph{Deprecated} - prefer creating an \code{auth} session up front via \code{\link{erosLogin}} (see Details).}
  \item{password}{character. USGS Earthdata Login password. \emph{Deprecated} - see \code{username}.}
  \item{auth}{an \code{eros_session} returned by \code{\link{erosLogin}}. Required for downloads unless the deprecated \code{username}/\code{password} arguments are supplied.}
  \item{bands}{optional character vector of asset keys (e.g. \code{c("blue","green","red","nir08")}) to restrict the result. The match is case-insensitive. If \code{NULL} (the default) all assets of every matching scene are returned, including band TIFs and metadata files such as \code{MTL.txt} and \code{ANG.txt}.}
  \item{limit}{positive integer. Maximum number of scenes to retrieve from the STAC search.}
  \item{overwrite}{logical. If \code{TRUE}, existing files are re-downloaded.}
  \item{verbose}{logical. If \code{FALSE}, progress bars and per-file cache/progress messages are suppressed.}
  \item{...}{Additional arguments. The legacy \code{server} and \code{version} arguments are accepted for backwards compatibility but ignored, with a warning.}
}

\value{
A character vector. When \code{download = FALSE}, asset URLs that match the search and any \code{bands} filter. When \code{download = TRUE}, the local file paths of successfully downloaded assets.
}

\details{
Landsat data are distributed by USGS, not NASA. To download data you need a USGS Earthdata Login account; you can sign up for free at \url{https://ers.cr.usgs.gov/register}. The same Earthdata Login can be used with NASA EOSDIS (see \code{\link{getNASA}}), but the actual authentication flow is different - USGS requires an interactive form-login on \code{ers.cr.usgs.gov}.

\strong{Authentication.} The recommended way to supply credentials is to call \code{\link{erosLogin}} once and pass the returned session via \code{auth}:
\preformatted{auth <- erosLogin("my_user", "my_password")
getLandsat(..., download = TRUE, auth = auth)}
\code{erosLogin()} validates the credentials up front, so authentication errors are caught immediately rather than only when a download is attempted, and the session can be reused across many \code{getLandsat()} calls without re-authenticating. The \code{username}/\code{password} arguments are still accepted for backwards compatibility but are deprecated and may emit a warning. They are ignored when \code{auth} is supplied.

\strong{Caching.} When \code{download = TRUE} and an asset already exists in \code{path} (and \code{overwrite = FALSE}), \code{getLandsat()} re-uses the local copy and does \emph{not} contact USGS. With \code{verbose = TRUE} this is announced as a \code{"reading from cache: ..."} message. Use \code{\link{erosLogin}} to validate credentials independently of the cache.

For programmatic access at scale, also consider the official USGS M2M API (\url{https://m2m.cr.usgs.gov/}) or the \pkg{rstac} package, which both target the same STAC catalog.
}

\seealso{ \code{\link{erosLogin}}, \code{\link{getProducts}}, \code{\link{getNASA}}}

\examples{
\dontrun{
sdate <- "2019-05-01"
edate <- "2019-05-30"
area  <- c(33, 34, -5, -4)
dir   <- tempdir()

# list all assets for matching scenes
urls <- getLandsat("landsat-c2l1", start_date = sdate, end_date = edate,
                   aoi = area, download = FALSE)
head(urls)

# only the four optical bands of every matching scene
urls <- getLandsat("landsat-c2l1", start_date = sdate, end_date = edate,
                   aoi = area, download = FALSE,
                   bands = c("blue", "green", "red", "nir08"))

# download a Surface Reflectance scene (requires Earthdata Login credentials)
# auth  <- erosLogin("your_user", "your_pwd")
# files <- getLandsat("landsat-c2l2-sr",
#                     start_date = sdate, end_date = edate,
#                     aoi = area, download = TRUE, auth = auth,
#                     bands = c("blue", "green", "red", "nir08"),
#                     path = dir)
}
}
