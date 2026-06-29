\name{erosLogin}

\alias{erosLogin}
\alias{print.eros_session}

\title{Authenticate against the USGS EROS login system}

\description{
Validate a USGS Earthdata Login username/password against \url{https://ers.cr.usgs.gov/} and return a session object that can be passed to \code{\link{getLandsat}} via its \code{auth} argument.

Although USGS uses the same "Earthdata Login" credentials as NASA, the actual authentication flow for Landsat downloads is different: USGS requires an interactive form-login on \code{ers.cr.usgs.gov} that establishes a session cookie. \code{erosLogin()} performs that flow once and stores the resulting curl handle in the session object, so subsequent \code{getLandsat(download = TRUE, auth = ...)} calls reuse the same authenticated session.
}

\usage{
erosLogin(username, password, verbose = TRUE)
}

\arguments{
  \item{username}{character. USGS Earthdata Login user name. Sign up at \url{https://ers.cr.usgs.gov/register}.}
  \item{password}{character. USGS Earthdata Login password.}
  \item{verbose}{logical. If \code{TRUE} (the default), print a status message before and after the form-login probe.}
}

\details{
Authentication is performed by GETting the EROS login page, scraping the CSRF token from its HTML form, and POSTing a form-encoded request with the supplied credentials. Success is confirmed by checking that the resulting HTML contains a "Sign Out" link.

The session holds a libcurl handle whose cookies authenticate subsequent download requests against \code{landsatlook.usgs.gov}. The session attempts a logout when the object is garbage-collected.

If the EROS UI changes such that the login flow stops working, \code{erosLogin()} fails with an informative error. In that case consider using the \pkg{rstac} package or downloading manually from \url{https://earthexplorer.usgs.gov/}.
}

\value{
An \code{eros_session} object (an environment) with elements \code{username}, \code{handle}, \code{validated}, and \code{created}.
}

\seealso{
\code{\link{getLandsat}}, \code{\link{earthdataLogin}}
}

\examples{\dontrun{
auth <- erosLogin("my_user", "my_password")
auth   # prints a one-line summary

# reuse the session across multiple Landsat scenes
f <- getLandsat(product = "landsat-c2l1",
                start_date = "2019-05-01", end_date = "2019-05-31",
                aoi = c(33, 34, -5, -4),
                bands = c("blue", "green", "red", "nir08"),
                download = TRUE, path = tempdir(),
                auth = auth)
}}
