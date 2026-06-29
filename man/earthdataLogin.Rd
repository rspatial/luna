\name{earthdataLogin}

\alias{earthdataLogin}
\alias{print.earthdata_session}

\title{Authenticate against NASA Earthdata Login (URS)}

\description{
Validate a NASA Earthdata Login (URS) username/password against \url{https://urs.earthdata.nasa.gov/} and return a session object that can be passed to \code{\link{getNASA}} via its \code{auth} argument.

A successful call confirms up front that the credentials are correct, so subsequent \code{getNASA(download = TRUE, auth = ...)} calls will fail fast on auth errors rather than appearing to succeed when files happen to be cached on disk (issue #40).
}

\usage{
earthdataLogin(username, password, verbose = TRUE)
}

\arguments{
  \item{username}{character. Earthdata Login user name. Sign up at \url{https://urs.earthdata.nasa.gov/users/new}.}
  \item{password}{character. Earthdata Login password.}
  \item{verbose}{logical. If \code{TRUE} (the default), print a status message before and after the credential probe.}
}

\details{
Validation is performed by issuing a HTTP GET request to \code{https://urs.earthdata.nasa.gov/api/users/tokens} with HTTP Basic authentication. The endpoint is read-only (it returns a list of the user's existing API tokens, if any) and reliably returns HTTP 200 for valid credentials and HTTP 401 for invalid ones.

The returned session object holds the paths to a temporary netrc file and cookie jar that the rest of luna uses to authenticate downloads. Both files are removed when the session is garbage-collected or when R exits.

In addition to a working Earthdata Login account, downloading a particular product requires that you have accepted that product's End User License Agreement (EULA) at \url{https://urs.earthdata.nasa.gov/}. Authentication can succeed (so \code{earthdataLogin()} returns a session) while subsequent downloads of an EULA-restricted product still fail with HTTP 401/403; in that case the failure is reported by \code{getNASA()}.
}

\value{
An \code{earthdata_session} object (an environment) with elements \code{username}, \code{netrc_file}, \code{cookie_file}, \code{validated}, and \code{created}.
}

\seealso{
\code{\link{getNASA}}, \code{\link{erosLogin}}
}

\examples{\dontrun{
auth <- earthdataLogin("my_user", "my_password")
auth   # prints a one-line summary

# reuse the same session for many downloads, without re-authenticating
f <- getNASA(product = "MOD09A1",
             start_date = "2019-05-17", end_date = "2019-05-23",
             aoi = c(33, 34, -5, -4),
             download = TRUE, path = tempdir(),
             auth = auth)
}}
