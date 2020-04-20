\name{download_ee}

\alias{download_ee}

\title{download from Earth Explorer}

\description{
Find and download ee data 
}

\usage{
download_ee(product, ids, path, username, password, ...)
}

\arguments{
  \item{product}{character. Supported products can be found using \code{\link{getProducts}}}
  \item{ids}{entity IDs}
  \item{path}{character. Path name indicating where to store the data}
  \item{username}{character. EOSDIS user name (see Details)}
  \item{password}{character. EOSDIS password (see Details)}
  \item{...}{Additional arguments. None implemented}
}


\value{
Character vector of file names pointing to the downloaded files
}

\details{
This function is an R interface to the Earth Explores system. Access is free, but it requires a "Earthdata" account. If you do not have an account, you can sign up here: \url{https://urs.earthdata.nasa.gov/users/new}

}

\seealso{\code{\link{getLandsat}}} 

\examples{  
# to do
}

