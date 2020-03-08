\name{getProducts}

\alias{getProducts}

\title{Find product names}

\description{
Find available product names for download. See \code{\link{getModis}} for further use.
}

\usage{
getProducts(product, ...)
}

\arguments{
  \item{product}{character}
  \item{...}{additional arguments. None implemented}
}


\value{
character
}

\seealso{ \code{\link[luna]{getModis}} }


\examples{
# list of all available MODIS products
getProducts("^MOD|^MYD|^MCD")
}

