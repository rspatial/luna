\name{getProducts}

\alias{getProducts}

\title{Find product names}

\description{
Find available product names for download. See \code{\link{getModis}} for further use.
}

\usage{
getProducts(product)
}

\arguments{
  \item{product}{character}
}


\value{
character
}

\seealso{ \code{\link[luna]{getProducts}}}

\examples{
  # available MODIS products
  getProducts(c("^MOD|^MYD|^MCD"))
}

