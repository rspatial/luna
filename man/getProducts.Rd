\name{getProducts}

\alias{getProducts}

\title{Find NASA product names}

\description{
Find available products in NASA's Common Metadata Repository (CMR) that can be downloaded. See \code{\link{getNASA}} for further use.
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

\seealso{ \code{\link{getNASA}} }


\examples{
# list of all available MODIS products
getProducts("^MOD|^MYD|^MCD")
}

