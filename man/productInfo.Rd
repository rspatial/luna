\name{productInfo}

\alias{productInfo}

\title{NASA product information}

\description{
Get complete product information in the corresponding product website. The information is displayed in the default browser. See \code{\link{getProducts}} for available options of products.
}

\usage{
productInfo(product, ...)
}

\arguments{
  \item{product}{character}
  \item{...}{additional arguments (non implemented}
}

\seealso{ \code{\link[luna]{getProducts}}}

\examples{
\dontrun{
# MODIS in LP DAAC with version information
productInfo(product = "MCD18A1")
# Without any version or server information
productInfo(product = "LANDSAT_SURFACE_REFLECTANCE_L8_OLI_TIRS")
}
}

