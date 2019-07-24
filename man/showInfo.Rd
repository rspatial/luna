\name{showInfo}

\alias{showInfo}

\title{Load complete product information}

\description{
Load complete product information in the corresponding product website. Opens the default borwser. See \code{\link{getProducts}} for available options of products.
}

\usage{
showInfo(product)
}

\arguments{
  \item{product}{character}
}


\seealso{ \code{\link[luna]{getProducts}}}

\examples{
  # MODIS in LP DAAC
  showInfo(product = "MCD18A1", version = "006", server = "LPDAAC_ECS")
  # Without any version or server information
  showInfo(product = "LANDSAT_SURFACE_REFLECTANCE_L8_OLI_TIRS")
}

