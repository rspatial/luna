\name{pansharpen}

\alias{pansharpen}

\title{
pansharpen
}

\description{
Panchromatic band sharpening of lower resolution satellite images. 
}


\usage{
pansharpen(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...)
}
 
\arguments{
  \item{x}{SpatRaster with layers (bands) to be sharpened}
  \item{p}{SpatRaster with a single panchromatic layer at a higher spatial resolution than \code{x}}
  \item{method}{chracter. Either "Brovey" or "HSI"}
  \item{weights}{optional vector of weights for the "Brovey" method. The weights should have a length equal to \code{nlyrs(x)} and they should normally add up to 1}
  \item{filename}{character. Output filename}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}

\value{
SpatRaster
}

