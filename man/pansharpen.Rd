\name{panSharpen}

\alias{panSharpen}
\alias{panSharpen,SpatRaster,SpatRaster-method}

\title{
Panchromatic sharpening
}

\description{
Panchromatic band sharpening of lower resolution satellite images. 
}


\usage{
\S4method{panSharpen}{SpatRaster,SpatRaster}(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...)
}
 
\arguments{
  \item{x}{SpatRaster with layers (bands) to be sharpened}
  \item{p}{SpatRaster with a single panchromatic layer at a higher spatial resolution than \code{x}}
  \item{method}{character. Either "Brovey" or "HSI"}
  \item{weights}{optional vector of weights for the "Brovey" method. The weights should have a length equal to \code{nlyr(x)} and they should normally add up to 1}
  \item{filename}{character. Output filename}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{...}{additional arguments for writing files as in \code{\link[terra]{writeRaster}}}
}

\value{
SpatRaster
}

