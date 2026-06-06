\name{panSharpen}

\alias{panSharpen}
\alias{panSharpen,SpatRaster,SpatRaster-method}

\title{
Panchromatic sharpening
}

\description{
Sharpen multispectral imagery using a higher-resolution panchromatic band. Three transforms are supported: \code{"Brovey"}, \code{"HSI"}, and \code{"PCA"}.
}


\usage{
\S4method{panSharpen}{SpatRaster,SpatRaster}(x, p, method="Brovey", weights=NULL, filename="", overwrite=FALSE, ...)
}
 
\arguments{
  \item{x}{SpatRaster with the multispectral layers (bands) to be sharpened.}
  \item{p}{Single-layer SpatRaster with the panchromatic band, at higher spatial resolution than \code{x}.}
  \item{method}{character. One of \code{"Brovey"}, \code{"HSI"}, or \code{"PCA"}.}
  \item{weights}{optional numeric vector of weights, one per layer of \code{x}. Used to compute the intensity component as \eqn{I = \sum_i w_i B_i} (after normalising the weights to sum to 1). When \code{NULL} (the default), \eqn{I} is the unweighted mean of the multispectral bands. Ignored (with a warning) when \code{method = "PCA"}, which derives its own component weighting from the data.}
  \item{filename}{character. Optional output file. When supplied, the result is written via \code{\link[terra]{writeRaster}}.}
  \item{overwrite}{logical. If \code{TRUE}, an existing \code{filename} is overwritten.}
  \item{...}{additional arguments for \code{\link[terra]{writeRaster}} (only used when \code{filename} is supplied).}
}

\details{
\code{x} is first resampled to the geometry of \code{p}.

\strong{Brovey.} For each multispectral band \eqn{B_i}, the Brovey transform returns \eqn{B_i' = B_i \cdot P / I}. Without weights, \eqn{I} is the per-pixel mean of the multispectral bands. With weights, \eqn{I} is the weighted sum, equivalent to the algorithm used by GDAL's \code{gdal_pansharpen.py} (the default GDAL pansharpening method).

\strong{HSI} The implementation follows the additive (also called \emph{fast}) IHS formulation: substituting the intensity component with a pan band that has been histogram-matched to \eqn{I} is algebraically equivalent to \eqn{B_i' = B_i + (P' - I)}, where \eqn{P'} is the pan band linearly rescaled so its mean and standard deviation match those of \eqn{I}. 

\strong{PCA.} The multispectral stack is forward-projected onto its principal components using \code{\link[terra]{prcomp}}; the first principal component (the dominant variance direction, analogous to a data-driven intensity) is then replaced with the pan band rescaled to its mean and sd, and the inverse rotation is applied to recover sharpened bands. Columns of the rotation matrix are flipped so that each component's loadings sum positively. PCA pan-sharpening is data-driven and well-suited to multispectral inputs with more than 3 bands, where Brovey and IHS rely on heuristic intensity definitions.

If you want behaviour equivalent to \code{gdal_pansharpen.py}, use \code{method = "Brovey"} with \code{weights} reflecting the spectral overlap of each multispectral band with the panchromatic band; for sensors where the pan band covers the visible plus NIR (e.g. Landsat 8 OLI band 8) typical weights are roughly \code{c(R=0.21, G=0.27, B=0.42, NIR=0.10)}.
}

\value{
SpatRaster with the same number of layers as \code{x}, at the resolution of \code{p}.
}

\seealso{\code{\link[terra]{colorize}}}

\examples{
library(terra)

f  <- system.file("ex/lsat.tif", package = "luna")
ms <- rast(f)[[1:3]]                       # B (B1), G (B2), R (B3)

# Synthesise a higher-resolution "panchromatic" band for the example.
# In practice, `p` would be the actual sensor pan band.
pan <- disagg(mean(rast(f)[[1:4]]), 2)

# Brovey (GDAL default)
b1 <- panSharpen(ms, pan, method = "Brovey")

# Weighted Brovey, matching gdal_pansharpen.py semantics
b2 <- panSharpen(ms, pan, method = "Brovey",
                 weights = c(0.42, 0.27, 0.21))

# Additive IHS (works for any number of bands)
h  <- panSharpen(ms, pan, method = "HSI")

# PCA (data-driven; recommended for >3 bands)
pc <- panSharpen(ms, pan, method = "PCA")

# Compare resolutions
res(ms)
res(pan)
res(b1)
}
