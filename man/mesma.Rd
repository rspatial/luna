\name{mesma}

\alias{mesma}
\alias{mesma,SpatRaster-method}

\title{Multiple Endmember Spectral Mixture Analysis (Spectral Unmixing)}

\description{
\code{mesma} performs a multiple endmember spectral mixture analysis on a multiband raster image.

For unmixing, a non-negative least squares (NNLS) regression which is using a sequential coordinate-wise algorithm (SCA) based on Franc et al. (2005).
}

\usage{
\S4method{mesma}{SpatRaster}(x, em, iterate=400, tolerance=0.00000001, ...)
}

\arguments{
\item{x}{SpatRaster. Usually representing a hyperspectral remotely sensed image}
\item{em}{matrix or data.frame with spectral endmembers. Rows represent a single endmember of a class, columns represent the spectral bands (i.e. columns correspond to number of bands in \code{img}). The number of rows needs to be > 1}
\item{iterate}{integer. Set maximum iteration per pixel. Processing time could increase the more iterations are made possible}
\item{tolerance}{numeric. Tolerance limit representing a nearly zero minimal number}
\item{...}{further arguments passed to \link[raster]{writeRaster}}
}

\value{
SpatRaster with one layer per end-member, with each value representing the estimated presence probability of the end-member per pixel (0 to 1), and an RMSE layer.
}

\note{Depending on \code{iterate} and \code{tolerance} settings, the sum of estimated presence probabilities per pixel varies around 1.
}

\references{
Franc, V., Hlaváč, V., & Navara, M. (2005). Sequential coordinate-wise algorithm for the non-negative least squares problem. In: International Conference on Computer Analysis of Images and Patterns (pp. 407-414). Berlin, Heidelberg.
}

\author{
Jakob Schwalb-Willmann
}

\examples{
lsat <- rast(system.file("ex/lsat.tif", package="luna"))

# endmember spectra: water and land
em_names <- c("water", "land")
pts <- data.frame(class=em_names, cell = c(47916,5294))
em <- lsat[pts$cell]
rownames(em) <- em_names

# unmix the image for water and land
probs <- mesma(lsat, em)

# take a look
hist(probs$water)
plot(probs$water, col = c("white","blue"))
hist(probs$land)
plot(probs$land, col = c("white","brown"))
}

