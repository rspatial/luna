\name{modis_mask}

\alias{modis_mask}

\title{MODIS mask}

\description{Create a "mask" for a MODIS image based your selection of bad quality pixels, using the Quality Assessment (QA) band. }

\usage{
modis_mask(x, bits, qmat, ...)
}

\arguments{
  \item{x}{SpatRaster}
  \item{bits}{single integer value: 16 or 32}
  \item{qmat}{three-column matrix of data.frame. The first and second columns are positive integers (or character representation thereof) indicating the start and end bits (starting counting at one, not at zero!). The third column has the patterns to be rejected, stored as comma-separated characters. See example}
  \item{\dots}{additional arguments, none implemented}\
  \item{val}{integer}  
}

\value{
SpatRaster
}


\examples{
\dontrun{
f <- "MOD09A1.A2011009.h21v08.006.2015216112834.hdf"
mod <- rast(f)
shortnames <- substr(names(mod), 86, 120)
names(mod) <- shortnames

qc <- mod[[12]]

from <- c(1,3,11,12) #,16)
to   <- c(2,6,11,14) #,17)
reject <- c("10,11", "1100,1101,1110,1111", "1", "000,110,111") # "11")

qmat <- cbind(from, to, reject)

m <- modis_mask(qc, 16, qmat)
plot(m)
}
}

\keyword{spatial}
