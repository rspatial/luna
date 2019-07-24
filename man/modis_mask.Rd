\name{modis_mask}

\alias{modis_mask}
\alias{unpack_reject}

\title{MODIS mask}

\description{MODIS mask}

\usage{
modis_mask(x, bits, se, reject, ...)
unpack_reject(val, bits=32, se, reject)
}

\arguments{
  \item{x}{SpatRaster}
  \item{bits}{single integer value: 16 or 32}
  \item{se}{matrix}
  \item{reject}{list}
  \item{\dots}{additional arguments, none implemented}\
  \item{val}{integer}  
}

\value{
SpatRaster
}


\author{
Authors
}

\examples{
\dontrun{
f <- "MOD09A1.A2011009.h21v08.006.2015216112834.hdf"
mod <- rast(f)
shortnames <- substr(names(mod), 86, 120)
names(mod) <- shortnames

qc <- mod[[12]]

se <- matrix(c(1,2,3,6,11,11,12,14,16,16), ncol=2, byrow=TRUE) 
reject <- list(c("10", "11"), c("1100","1101","1110","1111"), "1", c("000","110","111"), "11")

m <- modis_mask(qc, 16, se, reject)
plot(m)
}
}

\keyword{spatial}
