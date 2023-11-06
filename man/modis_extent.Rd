\name{modisExtent}

\alias{modisExtent}

\title{Get the extent of a MODIS tile}

\description{
Get the extent of a MODIS tile from the h (horizontal; column number) and v (vertical; row number) identifier in the filename 
}

\usage{
modisExtent(f=NULL, h, v)
}

\arguments{
  \item{f}{character. filename that includes a h12v09 pattern}
  \item{h}{integer between 0 and 35}
  \item{v}{integer between 0 and 17}
}


\value{
SpatExtent
}

\seealso{ \code{\link{modisDate}}} 

\examples{  
f <- "MOD09A1.A2019137.h21v09.006.2019150085736.hdf"
modisExtent(f)

modisExtent(h=21, v=9)
}

