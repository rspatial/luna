\name{modisDate}

\alias{modisDate}

\title{Extract the date from a MODIS filename}

\description{
Extract the date that is encoded in a MODIS filename
}

\usage{
getModisYMD(filename)
}

\arguments{
  \item{filename}{character. MODIS product filenames}
}


\value{
data.frame
}

\seealso{ \code{\link{getModis}}} 

\examples{  
modisDate("MOD09A1.A2019137.h21v09.006.2019150085736.hdf")
}

