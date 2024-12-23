\name{getAVHRR}

\alias{getAVHRR}

\title{Find and download AVHRR NDVI Climate Data Record (CDR)}

\description{
  Find and download AVHRR NDVI CDR global layers for a time period. The function returned is list of filenames that may have been downloaded.
}

\usage{
getAVHRR(start_date, end_date, path, overwrite=FALSE, update=FALSE, download=TRUE, quiet=FALSE, ...)
}

\arguments{
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{path}{character. Path to download data to}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{update}{logical. If \code{TRUE}, existing records of AVHRR NDVI CDR will be updated}
  \item{download}{logical. Should the files be downloaded if not available on \code{path}?}
  \item{quiet}{logical. Should messages be printed to track the download progress?}
  \item{...}{Additional arguments that can be passed to \code{download.file})}
}


\value{
  Character vector of file names pointing to the downloaded files
}


\examples{  
\dontrun{
  start <- "2019-05-17" 
  end <- "2019-05-23"
  path <- tempdir()
  ff <- getAVHRR(start, end, path, download=FALSE)
}
}

