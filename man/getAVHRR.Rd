\name{getAVHRR}

\alias{getAVHRR}

\title{Find and download AVHRR NDVI Climate Data Record (CDR)}

\description{
  Find and download AVHRR NDVI CDR global layers for a time period. The function returned is list of filenames that may have been downloaded.
}

\usage{
getAVHRR(start_date, end_date, path, download=FALSE, overwrite=FALSE, update=FALSE, quiet=FALSE, ...)
}

\arguments{
  \item{start_date}{character. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character. end date for the data requested formatted yyyy-m-d}
  \item{path}{character. Path to download data to}
  \item{download}{logical. If \code{TRUE}, the files are downloaded. If \code{FALSE} (the default), only the file names that match the date range are returned}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{update}{logical. If \code{TRUE}, the cached index of available AVHRR NDVI CDR files is refreshed}
  \item{quiet}{logical. If \code{TRUE}, messages and the download progress bar are suppressed}
  \item{...}{Additional arguments passed to \code{\link[utils]{download.file}} (for example \code{method})}
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

