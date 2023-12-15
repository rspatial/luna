\name{getERS}

\alias{getERS}

\title{Download data from ERS}

\description{
Find and download ERS data 
}

\usage{
getERS(scenes, path, username, password, overwrite=FALSE, ...)
}

\arguments{
  \item{scenes}{character}
  \item{path}{character. Path name indicating where to store the data}
  \item{username}{character. User name}
  \item{password}{character. Password}
  \item{overwrite}{logical. If \code{TRUE}, existing files are overwritten}
  \item{...}{additional arguments. None implemented}
}


\value{
Character vector of file names pointing to the downloaded files
}

\seealso{\code{\link{getLandsat}}} 
