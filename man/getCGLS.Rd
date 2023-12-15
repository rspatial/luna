\name{getCGLS}

\alias{getCGLS}

\title{Download CGLS products}

\description{
Download data provided by the Copernicus Global Land Service (https://land.copernicus.eu/global/)
You need to register at https://land.copernicus.eu/global/ and create a username and password.

See https://land.copernicus.eu/global/products/ for a product overview and product details and see https://land.copernicus.vgt.vito.be/manifest/ for an overview for data availability.
}

\usage{
getCGLS(product, start_date, end_date, resolution="1km", 
		version=1, path, username, password, ...)
}

\arguments{
  \item{product}{character. For example: "fapar", "fcover", "lai", "ndvi", "ssm", "swi", "lst"}
  \item{resolution}{character. one of "1km", "300m" or "100m"}
  \item{version}{positive integer. product version}
  \item{start_date}{character or Date. Start date for the data requested formatted yyyy-m-d}
  \item{end_date}{Character or Date. End date for the data requested formatted yyyy-m-d}
  \item{path}{character. Path name indicating where to store the data}
  \item{username}{character. EOSDIS user name (see Details)}
  \item{password}{character. EOSDIS password (see Details)}
  \item{...}{Additional arguments passed to download.file)}
}

\value{
SpatRaster
}

\seealso{\code{\link{getNASA}}} 

