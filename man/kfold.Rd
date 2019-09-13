\name{kfold}

\alias{kfold}

\title{
k-fold partitioning
}

\description{
k-fold partitioning of a data set for model testing purposes. Each record in a matrix (or similar data structure) is randomly assigned to a group. Group numbers are between 1 and \code{k}. 
}


\usage{
kfold(x, k=5, by)
}
 
\arguments{
  \item{x}{vector, matrix, data.frame}
  \item{k}{number of groups}
  \item{by}{Optional argument. A vector or factor with sub-groups (e.g. species). Its length should be the same as the number of records in x}
}

\value{
a vector with group assignments
}

\author{
Robert J. Hijmans
}

\examples{

k <- kfold(1:100, 5)
k[1:25]
table(k)

}


