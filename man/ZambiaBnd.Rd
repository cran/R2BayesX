\name{ZambiaBnd}
\alias{ZambiaBnd}

\title{Zambia Map}

\description{
  This database produces a map of Zambia containing 57 districts.
}

\usage{data("ZambiaBnd")}

\format{
  A \code{\link[base]{list}} of class \code{"bnd"} containing 57 polygon matrices with 
  x-coordinates in the first and y-coordinates in the second column each. 
}

\source{
  \url{http://www.BayesX.org}.
}

\seealso{\code{\link{plotmap}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}}

\examples{
## load ZambiaBnd and plot it
data("ZambiaBnd")
plotmap(ZambiaBnd)
}

\keyword{datasets}
