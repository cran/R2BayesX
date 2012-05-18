\name{MunichBnd}
\alias{MunichBnd}

\title{Munich Map}

\description{
  This database produces a city map of Munich containing 105 administrative districts.
}

\usage{data("MunichBnd")}

\format{
  A \code{\link[base]{list}} of class \code{"bnd"} containing 106 polygon matrices with 
  x-coordinates in the first and y-coordinates in the second column each. 
}

\source{
  \url{http://www.BayesX.org}.
}

\seealso{\code{\link{plotmap}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}}

\examples{
## load MunichBnd and plot it
data("MunichBnd")
plotmap(MunichBnd)
}

\keyword{datasets}
