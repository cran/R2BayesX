\name{BeechBnd}
\alias{BeechBnd}

\title{Beech Location Map}

\description{
  This database produces a location map of beeches around Rothenbuch, Germany.
}

\usage{data("BeechBnd")}

\format{
  A \code{\link[base]{list}} of class \code{"bnd"} containing 83 polygon matrices with 
  x-coordinates in the first and y-coordinates in the second column each. 
}

\source{
  \url{http://www.BayesX.org}.
}

\seealso{\code{\link{plotmap}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}}

\examples{
## load BeechBnd and plot it
data("BeechBnd")
plotmap(BeechBnd)
}

\keyword{datasets}
