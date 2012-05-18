\name{GermanyBnd}
\alias{GermanyBnd}

\title{Germany Map}

\description{
  This database produces a map of Germany since 2001 containing 439 administrative districts.
}

\usage{data("GermanyBnd")}

\format{
  A \code{\link[base]{list}} of class \code{"bnd"} containing 466 polygon matrices with 
  x-coordinates in the first and y-coordinates in the second column each. 
}

\source{
  \url{http://www.BayesX.org}.
}

\seealso{\code{\link{plotmap}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}}

\examples{
## load GermanyBnd and plot it
data("GermanyBnd")
plotmap(GermanyBnd)
}

\keyword{datasets}
