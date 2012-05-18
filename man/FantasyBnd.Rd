\name{FantasyBnd}
\alias{FantasyBnd}

\title{Fantasy Map}

\description{
  This database produces a fantasy map of 10 regions.
}

\usage{data("FantasyBnd")}

\format{
  A \code{\link[base]{list}} of class \code{"bnd"} containing 10 polygon matrices with 
  x-coordinates in the first and y-coordinates in the second column each. 
}

\seealso{\code{\link{plotmap}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}}

\examples{
## load FantasyBnd and plot it
data("FantasyBnd")
plotmap(FantasyBnd)
}

\keyword{datasets}
