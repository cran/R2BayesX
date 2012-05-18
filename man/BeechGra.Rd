\name{BeechGra}
\alias{BeechGra}

\title{Beech Neighborhood Information}

\description{
  This database produces a graph file including neighborhood information of the beech trees around 
  Rothenbuch, Germany.
}

\usage{data("BeechGra")}

\format{
  An adjacency matrix that represents the neighborhood structure defined in the graph file. 
}

\source{
  \url{http://www.BayesX.org}.
}

\seealso{\code{\link{read.gra}}, \code{\link{bnd2gra}}}

\examples{
## load BeechGra adjacency matrix
data("BeechGra")
print(BeechGra)
}

\keyword{datasets}
