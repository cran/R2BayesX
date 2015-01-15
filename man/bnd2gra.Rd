\name{bnd2gra}
\alias{bnd2gra}

\title{Convert Boundary Format to Graph Format}

\description{
  Converts a map in boundary format to a map in graph format.
}

\usage{
bnd2gra(map, npoints = 2)
}

\arguments{
  \item{map}{map in boundary format that should be converted.}
  \item{npoints}{integer. How many points must be shared by two polygons to be a neighbor?}
}

\value{
  Returns an adjacency matrix that represents the neighborhood structure of the map object in
  graph format.
}

\references{
  \pkg{BayesX} Reference Manual. Available at \url{http://www.BayesX.org}.
}

\author{
  Felix Heinzl, Thomas Kneib.
}

\seealso{
  \code{\link{read.bnd}}, \code{\link{read.gra}}, \code{\link{write.bnd}}, \code{\link{write.gra}}.
}

\examples{
data("FantasyBnd")
plotmap(FantasyBnd, names = TRUE)
adjmat <- bnd2gra(FantasyBnd)
adjmat
}

\keyword{spatial}
