\name{delete.neighbor}
\alias{delete.neighbor}

\title{Delete Neighborhood Relations}

\description{
  Adds the neighborhhod relationship between two given regions from a map object in graph format.
}

\usage{
delete.neighbor(map, region1, region2)
}

\arguments{
  \item{map}{map object in graph format that should be modified.}
  \item{region1, region2}{names of the regions that should no longer be regarded as neighbors.}
}

\value{
  Returns an adjacency matrix that represents the neighborhood structure of \code{map} minus the
  deleted neighborhood relation in graph format.
}

\author{
  Felix Heinzl, Thomas Kneib.
}

\seealso{
  \code{\link{get.neighbor}}, \code{\link{add.neighbor}}, \code{\link{read.gra}},
  \code{\link{write.gra}}, \code{\link{bnd2gra}}.
}

\examples{
## read the graph file
file <- file.path(find.package("R2BayesX"), "examples", "Germany.gra")
germany <- read.gra(file)

## delete some neighbors
get.neighbor(germany, c("7339"))
germany <- delete.neighbor(germany, "7339", "7141")
get.neighbor(germany, c("7339"))
}

\keyword{spatial}
