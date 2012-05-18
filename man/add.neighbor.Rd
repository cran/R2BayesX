\name{add.neighbor}
\alias{add.neighbor}

\title{Add Neighborhood Relations}

\description{
  Adds a neighborhhod relationship between two given regions to a map object in graph format.
}

\usage{
  add.neighbor(map, region1, region2)
}

\arguments{
  \item{map}{map object in graph format that should be modified.}
  \item{region1, region2}{character, names of the regions that should be connected as
    neighbors.}
}

\value{
  Returns an adjacency matrix that represents the neighborhood structure of \code{map} plus the new
  neighborhood relation in graph format.
}

\author{
  Felix Heinzl, Thomas Kneib.
}

\seealso{
  \code{\link{get.neighbor}}, \code{\link{delete.neighbor}}, \code{\link{read.gra}},
  \code{\link{write.gra}}, \code{\link{bnd2gra}}.
}

\examples{
## read the graph file
file <- file.path(find.package("R2BayesX"), "examples", "Germany.gra")
germany <- read.gra(file)

## add some neighbors
get.neighbor(germany, c("1001", "7339"))
germany <- add.neighbor(germany, "7339", "1001")
get.neighbor(germany, c("1001", "7339"))
}

\keyword{spatial}
