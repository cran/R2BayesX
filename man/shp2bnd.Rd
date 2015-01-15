\name{shp2bnd}
\alias{shp2bnd}

\title{convert a shape-file into a boundary object}

\description{
  Converts the geographical information provided in a shape-file into a boundary object
  (see Chapter 5 of the \pkg{BayesX} Reference Manual)
}

\usage{
  shp2bnd(shpname, regionnames, check.is.in = TRUE)
}

\arguments{
  \item{shpname}{base filename of the shape-file (including path)}
  \item{regionnames}{either a vector of region names or the name of the variable in the
    dbf-file representing these names}
  \item{check.is.in}{test whether some regions are surrounded by other regions (\code{FALSE} speeds
    up the execution time but may result in a corrupted bnd-file)}
}

\value{
  Returns a boundary object, i.e. a list of polygons that form the map. See \code{\link{read.bnd}}
  for more information on the format.
}

\references{
  \pkg{BayesX} Reference Manual. Available at \url{http://www.BayesX.org}.
}

\author{
  Felix Heinzl, Daniel Sabanes Bove, Thomas Kneib with contributions by Michael Hoehle and
  Frank Sagerer.
}

\seealso{
  \code{\link{write.bnd}}, \code{\link{read.bnd}}, \code{\link{plotmap}}.
}

\examples{
## read shapefile into bnd object
shpname <- file.path(find.package("R2BayesX"), "examples", "Northamerica")
north <- shp2bnd(shpname = shpname, regionnames = "COUNTRY")

## draw the map
plotmap(north)
}

\keyword{spatial}
