\name{Interface between nb and gra format}
\alias{nb2gra}
\alias{gra2nb}

\title{Convert nb and gra format into each other}

\description{
  Convert neighborhood structure objects of class \code{"nb"} from R-package \pkg{spdep} to graph
  objects of class \code{"gra"} from R-package \pkg{R2BayesX} and vice versa.  
}
\usage{
nb2gra(nbObject)
gra2nb(graObject)
}

\arguments{
  \item{nbObject}{neighborhood structure object of class \code{"nb"}}
  \item{graObject}{graph object of class \code{"gra"}}
}
\value{
  Equivalent object in the other format.
}

\author{
  Daniel Sabanes Bove.
}

\seealso{
  \code{\link{sp2bnd}}, \code{\link{bnd2sp}} for conversion between the geographical information
  formats and \code{\link{read.gra}}, \code{\link{write.gra}} for the interface to the
  \pkg{R2BayesX} files.
}
 
\examples{
## first nb to gra:
if(require("spdep")) {
  example(columbus)
  colNb <- poly2nb(columbus)

  ## ... here manual editing is possible ...
  ## then export to graph format
  colGra <- nb2gra(colNb)

  ## and save in BayesX file
  graFile <- tempfile()
  write.gra(colGra, file=graFile)

  ## now back from gra to nb:
  colGra <- read.gra(graFile)
  newColNb <- gra2nb(colGra)
  newColNb

  ## compare this with the original
  colNb

  ## only the call attribute does not match (which is OK):
  all.equal(newColNb, colNb, check.attributes = FALSE)
  attr(newColNb, "call")
  attr(colNb, "call")
}
}

\keyword{spatial}
