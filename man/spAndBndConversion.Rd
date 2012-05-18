\name{Interface between sp and bnd format}
\alias{sp2bnd}
\alias{bnd2sp}

\title{Convert sp and bnd format into each other}
\description{
  Convert geographical information objects of class \code{"SpatialPolygons"} (or specializations)
  from \R-package \pkg{sp} to objects of class \code{"bnd"} from \R-package \pkg{R2BayesX} and vice
  versa.
}

\usage{
sp2bnd(spObject, regionNames, height2width, epsilon)
bnd2sp(bndObject)
}

\arguments{
  \item{spObject}{object of class \code{"SpatialPolygons"} (or specializations).} 
  \item{regionNames}{character vector of region names (parallel to the Polygons list in spObject), 
    defaults to the IDs.}
  \item{height2width}{ratio of total height to width, defaults to the bounding box values.}
  \item{epsilon}{how much can two polygons differ (in maximum squared Euclidean distance) and still
    match each other?, defaults to machine precision.} 
  \item{bndObject}{object of class \code{"bnd"}.}
}

\value{
  Equivalent object in the other format.  
}

\author{
  Daniel Sabanes Bove.
}

\seealso{
  \code{\link{nb2gra}}, \code{\link{gra2nb}} for conversion between the neighborhood structure
  formats and \code{\link{read.bnd}}, \code{\link{write.bnd}} for the interface to the
  \pkg{R2BayesX} files.
}

\examples{
## bnd to sp:
file <- file.path(find.package("R2BayesX"), "examples", "Germany.bnd")
germany <- read.bnd(file)
spGermany <- bnd2sp(germany)

## plot the result together with the neighborhood graph
if(require("sp") & require("spdep")) {
  plot(spGermany)
  nbGermany <- poly2nb(spGermany)
  plot(nbGermany, coords = coordinates(spGermany), add = TRUE)

  ## example with one region inside another
  spExample <- spGermany[c("7231", "7235"), ]
  plot(spExample)
  plot(poly2nb(spExample), coords = coordinates(spExample), add = TRUE)

  ## now back from sp to bnd:
  bndGermany <- sp2bnd(spGermany)
  plotmap(bndGermany)

  ## compare names and number of polygons
  stopifnot(
    identical(names(bndGermany), names(germany)),
    identical(length(bndGermany), length(germany))
  )
}
}

\keyword{spatial}

