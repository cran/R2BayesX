\name{read.bnd}
\alias{read.bnd}

\title{Read Geographical Information in Boundary Format}

\description{
  Reads the geographical information provided in a file in boundary format and stores it in a map
  object.
}

\usage{
read.bnd(file, sorted = FALSE)
}

\arguments{
  \item{file}{name of the boundary file to be read.}
  \item{sorted}{should the regions be ordered by the numbers speciying the region names
    (\code{sorted = TRUE})?}
}

\details{
  A boundary file provides the boundary information of a geographical map in terms of closed
  polygons. For each region of the map, the boundary file contains a block of lines defining the
  name of the region, the number of lines the polygon consists of, and the polygons themselves.
  The first line of such a block contains the region code surrounded by quotation marks and the
  number of lines the polygon of the region consists of. The region code and the number of lines
  must be separated by a comma. The subsequent lines contain the coordinates of the straight lines
  that form the boundary of the region. The straight lines are represented by the coordinates of
  their end points. Coordinates must be separated by a comma.

  The following is an example of a boundary file as provided in file \code{Germany.bnd} in the
  examples folder of this package.
  \tabular{l}{
    "1001",9 \cr
    2534.64771,8409.77539 \cr
    2554.54712,8403.92285 \cr
    2576.78735,8417.96973 \cr
    2592.00439,8366.46582 \cr
    2560.39966,8320.81445 \cr
    2507.72534,8319.64453 \cr
    2496.02002,8350.07813 \cr
    2524.11304,8365.29492 \cr
    2534.64771,8409.77539 \cr
    "1002",18 \cr
    2987.64697,7774.17236 \cr
    2954.87183,7789.38916 \cr
    \dots
  }
  Hence, the region code of the first region is "1001" and contains of 9 points that form its
  polygon. The second region has region code "1002" and contains of 18 polygon points (note that
  only the first two points are shown).
}

\value{
  Returns a list of polygons that form the map. Additional attributes are
  \item{surrounding}{Parallel list where for each polygon, the name of a possible
    surrounding region is saved.}
  \item{height2width}{Ratio between height and width of the map. Allows customised drawing and
     storage in files by specifying the appropriate height and width.}
}

\references{
  \pkg{BayesX} Reference Manual. Available at \url{http://www.BayesX.org}.
}

\author{
  Daniel Sabanes Bove, Felix Heinzl, Thomas Kneib, Andreas Brezger.
}

\seealso{
  \code{\link{write.bnd}}, \code{\link{plotmap}}, \code{\link{read.gra}}, \code{\link{write.gra}}.
}

\examples{
file <- file.path(find.package("R2BayesX"), "examples", "Germany.bnd")
germany <- read.bnd(file)
plotmap(germany)
}

\keyword{spatial}
