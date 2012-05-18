\name{read.gra}
\alias{read.gra}

\title{Read Geographical Information in Graph Format}

\description{
  Reads the geographical information provided in a file in graph format and stores it in a map
  object.
}

\usage{
read.gra(file, sorted = FALSE, sep = " ")
}

\arguments{
  \item{file}{the file path of the graph file to be read.}
  \item{sorted}{logical. Should the regions be ordered by the numbers specifying the region names
    (\code{sorted = TRUE})?}
  \item{sep}{the field separator character. Values on each line of the file are separated by this 
    character.}
}

\details{
  A graph file stores the nodes and the edges of a graph and is a convenient way to represent the
  neighborhood structure of a geographical map. The structure of a graph file is given by:
  \itemize{
    \item The first line of the graph file specifies the total number of nodes.
    \item The subsequent three lines correspond to the node with the name given in line 2,
      the number of neighbors in line 3 and the neighboring node identity numbers in line 4.
  }
  Note that the note identity numbering starts with \code{0}. Example taken from the package
  example file \code{Germany.gra}:
  \tabular{l}{
    309 \cr
    1001 \cr
    1 \cr
    1 \cr
    1059 \cr
    3 \cr
    0 3 4 \cr
    1002 \cr
    2 \cr
    5 4 \cr
    1051 \cr
    3 \cr
    4 1 9 \cr
    1058 \cr
    7 \cr
    2 6 3 5 1 10 9 \cr
    \dots
  }
  Hence, this graph file contains of 309 regions. The first region with name 1001 has 1 neighbor
  with neighboring node identity number 1. The last region in this example, region 1058, has 7
  neighbors with neighboring node identity numbers 2 6 3 5 1 10 9.
  
  In addition, graph files using the following format may be imported:
  \itemize{
    \item The first line of the graph file specifies the total number of nodes.
    \item The subsequent lines start with the node name followed by the number of neighbors and
      the neighboring node identity numbers.
  }
  Example:
  \tabular{l}{
    309 \cr
    1001 1 2 \cr
    1059 3 1 4 5 \cr
    1002 2 6 5 \cr
    1051 3 5 2 10 \cr
    1058 7 3 7 4 6 2 11 10 \cr
    \dots
  }
}

\value{
  Returns an adjacency matrix that represents the neighborhood structure defined in the graph file.
  The diagonal elements of this matrix are the number of neighbors of each region. The off-diagonal
  elements are either \code{-1} if regions are neighbors else \code{0}.
}

\references{
  \pkg{BayesX} Reference Manual, Chapter 5. Available at \url{http://www.BayesX.org}.
}

\author{
  Thomas Kneib, Felix Heinzl, rewritten by Nikolaus Umlauf.
}

\seealso{
  \code{\link{write.gra}}, \code{\link{read.bnd}}, \code{\link{write.bnd}},
  \code{\link{get.neighbor}}, \code{\link{add.neighbor}}, \code{\link{delete.neighbor}}.
}

\examples{
file <- file.path(find.package("R2BayesX"), "examples", "Germany.gra")
germany <- read.gra(file)
}

\keyword{spatial}
