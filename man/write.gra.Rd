\name{write.gra}
\alias{write.gra}

\title{Saving Maps in Graph Format}

\description{
  Writes the information of a map object to a file (in graph format).
}

\usage{
write.gra(map, file, replace = FALSE)
}

\arguments{
  \item{map}{map object ot be saved (should be in graph format, see \code{\link{bnd2gra}} for
    the conversion of boundary format to graph format).}
  \item{file}{name of the file to write to}
  \item{replace}{should an existing file be overwritten with the new version?}
}

\references{
  \pkg{BayesX} Reference Manual. Available at \url{http://www.BayesX.org}.
}

\author{
  Thomas Kneib, Felix Heinzl.
}

\seealso{
  \code{\link{read.gra}}, \code{\link{read.bnd}}, \code{\link{write.bnd}}.
}

\examples{
data("FantasyBnd")
tfile <- tempfile()
write.gra(bnd2gra(FantasyBnd), file = tfile)
cat(readLines(tfile), sep = "\n")
unlink(tfile)
}

\keyword{spatial}
