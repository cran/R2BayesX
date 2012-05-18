\name{write.bnd}
\alias{write.bnd}

\title{Saving Maps in Boundary Format}

\description{
  Writes the information of a map object to a file (in boundary format)
}

\usage{
  write.bnd(map, file, replace = FALSE)
}

\arguments{
  \item{map}{pap object ot be saved (should be in boundary format).}
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
  \code{\link{read.bnd}}, \code{\link{write.gra}}, \code{\link{read.gra}}.
}

\examples{
data("FantasyBnd")
tfile <- tempfile()
write.bnd(FantasyBnd, file = tfile)
cat(readLines(tfile), sep = "\n")
unlink(tfile)
}

\keyword{spatial}
