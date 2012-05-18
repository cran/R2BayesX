\name{bayesx_runtime}
\alias{bayesx_runtime}
\alias{runtime}

\title{
  BayesX Program-Runtimes
}

\description{
  Function to extract running times of the \pkg{BayesX} binary.
}
 
\usage{
bayesx_runtime(x, model = 1L)
}

\arguments{
  \item{x}{a fitted \code{"bayesx"} object.}
  \item{model}{integer, for which model the program-file should be printed, i.e. if \code{x}
    contains more that one estimated model.}
}

\value{
  The runtime of the \pkg{BayesX} binary returned form \code{\link[base]{system.time}}.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{bayesx}}.
}

\examples{
\dontrun{
## generate some data
set.seed(111)
n <- 500

## regressor
dat <- data.frame(x = runif(n, -3, 3))

## response 
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x), data = dat)

## now see the prg-file
bayesx_runtime(b)
}
}

\keyword{regression}
