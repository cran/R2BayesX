\name{bayesx_logfile}
\alias{bayesx_logfile}
\alias{logfile}

\title{
  BayesX Log-Files 
}

\description{
  Function to show the internal \pkg{BayesX} log-files.
}
 
\usage{
bayesx_logfile(x, model = 1L)
}

\arguments{
  \item{x}{a fitted \code{"bayesx"} object.}
  \item{model}{integer, for which model the log-file should be printed, i.e. if \code{x}
    contains more that one estimated model.}
}

\value{
  The log-file returned from \pkg{BayesX}.
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

## now see the log-file
bayesx_logfile(b)
}
}

\keyword{regression}
