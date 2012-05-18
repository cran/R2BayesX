\name{term.freqs}
\alias{term.freqs}

\title{
  Extract model term selection frequencies.
}

\description{
  This function takes a fitted \code{\link{bayesx}} object and returns selection frequency tables of
  model terms. These tables are only returned using the stepwise procedure in combination with
  the bootstrap confidence intervals, see function \code{\link{bayesx.control}}.
}
 
\usage{
term.freqs(object, model = NULL, term = NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{model}{for which model the tables should be provided, either an integer or a character, 
    e.g. \code{model = "mcmc.model"}.}
  \item{term}{character or integer. The term for which the frequency table should be extracted.}
  \item{\dots}{not used.}
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{bayesx}}, \code{\link{bayesx.control}}.
}

\examples{
\dontrun{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, -1, 1),
   w = runif(n, 0, 1), fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x) + sx(z) + sx(w) + sx(fac, bs = "re"),
   method = "STEP", CI = "MCMCbootstrap", bootstrapsamples = 99,
   data = dat)
summary(b)

## extract frequency tables
term.freqs(b)
}
}

\keyword{regression}
