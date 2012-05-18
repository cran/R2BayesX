\name{summary.bayesx}
\alias{summary.bayesx}

\title{
  Bayesx Summary Statistics
}

\description{
  Takes an object of class \code{"bayesx"} and displays summary statistics.
}
 
\usage{
\method{summary}{bayesx}(object, model = NULL, 
  digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{model}{for which model the plot should be provided, either an integer or a character, e.g. 
    \code{model = "mcmc.model"}.}
  \item{digits}{choose the decimal places of represented numbers in the summary statistics.}
  \item{\dots}{not used.}
}

\details{
  This function supplies detailed summary statistics of estimated objects with \pkg{BayesX}, i.e. 
  informations on smoothing parameters or variances are supplied, as well as random effects 
  variances and parametric coefficients. Depending on the model estimated and the output provided, 
  additional model specific information will be printed, e.g. if \code{method = "MCMC"} was 
  specified in \code{\link{bayesx}}, the number of \code{iterations}, the \code{burnin} and so forth 
  is shown. Also goodness of fit statistics are provided if the \code{object} contains such 
  informations.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{bayesx}}, \code{\link{read.bayesx.output}}.
}

\examples{
\dontrun{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, -3, 3),
   w = runif(n, 0, 6), fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + sin(x) + cos(z) * sin(w) +
   c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac] + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x) + sx(z, w, bs = "te") + fac,
   data = dat, method = "MCMC")

## now show summary statistics
summary(b)
}
}

\keyword{regression}
