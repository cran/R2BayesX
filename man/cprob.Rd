\name{cprob}
\alias{cprob}

\title{
  Extract Contour Probabilities  
}

\description{
  Function to extract estimated contour probabilities of a particular effect estimated with 
  P-splines using Markov chain Monte Carlo (MCMC) estimation techniques. Note that, the contour
  probability option must be specified within function \code{\link{sx}}, see the example.
}
 
\usage{
cprob(object, model = NULL, term = NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{model}{for which model the contour probabilities should be provided, either an integer or a 
    character, e.g. \code{model = "mcmc.model"}.}
  \item{term}{if not \code{NULL}, the function will search for the term contour probabilities should
    be extracted for, either an integer or a character, eg \code{term = "s(x)"}.}
  \item{\dots}{not used.}
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\references{
  Brezger, A., Lang, S. (2008): Simultaneous probability statements for Bayesian P-splines. 
    \emph{Statistical Modeling}, \bold{8}, 141--186.
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
## need to set the contourprob option, 
## otherwise BayesX will not calculate probabilities
## see also the reference manual of BayesX available
## at www.BayesX.org
b <- bayesx(y ~ sx(x, bs = "ps", contourprob = 4), data = dat)

## extract contour probabilities
cprob(b, term = "sx(x)")
}
}

\keyword{regression}
