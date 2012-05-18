\name{GCV}
\alias{GCV}
\alias{GCV.bayesx}

\title{
  Gerneralized Cross Validation Criterion
}

\description{
  Generic function returning the generalized cross validation criterium of a fitted model object.
}
 
\usage{
GCV(object, ...)

\method{GCV}{bayesx}(object, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{\dots}{specify for which model the criterion should be returned, e.g. type \code{model = 1}
    to obtain the value for the first model. Only meaningful if \code{object} contains of more than 
    one model.}
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
set.seed(121)
n <- 200

## regressors
dat <- data.frame(x = runif(n, -3, 3))

## generate response 
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x), data = dat, method = "REML")

## extract GCV
GCV(b)
}
}

\keyword{regression}
