\name{fitted.bayesx}
\alias{fitted.bayesx}
\alias{residuals.bayesx}

\title{
  Extract BayesX Fitted Values and Residuals
}

\description{
  Extractor functions to the fitted values/model residuals of the estimated model with 
  \code{\link{bayesx}} and fitted model term partial effects/residuals.
}
 
\usage{
\method{fitted}{bayesx}(object, model = NULL, term = NULL, ...)

\method{residuals}{bayesx}(object, model = NULL, term = NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{model}{for which model the fitted values/residuals should be provided, either an integer or 
    a character, e.g. \code{model = "mcmc.model"}.}
  \item{term}{if not \code{NULL}, the function will search for the term fitted values/residuals 
    specified here, either an integer or a character, eg \code{term = "sx(x)"}.}
  \item{\dots}{not used.}
}

\value{ 
  For \code{fitted.bayesx}, either the fitted linear predictor and mean or if e.g. 
  \code{term = "sx(x)"}, an object with class \code{"xx.bayesx"}, where \code{"xx"} is depending of 
  the type of the term. In principle the returned term object is simply a \code{\link{data.frame}} 
  containing the covariate(s) and its effects, depending on the estimation method, e.g. for MCMC
  estimated models, mean/median fitted values and other quantities are returned. Several additional
  informations on the term are provided in the \code{\link{attributes}} of the object. For all types
  of terms plotting functions are provided, see function \code{\link{plot.bayesx}}.

  Using \code{residuals.bayesx} will either return the mean model residuals or the mean partial 
  residuals of a term specified in argument \code{term}.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{read.bayesx.output}}.
}

\examples{
\dontrun{
## generate some data
set.seed(121)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, 0, 1),
  w = runif(n, 0, 3))

## generate response 
dat$y <- with(dat, 1.5 + sin(x) + z -3 * w + rnorm(n, sd = 0.6))

## estimate model
b1 <- bayesx(y ~ sx(x) + z + w, data = dat)

## extract fitted values
fit <- fitted(b1)
hist(fit, freq = FALSE)

## now extract 1st model term
## and plot it
fx <- fitted(b1, term = "sx(x)")
plot(fx)

## extract model residuals
hist(residuals(b1))

## extract partial residuals for sx(x)
pres <- residuals(b1, term = "sx(x)")
plot(fx, ylim = range(pres[, 2]))
points(pres)
}

## now another example with
## use of read.bayesx.output
## load example data from
## package R2BayesX
dir <- file.path(find.package("R2BayesX"), "examples", "ex01")
b2 <- read.bayesx.output(dir)

## extract fitted values
hist(fitted(b2))

## extract model term of x
## and plot it
fx <- fitted(b2, term = "sx(x)")
plot(fx)


## have a look at the attributes
names(attributes(fx))

## extract the sampling path of the variance
spv <- attr(fx, "variance.sample")
plot(spv, type = "l")


\dontrun{
## combine model objects
b <- c(b1, b2)

## extract fitted terms for second model
fit <- fitted(b, model = 2, term = 1:2)
names(fit)
plot(fit["sx(id)"])
}
}

\keyword{regression}
