\name{predict.bayesx}
\alias{predict.bayesx}

\title{
  Prediction from fitted BayesX objects
}

\description{
  Takes a fitted \code{"bayesx"} object returned from \code{\link{bayesx}} and produces predictions
  by refitting the initial model with weights set to zero for new observations.
}
 
\usage{
\method{predict}{bayesx}(object, newdata, model = NULL,
  type = c("response", "link", "terms", "model"),
  na.action = na.pass, digits = 5, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"} or \code{"bayesx.hpc"}.}
  \item{newdata}{a data frame or list containing the values of the model covariates at which
    predictions are required. If missing \code{newdata} is the \code{model.frame} of the provided
    model.}
  \item{model}{for which model should predictions be calculated, either an integer or 
    a character, e.g. \code{model = "mcmc.model"}. Note that exactly one model must be selected
    within argument \code{model} to compute predicted values!}
  \item{type}{when \code{type = "response"}, the default, predictions on the scale of the response
    are returned, \code{"link"} returns the linear predictor. When \code{type = "terms"}, each
    component of the linear predictor is returned, but excludes any offset and intercept.
    If \code{type = "model"}, the full model returned from updating the initial model with weights,
    that is used for computing predictions, is returned.}
  \item{na.action}{function determining what should be done with missing values in \code{newdata}.}
  \item{digits}{predictions should usually be based on the new values provided in argument
    \code{newdata}. However, since this prediction method uses refitting of the model with
    weights, predictions for model terms need to be matched with the new observations. \pkg{BayesX}
    returns values with a lower precision than \R, therefore argument \code{digits} is used to round
    values when \code{type = "terms"}, to find matching \code{newdata} pairs in the fitted objects
    returned from the refitted model and the new data. Note that this is a workaround and not 100\%
    bulletproof. It is recommended to compute predictions for \code{type = "response"} or
    \code{type = "link"}.}
  \item{\dots}{not used.}
}

\value{ 
  Depending on the specifications of argument \code{type}.
}

\note{
  This prediction method is based on refitting the initial model with weights, i.e., if new
  observations lie outside the domain of the respective covariate, the knot locations when using
  e.g. P-splines are calculated using the old and the new data. Hence, if there are large gaps
  between the old data domain and new observations, this could affect the overall fit of the
  estimated spline, i.e., compared to the initial model fit there will be smaller or larger
  differences depending on the \code{newdata} provided. 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{fitted.bayesx}}, \code{\link{bayesx}}.
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
b <- bayesx(y ~ sx(x) + z + w, data = dat)

## create some data for which predictions are required
nd <- data.frame(x = seq(2, 5, length = 100), z = 1, w = 0)

## prediction model from refitting with weights
nd$fit <- predict(b, newdata = nd)
plot(fit ~ x, type = "l", data = nd)
}
}

\keyword{regression}
