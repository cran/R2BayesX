\name{plot.bayesx}
\alias{plot.bayesx}

\title{
  Default BayesX Plotting
}

\description{
  Generic functions for plotting objects of class \code{"bayesx"} and model term classes 
  \code{"geo.bayesx"}, \code{"linear.bayesx"}, \code{"mrf.bayesx"}, \code{"random.bayesx"} and
  \code{"sm.bayesx"}.
}
 
\usage{
\method{plot}{bayesx}(x, model = NULL, term = NULL, which = 1L, ask = FALSE, ...)
}

\arguments{
  \item{x}{a fitted \code{\link{bayesx}} object.}
  \item{model}{for which model the plot should be provided, either an integer or a character, e.g. 
    \code{model = "mcmc.model"}.}
  \item{term}{the term that should be plotted, either an integer or a character, e.g. 
    \code{term = "sx(x)"}.}
  \item{which}{choose the type of plot that should be drawn, possible options are: \code{"effect"},
    \code{"coef-samples"}, \code{"var-samples"}, \code{"intcpt-samples"}, \code{"hist-resid"}, 
    \code{"qq-resid"}, \code{"scatter-resid"}, \code{"scale-resid"}, \code{"max-acf"}. Argument 
    \code{which} may also be specified as integer, e.g. \code{which = 1}. The first three arguments 
    are all model term specific. For the residual model diagnostic plot options \code{which} 
    may be set with \code{which = 5:8}.}
  \item{ask}{\dots}
  \item{\dots}{other graphical parameters passed to \code{\link{plotblock}}, \code{\link{plotmap}}, 
    \code{\link{plot2d}}, \code{\link{plot3d}}, \code{\link[stats]{acf}} and 
    \code{\link[stats]{density}}.}
}

\details{
  Depending on the class of the term that should be plotted, function \code{plot.bayesx} calls one 
  of the following plotting functions in the end:
  \itemize{
    \item \code{\link{plotblock}},
    \item \code{\link{plotsamples}},
    \item \code{\link{plotmap}},
    \item \code{\link{plot2d}},
    \item \code{\link{plot3d}},
    \item \code{\link[stats]{acf}},
    \item \code{\link[stats]{density}},
  }
  For details on argument specifications, please see the help sites for the corresponding function.
  
  If argument \code{x} contains of more than one model and e.g. \code{term = 2}, the second terms
  of all models will be plotted
}

\note{
  If a model is specified with a structured and an unstructured spatial effect, e.g. the model 
  formula is something like \code{y ~ sx(id, bs = "mrf", map = MapBnd) + sx(id, bs = "re")}, the
  model output contains of one additional total spatial effect, named with \code{"sx(id):total"}.
  Also see the last example. 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plotblock}}, \code{\link{plotsamples}}, \code{\link{plotmap}}, \code{\link{plot2d}}, 
  \code{\link{plot3d}}, \code{\link{bayesx}}, \code{\link{read.bayesx.output}}.
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
b1 <- bayesx(y ~ sx(x) + sx(z, w, bs = "te") + fac,
   data = dat, method = "MCMC")

## plot p-spline term
plot(b1, term = 1)
## same with
plot(b1, term = "sx(x)")

## with residuals
plot(b1, term = "sx(x)", residuals = TRUE)

## plot tensor term
plot(b1, term = "sx(z,w)")

## use other palette
plot(b1, term = "sx(z,w)", col.surface = heat.colors)

## swap colors
plot(b1, term = "sx(z,w)", col.surface = heat.colors, swap = TRUE)

## plot tensor term with residuals
plot(b1, term = "sx(z,w)", residuals = TRUE)

## plot image and contour
plot(b1, term = "sx(z,w)", image = TRUE)
plot(b1, term = "sx(z,w)", image = TRUE, contour = TRUE)

## increase the grid
plot(b1, term = "sx(z,w)", image = TRUE, contour = TRUE, grid = 100)

## plot factor term
plot(b1, term = "fac")

## plot factor term with residuals
plot(b1, term = "fac", resid = TRUE, cex = 0.5)

## plot residual dignostics
plot(b1, which = 5:8)

## plot variance sampling path of term sx(x)
plot(b1, term = 1, which = "var-samples")

## plot coefficients sampling paths of term sx(x)
plot(b1, term = 1, which = "coef-samples")

## plot the sampling path of the intercept
par(mfrow = c(1, 1))
plot(b1, which = "intcpt-samples")

## plot the autcorrelation function  
## of the sampled intercept
plot(b1, which = "intcpt-samples", 
  acf = TRUE, lag.max = 50)

## increase lags
plot(b1, which = "intcpt-samples", 
  acf = TRUE, lag.max = 200)

## plot maximum autocorrelation 
## of all sampled parameters in b1
plot(b1, which = "max-acf")

## plot maximum autocorrelation of 
## all sampled coefficients of term sx(x)
plot(b1, term = "sx(x)", which = "coef-samples", 
  max.acf = TRUE, lag.max = 100)


## now a spatial example
set.seed(333)

## simulate some geographical data
data("MunichBnd")
N <- length(MunichBnd); names(MunichBnd) <- 1:N
n <- N*5

## regressors
dat <- data.frame(id = rep(1:N, n/N), x1 = runif(n, -3, 3))
dat$sp <- with(dat, sort(runif(N, -2, 2), decreasing = TRUE)[id])
dat$re <- with(dat, rnorm(N, sd = 0.6)[id])

## response
dat$y <- with(dat, 1.5 + sin(x1) + sp + re + rnorm(n, sd = 0.6))

## estimate model
b2 <- bayesx(y ~ sx(x1) + sx(id, bs = "mrf", map = MunichBnd) +
  sx(id, bs = "re"), method = "MCMC", data = dat)

## summary statistics
summary(b2)

## plot structured spatial effect
plot(b2, term = "sx(id)", map = MunichBnd)

## plot unstructured spatial effect
plot(b2, term = "sx(id):re", map = MunichBnd)

## now without map
## generates a kernel density plot
## of the effects
plot(b2, term = "sx(id):mrf", map = FALSE)
plot(b2, term = "sx(id):re", map = FALSE)

## with approximate quantiles of the  
## kernel density estimate 
plot(b2, term = "sx(id):re", map = FALSE, 
  kde.quantiles = TRUE, probs = c(0.025, 0.5, 0.975))

## plot the total spatial effect
plot(b2, term = "sx(id):total")
plot(b2, term = "sx(id):total", map = MunichBnd)

## combine model objects
b <- c(b1, b2)

## plot first term of second model
plot(b, model = 2, term = 1)
plot(b, model = "b2", term = "sx(x1)")

## plot second term of both models
plot(b, term = 2, map = MunichBnd)

## plot everything
plot(b)
}
}

\keyword{hplot}
