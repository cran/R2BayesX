\name{plotsamples}
\alias{plotsamples}

\title{
  Plot Sampling Path(s) of Coefficient(s) and Variance(s) 
}

\description{
  This function plots the sampling paths of coefficient(s) and variance(s) stored in model term
  objects typically returned from function \code{\link{bayesx}} or \code{\link{read.bayesx.output}}.
}
 
\usage{
plotsamples(x, selected = "NA", acf = FALSE, var = FALSE,
  max.acf = FALSE, subset = NULL, ...)
}

\arguments{
  \item{x}{a vector or matrix, where each column represents a different sampling path to be 
    plotted.}
  \item{selected}{a character string containing the term name the sampling paths are plotted for.}
  \item{acf}{if set to \code{TRUE}, the autocorrelation function for each sampling path is plotted.}
  \item{var}{indicates whether coefficient or variance sampling paths are displayed and simply 
    changes the main title.}
  \item{max.acf}{if set to \code{TRUE}, \code{plotsamples} will evaluate the maximum
    autocorrelation over all parameters of each sample.}
  \item{subset}{integer. An index which selects the coefficients for which sampling paths
    should be plotted.}
  \item{\dots}{other graphical parameters to be passed to \code{\link[graphics]{plot}} and
    \code{\link[stats]{acf}}, e.g. argument \code{lag.max} if \code{acf = TRUE}. An argument
    \code{ask} controls the display when more than 12 sampling paths should be plotted.}
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plot.bayesx}}, \code{\link{bayesx}}, \code{\link{read.bayesx.output}}.
}

\examples{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3))

## response
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

\dontrun{
## estimate model
b <- bayesx(y ~ sx(x), data = dat)
summary(b)

## plot sampling path for
## the variance
plot(b, term = "sx(x)", which = "var-samples")

## plot sampling paths for 
## coefficients
plot(b, term = "sx(x)", which = "coef-samples")

## plot maximum autocorrelation of
## all sampled parameters of term s(x)
plot(b, term = "sx(x)", which = "coef-samples", max.acf = TRUE)

## extract samples of term sx(x)
sax <- as.matrix(samples(b, term = "sx(x)"))

## now use plotsamples
plotsamples(sax, selected = "sx(x)")

## some variations
plotsamples(sax, selected = "sx(x)", acf = TRUE)
plotsamples(sax, selected = "sx(x)", acf = TRUE, lag.max = 200)
}
}

\keyword{hplot}
