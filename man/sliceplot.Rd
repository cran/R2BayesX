\name{sliceplot}
\alias{sliceplot}

\title{
  Plot Slices of Bivariate Functions 
}

\description{
  This function plots slices from user defined values of bivariate surfaces.
}
 
\usage{
sliceplot(x, y = NULL, z = NULL, view = 1, c.select = NULL,
  values = NULL, probs = c(0.1, 0.5, 0.9), grid = 100,
  legend = TRUE, pos = "topright", digits = 2, data = NULL,
  rawdata = FALSE, type = "akima", linear = FALSE,
  extrap = FALSE, k = 40, rug = TRUE, rug.col = NULL,
  jitter = TRUE, ...)
}

\arguments{
  \item{x}{a matrix or data frame, containing the covariates for which the effect should be plotted 
    in the first and second column and at least a third column containing the effect, typically 
    the structure for bivariate functions returned within \code{\link{bayesx}} and 
    \code{\link{read.bayesx.output}} model term objects is used, also see 
    \code{\link{fitted.bayesx}}. Another possibility is to specify the plot via a \code{formula}, 
    e.g. for simple plotting of bivariate surfaces \code{z ~ x + y}, also see the example.}
  \item{y}{if \code{x} is a vector the argument \code{y} and \code{z} must also be supplied as
    vectors.}
  \item{z}{if \code{x} is a vector the argument \code{y} and \code{z} must also be supplied as
    vectors, \code{z} defines the surface given by \eqn{z = f(x, y)}.}
  \item{view}{which variable should be used for the x-axis of the plot, the other variable will be
    used to compute the slices. May also be a \code{character} with the name of the corresponding
    variable.}
  \item{c.select}{\code{integer}, selects the column that is used in the resulting matrix to be
    used as the \code{z} argument.}
  \item{values}{the values of the \code{x} or \code{y} variable that should be used for computing
    the slices, if set to \code{NULL}, slices will be constructed according to the quantiles, see
    also argument \code{probs}.}
  \item{probs}{numeric vector of probabilities with values in [0,1] to be used within function
    \code{\link[stats]{quantile}} to compute the \code{values} for plotting the slices.}
  \item{grid}{the grid size of the surface where the slices are generated from.}
  \item{legend}{if set to \code{TRUE}, a legend with the \code{values} that where used for slicing
    will be added.}
  \item{pos}{the position of the legend, see also function \code{\link[graphics]{legend}}.}
  \item{digits}{the decimal place the legend values should be rounded.}
  \item{data}{if \code{x} is a \code{formula}, a \code{data.frame} or \code{list}. By default the 
    variables are taken from \code{environment(x)}: typically the environment from which 
    \code{plot3d} is called.}
  \item{rawdata}{if set to \code{TRUE}, the data will not be interpolated, only raw data will be
    used. This is useful when displaying data on a regular grid.}
  \item{type}{character. Which type of interpolation metjod should be used. The default is
    \code{type = "akima"}, see function \code{\link[akima]{interp}}. The two other options are
    \code{type = "mba"}, which calls function \code{\link[MBA]{mba.surf}} of package \pkg{MBA}, or
    \code{type = "mgcv"}, which uses a spatial smoother withing package \pkg{mgcv} for interpolation.
    The last option is definitely the slowest, since a full regression model needs to be estimated.}
  \item{linear}{logical. Should linear interpolation be used withing function
     \code{\link[akima]{interp}}?}
  \item{extrap}{logical. Should interpolations be computed outside the observation area
    (i.e., extrapolated)?}
  \item{k}{integer. The number of basis functions to be used to compute the interpolated surface
    when \code{type = "mgcv"}.}
  \item{rug}{add a \code{\link[graphics]{rug}} to the plot.}
  \item{jitter}{if set to \code{TRUE} a \code{\link[base]{jitter}}ed 
      \code{\link[graphics]{rug}} plot is added.}
  \item{rug.col}{specify the color of the rug representation.}
  \item{\dots}{parameters passed to \code{\link{matplot}} and \code{\link[graphics]{legend}}.}
}

\details{
  Similar to function \code{\link{plot3d}}, this function first applies bivariate interpolation
  on a regular \code{grid}, afterwards the slices are computed from the resulting surface. 
}

\note{
  Function \code{sliceplot} uses per default the \pkg{akima} package to construct smooth interpolated
  surfaces, therefore, package \pkg{akima} needs to be installed. The \pkg{akima} package has an ACM
  license that restricts applications to non-commercial usage, see

  \url{http://www.acm.org/publications/policies/softwarecrnotice}
  
  Function \code{sliceplot} prints a note refering to the ACM licence. This note can be supressed by
  setting

  \code{options("use.akima" = TRUE)} 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plot.bayesx}}, \code{\link{bayesx}}, \code{\link{read.bayesx.output}},
  \code{\link{fitted.bayesx}}, \code{\link{plot3d}}.
}

\examples{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(z = runif(n, -3, 3), w = runif(n, 0, 6))

## response
dat$y <- with(dat, 1.5 + cos(z) * sin(w) + rnorm(n, sd = 0.6))

\dontrun{
## estimate model
b <- bayesx(y ~ sx(z, w, bs = "te", knots = 5), data = dat, method = "REML")
summary(b)

## plot estimated effect
plot(b, term = "sx(z,w)", sliceplot = TRUE)
plot(b, term = "sx(z,w)", sliceplot = TRUE, view = 2)
plot(b, term = "sx(z,w)", sliceplot = TRUE, view = "w")
plot(b, term = "sx(z,w)", sliceplot = TRUE, c.select = 4)
plot(b, term = "sx(z,w)", sliceplot = TRUE, c.select = 6)
plot(b, term = "sx(z,w)", sliceplot = TRUE, probs = seq(0, 1, length = 10))
}

## another variation
dat$f1 <- with(dat, sin(z) * cos(w))
sliceplot(cbind(z = dat$z, w = dat$w, f1 = dat$f1))

## same with formula 
sliceplot(sin(z) * cos(w) ~ z + w, ylab = "f(z)", data = dat)

## compare with plot3d()
plot3d(sin(z) * 1.5 * w ~ z + w, zlab = "f(z,w)", data = dat)
sliceplot(sin(z) * 1.5 * w ~ z + w, ylab = "f(z)", data = dat)
sliceplot(sin(z) * 1.5 * w ~ z + w, view = 2, ylab = "f(z)", data = dat)
}

\keyword{hplot}
