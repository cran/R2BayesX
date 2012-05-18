\name{plotblock}
\alias{plotblock}

\title{
  Factor Variable and Random Effects Plots
}

\description{
  Function to plot effects for model terms including factor, or group variables for random effects, 
  typically used for objects created within \code{\link{bayesx}} or \code{\link{read.bayesx.output}}.
}
 
\usage{
plotblock(x, residuals = FALSE, range = c(0.3, 0.3), 
  col.residuals = "black", col.lines = "black", c.select = NULL, 
  fill.select = NULL , col.polygons = NULL, data = NULL,
  shift = NULL, trans = NULL, ...)
}

\arguments{
  \item{x}{either a \code{list} of length of the unique factors, where each \code{list} element 
    contains the estimated effects for one factor as a matrix, see \code{\link{fitted.bayesx}}, or 
    one data matrix with first column as the group or factor variable. Also formulas are accepted, 
    e.g it is possible to specify the plot with \code{f ~ x} or \code{f1 + f2 ~ x}. By convention, 
    the covariate for which effects should be plotted, is always in the first column in the 
    resulting data matrix, that is used for plotting, i.e. in the second formula example, the data 
    matrix is \code{cbind(x, f1, f2)}, also see argument \code{c.select} and \code{fill.select}.}
  \item{residuals}{if set to \code{TRUE}, partial residuals will be plotted if available. Partial
    residuals may be set as an \code{\link[base]{attr}}ibute of \code{x} named 
    \code{"partial.resids"}, where the partial residuals must be a matrix with first column 
    specifying the covariate, and second column the partial residuals that should be plotted.}
  \item{range}{\code{numeric} vector, specifying the left and right bound of the block.}
  \item{col.residuals}{the color of the partial residuals.}
  \item{col.lines}{vector of maximum length of columns of \code{x} minus 1, specifying the color of 
    the lines.}
  \item{c.select}{\code{integer} vector of maximum length of columns of \code{x}, selects the 
    columns of the resulting data matrix that should be used for plotting. E.g. if \code{x} has 5 
    columns, then \code{c.select = c(1, 2, 5)} will select column 1, 2 and 5 for plotting. Note that 
    first element of \code{c.select} should always be 1, since this is the column of the covariate 
    the effect is plotted for.}
  \item{fill.select}{\code{integer} vector, select pairwise the columns of the resulting data matrix 
    that should form one polygon with a certain background color specified in argument \code{col}. 
    E.g. \code{x} has three columns, or is specified with formula \code{f1 + f2 ~ x}, then setting 
    \code{fill.select = c(0, 1, 1)} will draw a polygon with \code{f1} and \code{f2} as boundaries. 
    If \code{x} has five columns or the formula is e.g. \code{f1 + f2 + f3 + f4 ~ x}, then setting
    \code{fill.select = c(0, 1, 1, 2, 2)}, the pairs \code{f1}, \code{f2} and \code{f3}, \code{f4}
    are selected to form two polygons.}
  \item{col.polygons}{specify the background color for the upper and lower confidence bands, e.g. 
      \code{col = c("green", "red")}.}
  \item{data}{if \code{x} is a formula, a \code{data.frame} or \code{list}. By default the variables 
    are taken from \code{environment(x)}: typically the environment from which \code{plotblock} is 
    called.}
  \item{shift}{numeric. Constant to be added to the smooth before plotting.}
  \item{trans}{function to be applied to the smooth before plotting, e.g., to transform the
    plot to the response scale.}
  \item{\dots}{graphical parameters, please see the details.}
}

\details{
  Function \code{plotblock} draws for every factor or group the effect as a "block" in one graphic, 
  i.e. similar to boxplots, estimated fitted effects, e.g. containing quantiles for \code{MCMC} 
  estimated models, are drawn as one block, where the upper lines represent upper quantiles, the 
  middle line the mean or median, and lower lines lower quantiles, also see the examples. The 
  following graphical parameters may be supplied additionally:
  \itemize{
    \item \code{cex}: specify the size of partial residuals,
    \item \code{lty}: the line type for each column that is plotted, e.g. \code{lty = c(1, 2)},
    \item \code{lwd}: the line width for each column that is plotted, e.g. \code{lwd = c(1, 2)},
    \item \code{poly.lty}: the line type to be used for the polygons,
    \item \code{poly.lwd}: the line width to be used for the polygons,
    \item \code{density} \code{angle}, \code{border}: see \code{\link[graphics]{polygon}},
    \item \code{\dots}: other graphical parameters, see function \code{\link[graphics]{plot}}.
    }
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plot.bayesx}}, \code{\link{bayesx}}, \code{\link{read.bayesx.output}}, 
  \code{\link{fitted.bayesx}}.
}

\examples{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac] + 
  rnorm(n, sd = 0.6))

\dontrun{
## estimate model
b <- bayesx(y ~ fac, data = dat)
summary(b)

## plot factor term effects
plot(b, term = "fac")

## extract fitted effects
f <- fitted(b, term = "fac")

## now use plotblock
plotblock(f)

## some variations
plotblock(f, col.poly = c(2, 3))
plotblock(f, col.poly = NA, lwd = c(2, 1, 1, 1, 1))
plotblock(f, col.poly = NA, lwd = 3, range = c(0.5,0.5))
plotblock(f, col.poly = NA, lwd = 3, col.lines = 1:5, lty = 1)
plotblock(f, col.poly = NA, lwd = 3, col.lines = 1:5, 
  lty = c(3, 1, 2, 2, 1))
plotblock(f, resid = TRUE)
plotblock(f, resid = TRUE, cex = 0.1)
plotblock(f, resid = TRUE, cex = 0.1, col.resid = 2)
plotblock(f, resid = TRUE, cex = 2, col.resid = 3, pch = 3)
plotblock(f, lty = 0, poly.lty = 1, density = c(5, 20))
plotblock(f, lty = 0, poly.lty = 1, density = c(5, 20), 
  poly.lwd = c(1, 2))
plotblock(f, lty = 0, poly.lty = c(1, 2), density = c(5, 20))
plotblock(f, lty = 0, poly.lty = c(1, 2), density = c(5, 20), 
  border = c("red", "green3"))
plotblock(f, lty = 0, poly.lty = c(1, 2), density = c(5, 20), 
  border = c("red", "green3"), col.poly = c("blue", "yellow"))
plotblock(f, lty = c(1,0,0,0,0), poly.lty = c(1, 2), 
  density = c(5, 20), border = c("red", "green3"), 
  col.poly = c("blue", "yellow"))
plotblock(f, lty = c(1,0,0,0,0), poly.lty = c(1, 2), 
  density = c(20, 20), border = c("red", "green3"), 
  col.poly = c("blue", "yellow"), angle = c(10, 75))
}

## another example
plotblock(y ~ fac, data = dat, range = c(0.45, 0.45))

dat <- data.frame(fac = factor(rep(1:10, n/10)))
dat$y <- with(dat, c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac])
plotblock(y ~ fac, data = dat)
plotblock(cbind(y - 0.1, y + 0.1) ~ fac, data = dat)
plotblock(cbind(y - 0.1, y + 0.1) ~ fac, data = dat, 
  fill.select = c(0, 1, 1))
plotblock(cbind(y - 0.1, y + 0.1) ~ fac, data = dat, 
  fill.select = c(0, 1, 1), poly.lty = 2, lty = 1, 
  border = "grey5")
}

\keyword{hplot}
