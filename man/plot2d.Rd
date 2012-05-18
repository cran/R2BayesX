\name{plot2d}
\alias{plot2d}
\alias{plotnonp}
  
\title{
  2D Effect Plot 
}
  
\description{
  Function to plot simple 2D graphics for univariate effects/functions, typically used for objects 
  of class \code{"linear.bayesx"} and \code{"sm.bayesx"} returned from function \code{\link{bayesx}} 
  and \code{\link{read.bayesx.output}}.
}
 
\usage{
plot2d(x, residuals = FALSE, rug = TRUE, jitter = TRUE, 
  col.residuals = NULL, col.lines = NULL, col.polygons = NULL, 
  col.rug = NULL, c.select = NULL, fill.select = NULL, 
  data = NULL, sep = "", month = NULL, year = NULL,
  step = 12, shift = NULL, trans = NULL, ...)
}

\arguments{
  \item{x}{a matrix or data frame, containing the covariate for which the effect should be plotted 
    in the first column and at least a second column containing the effect, typically the structure 
    for univariate functions returned within \code{\link{bayesx}} and 
    \code{\link{read.bayesx.output}} model term objects is used, also see 
    \code{\link{fitted.bayesx}}. Another possibility is to specify the plot via a \code{formula}, 
    e.g. \code{y ~ x}, also see the example. \code{x} may also be a character file path to the
    data to be used for plotting.}
  \item{residuals}{if set to \code{TRUE}, partial residuals may also be plotted if available.}
  \item{rug}{add a \code{\link[graphics]{rug}} to the plot.}
  \item{jitter}{if set to \code{TRUE} a \code{\link[base]{jitter}}ed 
      \code{\link[graphics]{rug}} plot is added.}
  \item{col.residuals}{the color of the partial residuals.}
  \item{col.lines}{the color of the lines.}
  \item{col.polygons}{specify the background color of polygons, if \code{x} has at least 3 columns, 
    i.e. column 2 and 3 can form one polygon.}
  \item{col.rug}{specify the color of the rug representation.}
  \item{c.select}{\code{integer} vector of maximum length of columns of \code{x}, selects the 
    columns of the resulting data matrix that should be used for plotting. E.g. if \code{x} has 5
    columns, then \code{c.select = c(1, 2, 5)} will select column 1, 2 and 5 for plotting. Note that
    first element of \code{c.select} should always be the column that holds the variable for the
    x-axis.}
  \item{fill.select}{\code{integer} vector, select pairwise the columns of the resulting data matrix 
    that should form one polygon with a certain background color specified in argument \code{col}. 
    E.g. \code{x} has three columns, or is specified with formula \code{f1 + f2 ~ x}, then setting 
    \code{fill.select = c(0, 1, 1)} will draw a polygon with \code{f1} and \code{f2} as boundaries. 
    If \code{x} has five columns or the formula is e.g. \code{f1 + f2 + f3 + f4 ~ x}, then setting
    \code{fill.select = c(0, 1, 1, 2, 2)}, the pairs \code{f1}, \code{f2} and \code{f3}, \code{f4}
    are selected to form two polygons.}
  \item{data}{if \code{x} is a formula, a \code{data.frame} or \code{list}. By default the variables 
    are taken from \code{environment(x)}: typically the environment from which \code{plot2d} is 
    called. Note that \code{data} may also be a character file path to the data.}
  \item{sep}{the field separator character when \code{x} or \code{data} is a character, see function
    \code{\link{read.table}}.}
  \item{month, year, step}{provide specific annotation for plotting estimation results for temporal
    variables. \code{month} and \code{year} define the minimum time point whereas step specifies the
    type of temporal data with \code{step = 4}, \code{step = 2} and \code{step = 1} corresponding to
    quartely, half yearly and yearly data.}
  \item{shift}{numeric. Constant to be added to the smooth before plotting.}
  \item{trans}{function to be applied to the smooth before plotting, e.g., to transform the
    plot to the response scale.}
  \item{\dots}{other graphical parameters, please see the details.}
}

\details{
  For 2D plots the following graphical parameters may be specified additionally: 
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
## regressor
dat <- data.frame(x = runif(n,-3,3))

##  response 
dat$y <- with(dat, 10 + sin(x) + rnorm(n,sd=0.6))

\dontrun{
## estimate model
b <- bayesx(y ~ sx(x), data = dat)
summary(b)

## plot estimated effect
plot(b, which = 1)
plot(b, which = 1, rug = FALSE)

## extract fitted effects
f <- fitted(b, term = "sx(x)")

## now use plot2d
plot2d(f)
plot2d(f, residuals = TRUE)
plot2d(f, residuals = TRUE, pch = 2, col.resid = "green3")
plot2d(f, col.poly = NA, lwd = 1, lty = 1)
plot2d(f, col.poly = NA, lwd = 1, lty = 1, col.lines = 4)
plot2d(f, col.poly = c(2, 3), lwd = 1, col.lines = 4, lty = 1)
plot2d(f, lwd = c(1, 3, 2, 2, 3), col.poly = NA, lty = 1)
plot2d(f, lwd = c(1, 3, 2, 2, 3), col.poly = NA, lty = 1, col.lines = 2:6)
plot2d(f, lwd = c(1, 3, 2, 2, 3), col.poly = NA, lty = 1, col.lines = 2:6,
  resid = TRUE, pch = 4, col.resid = 7)
}

## another variation
plot2d(sin(x) ~ x, data = dat)
dat$f <- with(dat, sin(dat$x))
plot2d(f ~ x, data = dat)
dat$f1 <- with(dat, f + 0.1)
dat$f2 <- with(dat, f - 0.1)
plot2d(f1 + f2 ~ x, data = dat)
plot2d(f1 + f2 ~ x, data = dat, fill.select = c(0, 1, 1), lty = 0)
plot2d(f1 + f2 ~ x, data = dat, fill.select = c(0, 1, 1), lty = 0,
  density = 20, poly.lty = 1, poly.lwd = 2)
plot2d(f1 + f + f2 ~ x, data = dat, fill.select = c(0, 1, 0, 1), 
  lty = c(0, 1, 0), density = 20, poly.lty = 1, poly.lwd = 2)
}

\keyword{hplot}
