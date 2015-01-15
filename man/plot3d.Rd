\name{plot3d}
\alias{plot3d}
\alias{plotsurf}

\title{
  3D Effect Plot 
}

\description{
  Function to plot 3D graphics or image and/or contour plots for bivariate effects/functions, 
  typically used for objects of class \code{"sm.bayesx"} and \code{"geo.bayesx"} returned from 
  function \code{\link{bayesx}} and \code{\link{read.bayesx.output}}.
}
 
\usage{
plot3d(x, residuals = FALSE, col.surface = NULL, 
  ncol = 99L, swap = FALSE, col.residuals = NULL, col.contour = NULL, 
  c.select = NULL, grid = 30L, image = FALSE, contour = FALSE, 
  legend = TRUE, cex.legend = 1, breaks = NULL, range = NULL, 
  digits = 2L, d.persp = 1L, r.persp = sqrt(3), outscale = 0,
  data = NULL, sep = "", shift = NULL, trans = NULL,
  type = "akima", linear = FALSE, extrap = FALSE,
  k = 40, ...)
}

\arguments{
  \item{x}{a matrix or data frame, containing the covariates for which the effect should be plotted 
    in the first and second column and at least a third column containing the effect, typically 
    the structure for bivariate functions returned within \code{\link{bayesx}} and 
    \code{\link{read.bayesx.output}} model term objects is used, also see 
    \code{\link{fitted.bayesx}}. Another possibility is to specify the plot via a \code{formula}, 
    e.g. for simple plotting of bivariate surfaces \code{z ~ x + y}, also see the example.
    \code{x} may also be a character file path to the data to be used for plotting.}
  \item{residuals}{if set to \code{TRUE}, partial residuals may also be plotted if available.}
  \item{col.surface}{the color of the surface, may also be a function, e.g. 
    \code{col.surface = heat.colors}.}
  \item{ncol}{the number of different colors that should be generated, if \code{col.surface} is a 
    function.}
  \item{swap}{if set to \code{TRUE} colors will be represented in reverse order.}
  \item{col.residuals}{the color of the partial residuals, or if \code{contour = TRUE} the color of the
    contour lines.}
  \item{col.contour}{the color of the contour lines.}
  \item{c.select}{\code{integer} vector of maximum length of columns of \code{x}, selects the 
    columns of the resulting data matrix that should be used for plotting. E.g. if \code{x} has 5
    columns, then \code{c.select = c(1, 2, 5)} will select column 1, 2 and 5 for plotting. 
    If \code{c.select = 95} or \code{c.select = 80}, function \code{plot3d} will search for the
    corresponding columns to plot a 95\eqn{\%} or 80\eqn{\%} confidence surfaces respectively. Note
    that if e.g. \code{c.select = c(1, 2)}, \code{plot3d} will use columns 1 + 2 and 2 + 2 for 
    plotting.}
  \item{grid}{the grid size of the surface(s).}
  \item{image}{if set to \code{TRUE}, an \code{\link[fields]{image.plot}} is drawn.}
  \item{contour}{if set to \code{TRUE}, a \code{\link[graphics]{contour}} plot is drawn.}
  \item{legend}{if \code{image = TRUE} an additional legend may be added to the plot.}
  \item{cex.legend}{the expansion factor for the legend text, see \code{\link[graphics]{text}}.}
  \item{breaks}{a set of breakpoints for the colors: must give one more breakpoint than 
    \code{ncol}.}
  \item{range}{specifies a certain range values should be plotted for.}
  \item{digits}{specifies the legend decimal places.}
  \item{d.persp}{see argument \code{d} in function \code{\link[graphics]{persp}}.}
  \item{r.persp}{see argument \code{r} in function \code{\link[graphics]{persp}}.}
  \item{outscale}{scales the outer ranges of \code{x} and \code{z} limits used for interpolation.}
  \item{data}{if \code{x} is a \code{formula}, a \code{data.frame} or \code{list}. By default the 
    variables are taken from \code{environment(x)}: typically the environment from which 
    \code{plot3d} is called. Note that \code{data} may also be a character file path to the data.}
  \item{sep}{the field separator character when \code{x} or \code{data} is a character, see function
    \code{\link{read.table}}.}
  \item{shift}{numeric. Constant to be added to the smooth before plotting.}
  \item{trans}{function to be applied to the smooth before plotting, e.g., to transform the
    plot to the response scale.}
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
  \item{\dots}{parameters passed to \code{\link{colorlegend}} if an image plot with legend is drawn,
    also other graphical parameters, please see the details.}
}

\details{
  For 3D plots the following graphical parameters may be specified additionally: 
  \itemize{
    \item \code{cex}: specify the size of partial residuals,
    \item \code{col}: it is possible to specify the color for the surfaces if \code{se > 0}, then 
      e.g. \code{col = c("green", "black", "red")},
    \item \code{pch}: the plotting character of the partial residuals,
    \item \code{\dots}: other graphical parameters passed functions \code{\link[graphics]{persp}},
      \code{\link[fields]{image.plot}} and \code{\link[graphics]{contour}}.
    }
}

\note{
  Function \code{plot3d} uses per default the \pkg{akima} package to construct smooth interpolated
  surfaces, therefore, package \pkg{akima} needs to be installed. The \pkg{akima} package has an ACM
  license that restricts applications to non-commercial usage, see

  \url{http://www.acm.org/publications/policies/softwarecrnotice}
  
  Function \code{plot3d} prints a note refering to the ACM licence. This note can be supressed by
  setting

  \code{options("use.akima" = TRUE)} 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plot.bayesx}}, \code{\link{bayesx}}, \code{\link{read.bayesx.output}},
  \code{\link{fitted.bayesx}}, \code{\link{colorlegend}}.
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
plot(b, term = "sx(z,w)")

## extract fitted effects
f <- fitted(b, term = "sx(z,w)")

## now use plot3d
plot3d(f)
plot3d(f, swap = TRUE)
plot3d(f, residuals = TRUE)
plot3d(f, resid = TRUE, cex.resid = 0.1)
plot3d(f, resid = TRUE, pch = 2, col.resid = "green3")
plot3d(f, resid = TRUE, c.select = 95, cex.resid = 0.1)
plot3d(f, resid = TRUE, c.select = 80, cex.resid = 0.1)
plot3d(f, grid = 100, border = NA)
plot3d(f, c.select = 95, border = c("red", NA, "green"),
  col.surface = c(1, NA, 1), resid = TRUE, cex.resid = 0.2)

## now some image and contour
plot3d(f, image = TRUE, legend = FALSE)
plot3d(f, image = TRUE, legend = TRUE)
plot3d(f, image = TRUE, contour = TRUE)
plot3d(f, image = TRUE, contour = TRUE, swap = TRUE)
plot3d(f, image = TRUE, contour = TRUE, col.contour = "white")
plot3d(f, contour = TRUE)
op <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))
plot3d(f, image = TRUE, contour = TRUE, c.select = 3)
plot3d(f, image = TRUE, contour = TRUE, c.select = "Estimate")
plot3d(f, image = TRUE, contour = TRUE, c.select = "97.5%")
par(op)
}

## another variation
dat$f1 <- with(dat, sin(z) * cos(w))
with(dat, plot3d(cbind(z, w, f1)))

## same with formula 
plot3d(sin(z) * cos(w) ~ z + w, zlab = "f(z,w)", data = dat)
plot3d(sin(z) * cos(w) ~ z + w, zlab = "f(z,w)", data = dat, 
  ticktype = "detailed")

## play with palettes
plot3d(sin(z) * cos(w) ~ z + w, col.surface = heat.colors, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = topo.colors, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = cm.colors, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = rainbow, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = terrain.colors, data = dat)

plot3d(sin(z) * cos(w) ~ z + w, col.surface = rainbow_hcl, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = diverge_hcl, data = dat)
plot3d(sin(z) * cos(w) ~ z + w, col.surface = sequential_hcl, data = dat)

plot3d(sin(z) * cos(w) ~ z + w, 
  col.surface = rainbow_hcl(n = 99, c = 300, l = 80, start = 0, end = 100), 
  data = dat)
plot3d(sin(z) * cos(w) ~ z + w, 
  col.surface = rainbow_hcl(n = 99, c = 300, l = 80, start = 0, end = 100), 
  image = TRUE, grid = 200, data = dat)
}

\keyword{hplot}
