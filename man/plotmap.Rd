\name{plotmap}
\alias{plotmap}
\alias{plot.bnd}
\alias{drawmap}

\title{
  Plot Maps
}

\description{
  The function takes a \code{\link[base]{list}} polygons and draws the corresponding map. Different 
  colors for each polygon can be used. Typically used for objects of class \code{"mrf.bayesx"} and
  \code{"random.bayesx"} returned from function \code{\link{bayesx}} and 
  \code{\link{read.bayesx.output}}.
}
 
\usage{
plotmap(map, x = NULL, id = NULL, c.select = NULL, legend = TRUE,
  missing = TRUE, swap = FALSE, range = NULL, names = FALSE,
  values = FALSE, col = NULL, ncol = 100, breaks = NULL,
  cex.legend = 1, cex.names = 1, cex.values = cex.names, digits = 2L,
  mar.min = 2, add = FALSE, interp = FALSE, grid = 200,
  land.only = FALSE, extrap = FALSE, outside = FALSE, type = "akima",
  linear = FALSE, k = 40, p.pch = 15, p.cex = 1, shift = NULL,
  trans = NULL, ...)
}

\arguments{
  \item{map}{the map to be plotted, the map object must be a \code{\link[base]{list}} of matrices 
    with first column indicating the \code{x} coordinate and second column the \code{y} coordinate
    each, also see \code{\link[graphics]{polygon}}.}
  \item{x}{a matrix or data frame with two columns, first column indicates the region and 
    second column the the values which will define the background colors of the polygons, e.g. 
    fitted values from \code{\link{bayesx}}. More columns are possible, e.g. quantiles, which can 
    accessed with argument \code{se}.}
  \item{id}{if argument \code{x} is a vector, argument \code{id} should contain a character vector 
    of the same length of \code{x} with entries indicating the polygon the \eqn{i}-th value of 
    \code{x} belongs to, i.e. \code{id} must contain the same names as polygon names in \code{map}.}
  \item{c.select}{select the column of the data in \code{x} which should be used for plotting, may
    be an integer or character with the corresponding column name.}
  \item{legend}{if set to \code{TRUE}, a legend will be shown.}
  \item{missing}{should polygons be plotted for which no data is available in \code{x}?}
  \item{swap}{if set to \code{TRUE}, colors will be represented in reverse order.}
  \item{range}{specify the range of values in \code{x} which should enter the plot, e.g. only 
    values between -2 and 2 are of interest then \code{range = c(-2, 2)}.}
  \item{names}{if set to \code{TRUE} the name for each polygon will also be plotted at the centroids
    of the corresponding polygons.}
  \item{values}{if set to \code{TRUE} the corresponding values for each polygon will also be plotted 
    at the centroids of the polygons.}
  \item{col}{the color of the surface, may also be a function, e.g. 
    \code{col.surface = heat.colors}.}
  \item{ncol}{the number of different colors that should be generated if \code{col} is a function.}
  \item{breaks}{a set of breakpoints for the colors: must give one more breakpoint than 
    \code{ncol}.}
  \item{cex.legend}{text size of the numbers in the legend.}
  \item{cex.names}{text size of the names if \code{names = TRUE}.}
  \item{cex.values}{text size of the names if \code{values = TRUE}.}
  \item{digits}{specifies the legend decimal places.}
  \item{mar.min}{Controls the definition of boundaries. Could be either \code{NULL} for individual 
    settings of \code{mar} or a value which defines \code{mar} as follows: The boundaries will be 
    calculated according to the height to width ratio of the map with minimal boundary 
    \code{mar.min}.}
  \item{add}{if set to \code{TRUE}, the map will be added to an existing plot.}
  \item{interp}{logical. Should the values provided in argument \code{x} be interpolated to obtain
    a smooth colored map.}
  \item{grid}{integer. Defines the number of grid cells to be used for interpolation.}
  \item{land.only}{if set to \code{TRUE}, only interpoltated pixels that cover land are drawn,
    see also function \code{\link[maps]{map.where}}.}
  \item{extrap}{logical. Should interpolations be computed outside the observation area
    (i.e., extrapolated)?}
  \item{outside}{logical. Should interpolated values outside the boundaries of the map be plotted.}
  \item{type}{character. Which type of interpolation metjod should be used. The default is
    \code{type = "akima"}, see function \code{\link[akima]{interp}}. The two other options are
    \code{type = "mba"}, which calls function \code{\link[MBA]{mba.surf}} of package \pkg{MBA}, or
    \code{type = "mgcv"}, which uses a spatial smoother withing package \pkg{mgcv} for interpolation.
    The last option is definitely the slowest, since a full regression model needs to be estimated.}
  \item{linear}{logical. Should linear interpolation be used withing function
     \code{\link[akima]{interp}}?}
  \item{k}{integer. The number of basis functions to be used to compute the interpolated surface
    when \code{type = "mgcv"}.}
  \item{p.pch}{numeric. The point size of the grid cells when using interpolation.}
  \item{p.cex}{numeric. The size of the grid cell points whein using interpolation.}
  \item{shift}{numeric. Constant to be added to the smooth before plotting.}
  \item{trans}{function to be applied to the smooth before plotting, e.g., to transform the
    plot to the response scale.}
  \item{\dots}{parameters to be passed to \code{\link{colorlegend}} and others, e.g. change the 
    \code{border} of the polygons and \code{density}, see \code{\link[graphics]{polygon}}. Please 
    see the exmaples.}
}

\note{
  Function \code{plotmap} uses per default the \pkg{akima} package to construct smooth interpolated
  surfaces, therefore, package \pkg{akima} needs to be installed. The \pkg{akima} package has an ACM
  license that restricts applications to non-commercial usage, see

  \url{http://www.acm.org/publications/policies/softwarecrnotice}
  
  Function \code{plotmap} prints a note refering to the ACM licence. This note can be supressed by
  setting

  \code{options("use.akima" = TRUE)} 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{plot.bayesx}}, \code{\link{read.bnd}}, \code{\link{colorlegend}}.
}

\examples{
## load a sample map
data("FantasyBnd")

## plot the map 
op <- par(no.readonly = TRUE)
plotmap(FantasyBnd, main = "Example of a plain map")
plotmap(FantasyBnd, lwd = 1, main = "Example of a plain map")
plotmap(FantasyBnd, lwd = 1, lty = 2)
plotmap(FantasyBnd, lwd = 1, lty = 2, border = "green3")
plotmap(FantasyBnd, lwd = 1, lty = 2, border = "green3",
  density = 50)
plotmap(FantasyBnd, lwd = 1, lty = 2, 
  border = c("red", "green3"),
  density = c(10, 20), angle = c(5, 45))
plotmap(FantasyBnd, lwd = 1, lty = 2, 
  border = c("red", "green3"),
  density = c(10, 20), angle = c(5, 45),
  col = c("blue", "yellow"))
plotmap(FantasyBnd, col = gray.colors(length(FantasyBnd)))

## add some values to the corresponding polygon areas
## note that the first column in matrix val contains
## the region identification index
x <- cbind(as.integer(names(FantasyBnd)), runif(length(FantasyBnd), -2, 2))
plotmap(FantasyBnd, x = x)
        
## now only plot values for some certain regions
set.seed(432)
samps <- sample(x[,1], 4)
nx <- x[samps,]
plotmap(FantasyBnd, x = nx, density = 20)

## play with legend
plotmap(FantasyBnd, x = x, names = TRUE, legend = FALSE)
plotmap(FantasyBnd, x = nx, density = 20, pos = c(0, 1))
plotmap(FantasyBnd, x = nx, density = 20, pos = c(0, 0.8),
  side.legend = 2)
plotmap(FantasyBnd, x = nx, density = 20, pos = c(0, 0.8),
  side.legend = 2, side.tick = 2)
plotmap(FantasyBnd, x = nx, density = 20, pos = c(0, 0.8),
  side.legend = 2, side.tick = 2, cex.legend = 0.5)
plotmap(FantasyBnd, x = x, values = TRUE, 
  pos = c(-0.15, -0.12))
plotmap(FantasyBnd, x = nx, values = TRUE, 
  pos = c(-0.07, -0.22), width = 2, 
  at = nx[,2], side.legend = 2, distance.labels = 3,
  density = 20)
plotmap(FantasyBnd, x = nx, values = TRUE, 
  pos = c(-0.07, -0.22), width = 2, 
  at = nx[,2], side.legend = 2, distance.labels = 3,
  density = 20, symmetric = FALSE, 
  col = heat_hcl, swap = TRUE)
plotmap(FantasyBnd, x = nx, values = TRUE, 
  pos = c(-0.07, -0.22), width = 2, 
  at = nx[,2], side.legend = 2, distance.labels = 3,
  density = 20, symmetric = FALSE, 
  col = heat_hcl, swap = TRUE, range = c(-5, 5))
plotmap(FantasyBnd, x = nx, values = TRUE, 
  pos = c(-0.07, -0.22), width = 2, 
  at = nx[,2], side.legend = 2, distance.labels = 3,
  density = 20, symmetric = FALSE, 
  col = heat_hcl, swap = TRUE, lrange = c(-5, 5))
plotmap(FantasyBnd, x = nx, values = TRUE, 
  pos = c(-0.07, -0.22), width = 2, 
  at = nx[,2], side.legend = 2, distance.labels = 3,
  density = 20, symmetric = FALSE, 
  col = heat_hcl, swap = TRUE, 
  ncol = 4, breaks = seq(-2, 2, length = 5))

## more position options
plotmap(FantasyBnd, x = nx, density = 20, pos = "bottomleft")
plotmap(FantasyBnd, x = nx, density = 20, pos = "topleft")
plotmap(FantasyBnd, x = nx, density = 20, pos = "topright")
plotmap(FantasyBnd, x = nx, density = 20, pos = "bottomright")
plotmap(FantasyBnd, x = nx, density = 20, pos = "right")
par(op)

# load and plot a map from GermanyBnd
op <- par(no.readonly = TRUE)
data("GermanyBnd")
plotmap(GermanyBnd, main = "Map of GermanyBnd")
n <- length(GermanyBnd)

# add some colors
plotmap(GermanyBnd, col = rainbow(n))
plotmap(GermanyBnd, col = heat.colors(n))
plotmap(GermanyBnd, col = topo.colors(n))
plotmap(GermanyBnd, col = cm.colors(n))
plotmap(GermanyBnd, col = gray.colors(n))
plotmap(GermanyBnd, col = c("green", "green3"))
par(op)


## now with bayesx
set.seed(333)

## simulate some geographical data
data("MunichBnd")
N <- length(MunichBnd); names(MunichBnd) <- 1:N
n <- N*5

## regressors
dat <- data.frame(id = rep(1:N, n/N))
dat$sp <- with(dat, sort(runif(N, -2, 2), decreasing = TRUE)[id])

## response
dat$y <- with(dat, 1.5 + sp + rnorm(n, sd = 0.6))

\dontrun{
## estimate model
b <- bayesx(y ~ sx(id, bs = "mrf", map = MunichBnd),
  method = "MCMC", data = dat)

## summary statistics
summary(b)

## plot spatial effect
op <- par(no.readonly = TRUE)
plot(b, map = MunichBnd)
plot(b, map = MunichBnd, c.select = "97.5%", symmetric = FALSE)
plot(b, map = MunichBnd, c.select = "2.5%", symmetric = FALSE)
plot(b, map = MunichBnd, c.select = "50%")
plot(b, map = MunichBnd, names = TRUE, 
  cex.names = 0.5, cex.legend = 0.8)
plot(b, map = MunichBnd, range = c(-0.5, 0.5))
plot(b, map = MunichBnd, range = c(-5, 5))
plot(b, map = MunichBnd, col = heat_hcl, 
  swap = TRUE, symmetric = FALSE)
par(op)
}
}

\keyword{hplot} 
