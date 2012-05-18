\name{colorlegend}
\alias{colorlegend}

\title{
  Plot a Color Legend
}

\description{
  Function to generate a color legend, the legend may be added to an existing plot or drawn in a
  separate plotting window.
}

\usage{
colorlegend (color = NULL, ncol = NULL, x = NULL, 
  breaks = NULL, pos = NULL, side.legend = 1L, 
  side.ticks = 1L, range = NULL, lrange = NULL, 
  width = 1L, height = 1L, scale = TRUE, xlim = NULL, 
  ylim = NULL, plot = NULL, full = FALSE, add = FALSE, 
  col.border = "black", lty.border = 1L, lwd.border = 1L, 
  ticks = TRUE, at = NULL, col.ticks = "black", lwd.ticks = 1L, 
  lty.ticks = 1L, length.ticks = 1L, labels = NULL, 
  distance.labels = 1L, col.labels = "black", cex.labels = 1L, 
  digits = 2L, swap = FALSE, symmetric = TRUE, xpd = NULL, ...)
}

\arguments{
  \item{color}{the colors for the legend, may also be a function, e.g. \code{colors = heat.colors}.}
  \item{ncol}{the number of different colors that should be generated if \code{color} is a 
    function.}
  \item{x}{values for which the color legend should be drawn.}
  \item{breaks}{a set of breakpoints for the colors: must give one more breakpoint than 
    \code{ncol}.}
  \item{pos}{the position of the legend, either a numeric vector, e.g. \code{pos = c(0.1, 0.2)} will 
    add the legend at the 10\eqn{\%} point in the x-direction and at the 20\eqn{\%} point in the 
    y-direction of the plotting window, may also be negative, or one of the following: 
    \code{"bottomleft"}, \code{"topleft"}, \code{"topright"} or \code{"bottomright"}.}
  \item{side.legend}{if set to \code{2} the legend will be flipped by 90 degrees.}
  \item{side.ticks}{if set to \code{2}, the ticks and labels will be on the opposite site of the 
    legend.}
  \item{range}{specifies a range for \code{x} values for which the legend should be drawn.}
  \item{lrange}{specifies the range of legend.}
  \item{width}{the width of the legend, if \code{scale = TRUE} the width is proportional to the
    x-limits of the plotting window.}
  \item{height}{the height of the legend, if \code{scale = TRUE} the height is proportional to the
    y-limits of the plotting window.}
  \item{scale}{if set to \code{TRUE}, the \code{width} and \code{height} of the legend will be
    calculated proportional to the x- and y-limits of the plotting window.}
  \item{xlim}{the x-limits of the plotting window the legend should be added for, numeric vector 
    e.g. returned from function \code{\link[base]{range}}.}
  \item{ylim}{the y-limits of the plotting window the legend should be added for, numeric vector 
    e.g. returned from function \code{\link[base]{range}}.}
  \item{plot}{if set to \code{TRUE}, the legend will be drawn in a separate plotting window.}
  \item{full}{if set to \code{TRUE}, the legend will be drawn using the full window range.}
  \item{add}{if set to \code{TRUE}, the legend will be added to an existing plot.}
  \item{col.border}{the color of the surrounding border line of the legend.}
  \item{lty.border}{the line type of the surrounding border line of the legend.}
  \item{lwd.border}{the line width of the surrounding border line of the legend.}
  \item{ticks}{if set to \code{TRUE}, ticks will be added to the legend.}
  \item{at}{specifies at which locations ticks and labels should be added.}
  \item{col.ticks}{the colors of the ticks.}
  \item{lwd.ticks}{the line width of the ticks.}
  \item{lty.ticks}{the line type of the ticks.}
  \item{length.ticks}{the length of the ticks.}
  \item{labels}{specifies labels that should be added to the ticks.}
  \item{distance.labels}{the distance of the labels to the ticks, proportional to the length of
    the ticks.}
  \item{col.labels}{the colors of the labels.}
  \item{cex.labels}{text size of the labels.}
  \item{digits}{the decimal places if labels are numerical.}
  \item{swap}{if set to \code{TRUE} colors will be represented in reverse order.}
  \item{symmetric}{if set to \code{TRUE}, a symmetric legend will be drawn corresponding to the
    \code{+- max(abs(x))} value.}
  \item{xpd}{sets the \code{xpd} parameter in function \code{\link[graphics]{par}}.}
  \item{\dots}{other graphical parameters to be passed to function \code{\link[graphics]{text}}.}
}

\value{
  A named \code{list} with the colors generated, the breaks and the function \code{map}, which may 
  be used for mapping of \code{x} values to the colors specified in argument \code{colors}, please 
  see the examples below.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\examples{
## play with colorlegend
colorlegend()
colorlegend(side.legend = 2)
colorlegend(side.legend = 2, side.ticks = 2)
colorlegend(height = 2)
colorlegend(width = 1, height = 0.8, scale = FALSE, 
  pos = c(0, 0.2), length.ticks = 0.5)
colorlegend(color = heat.colors, ncol = 9)
colorlegend(color = heat.colors, ncol = 9, swap = TRUE)
colorlegend(pos = "bottomleft", xpd = TRUE)
colorlegend(pos = "topleft", xpd = TRUE)
colorlegend(pos = "topright", xpd = TRUE)
colorlegend(pos = "bottomright", xpd = TRUE)


## take x values for the color legend
x <- runif(100, -2, 2)
colorlegend(color = diverge_hcl, x = x)
colorlegend(color = diverge_hcl, x = x, at = c(-1.5, 0, 1.5))
colorlegend(color = diverge_hcl, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"))
colorlegend(color = rainbow_hcl, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5)
colorlegend(color = heat_hcl, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5,
  lwd.border = 2, lwd.ticks = 2, cex.labels = 1.5, font = 2)
colorlegend(color = topo.colors, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5,
  lwd.border = 2, lwd.ticks = 2, cex.labels = 1.5, font = 2,
  col.border = "green3", col.ticks = c(2, 5, 2), 
  col.labels = c(6, 4, 3))
colorlegend(color = diverge_hsv, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5,
  lwd.border = 2, lwd.ticks = 2, cex.labels = 1.5, font = 2,
  col.border = "green3", col.ticks = c(2, 5, 2), 
  col.labels = c(6, 4, 3), lty.border = 2, lty.ticks = c(2, 3, 2))
colorlegend(color = diverge_hsv, x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5,
  lwd.border = 2, lwd.ticks = 2, cex.labels = 1.5, font = 2,
  col.border = "green3", col.ticks = c(2, 5, 2), 
  col.labels = c(6, 4, 3), lty.border = 2, lty.ticks = c(2, 3, 2),
  ncol = 3)
colorlegend(color = c("red", "white", "red"), x = x, at = c(-1.5, 0, 1.5),
  labels = c("low", "middle", "high"), length.ticks = 1.5,
  lwd.border = 2, lwd.ticks = 2, cex.labels = 1.5, font = 2,
  col.border = "green3", col.ticks = c(2, 5, 2), 
  col.labels = c(6, 4, 3), lty.border = 2, lty.ticks = c(2, 3, 2),
  ncol = 3, breaks = c(-2, -1, 1, 2))
colorlegend(color = diverge_hcl, x = x, range = c(-3, 3))
colorlegend(color = diverge_hcl, x = x, range = c(-3, 3), lrange = c(-6, 6))


## combine plot with color legend
n <- 100
x <- y <- seq(-3, 3, length.out = n)
z <- outer(sin(x), cos(x)) 
pal <- colorlegend(color = diverge_hcl, x = z, plot = FALSE)
op <- par(no.readonly = TRUE)
par(mar = c(5.1, 4.1, 4.1, 6.1))
image(x = x, y = y, z = z, col = pal$colors, breaks = pal$breaks)
colorlegend(color = diverge_hcl, x = z, plot = FALSE, add = TRUE,
  xlim = range(x), ylim = range(z), scale = FALSE, 
  side.legend = 2, side.ticks = 2, pos = c(1.05, -1),
  width = diff(range(y)), height = 0.1 * diff(range(x)), 
  length.ticks = 3, at = seq(min(z), max(z), length.out = 5),
  xpd = TRUE)
par(op)


## another example with different plot
n <- 50
x <- sin(seq(-3, 3, length.out = n)) 
pal <- colorlegend(color = diverge_hcl, x = x, plot = FALSE)
op <- par(no.readonly = TRUE)
barplot(x, border = "transparent", col = pal$map(x))
colorlegend(color = diverge_hcl, x = x, plot = FALSE, add = TRUE,
  xlim = c(0, 60), ylim = c(-1, 1), pos = c(0, -0.15), xpd = TRUE,
  scale = FALSE, width = 60, height = 0.15, length.ticks = 3,
  at = seq(min(x), max(x), length.out = 9))
par(op)
}

\keyword{regression}
