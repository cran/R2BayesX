\name{getscript}
\alias{getscript}

\title{
  Generate an executable R fitted model script  
}

\description{
  The function generates an executable \R script for obtaining summary statistics, visualization
  of model diagnostics and term effect plots of a fitted \code{\link{bayesx}} model object. 
}
 
\usage{
getscript(object, file = NULL, device = NULL, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{file}{optional, an output file the script is written to.}
  \item{device}{a graphical device function, e.g. \code{\link[grDevices]{pdf}}, see the examples
    and the help site of \code{\link[grDevices]{Devices}} for all available devices. If set, the 
    script will have extra calls to the specified devices that will generate graphics to the 
    specified \code{file}. If \code{file = NULL}, the working directory is taken.}
  \item{\dots}{arguments passed to \code{devices}, e.g. \code{height} and \code{width} of a graphical
    device.}
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{bayesx}}.
}

\examples{
\dontrun{
## generate some data
set.seed(111)
n <- 500

## regressor
dat <- data.frame(x = runif(n, -3, 3))

## response 
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x), data = dat)

## generate the R script
## and print it
script <- getscript(b)
script

## with a pdf device
script <- getscript(b, device = pdf, height = 5, width = 6)
script
}
}

\keyword{regression}
