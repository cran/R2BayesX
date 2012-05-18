\name{GAMart}
\alias{GAMart}
\alias{gamart}

\title{GAM Artificial Data Set}

\description{
  This is an artificial data set mainly used to test the \pkg{R2BayesX} interfacing functions. The
  data includes three different types of response variables. One numeric, one binomial and a
  categorical response with 4 different levels. In addition, several numeric and factor covariates
  are provided. The data set is constructed such that the observations are based upon different
  locations (pixels in `longitude' and `latitude' coordinates) obtained from a regular grid.
}

\usage{data("GAMart")}

\format{
A data frame containing 500 observations on 12 variables.
\describe{
  \item{num:}{numeric, response variable.}
  \item{bin:}{factor, binomial response variable with levels \code{"no"} and \code{"yes"}.}
  \item{cat:}{factor, multi categorical response with levels \code{"none"}, \code{"low"},
    \code{"medium"} and \code{"high"}.}
  \item{x1:}{numeric covariate.}
  \item{x2:}{numeric covariate.}
  \item{x3:}{numeric covariate.}
  \item{fac:}{factor covariate with levels \code{"low"}, \code{"medium"} and \code{"high"}.}
  \item{id:}{factor, pixel identification index.}
  \item{long:}{numeric, the longitude coordinate of the pixel.}
  \item{lat:}{numeric, the latitude coordinate of the pixel.}
}
}

\seealso{\code{\link{bayesx}}}

\examples{
\dontrun{
data("GAMart")

## normal response
b <- bayesx(num ~ fac + sx(x1) + sx(x2) + sx(x3) +
  sx(long, lat, bs = "te") + sx(id, bs = "re"),
  data = GAMart)
summary(b)
plot(b)

## binomial response
b <- bayesx(bin ~ fac + sx(x1) + sx(x2) + sx(x3) +
  sx(long, lat, bs = "te") + sx(id, bs = "re"),
  data = GAMart, family = "binomial", method = "REML")
summary(b)
plot(b)

## categorical response
b <- bayesx(cat ~ fac + sx(x1) + sx(x2) + sx(x3) +
  sx(long, lat, bs = "te") + sx(id, bs = "re"),
  data = GAMart, family = "cumprobit", method = "REML")
summary(b)
plot(b)
}
}

\keyword{datasets}
