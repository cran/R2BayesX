\name{samples}
\alias{samples}

\title{
  Extract Samples of Coefficients and Variances  
}

\description{
  Function to extract the samples generated with Markov chain Monte Carlo simulation. 
}
 
\usage{
samples(object, model = NULL, term = NULL, coda = TRUE, acf = FALSE, ...)
}

\arguments{
  \item{object}{an object of class \code{"bayesx"}.}
  \item{model}{for which model the samples should be provided, either an integer or a character, 
    e.g. \code{model = "mcmc.model"}.}
  \item{term}{\code{\link{character}} or \code{\link{integer}}, the term for which samples should be
    extracted. Also samples of linear effects may be returned if available and
    \code{term = "linear-samples"}, or of the variance if \code{term = "var-samples"}. If set to
    \code{NULL}, the samples of the linear effects will be returned.}
  \item{acf}{if set to \code{TRUE}, the autocorrelation function of the samples will be provided.}
  \item{coda}{if set to \code{TRUE} the function will return objects of class \code{"mcmc"} or
    \code{"mcmc.list"} as provided in the \pkg{coda} package.}
  \item{\dots}{further arguments passed to function \code{\link[stats]{acf}}, e.g. argument 
    \code{lag.max} if \code{acf = TRUE}.}
}

\value{ 
  A \code{data.frame} or an object of class \code{"mcmc"} or \code{"mcmc.list"}, if argument
  \code{coda = TRUE}.
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
n <- 200

## regressor
dat <- data.frame(x = runif(n, -3, 3))

## response 
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x), data = dat)

## extract samples for the P-spline
sax <- samples(b, term = "sx(x)")
colnames(sax)

## plotting
plot(sax)

## linear effects samples
samples(b, term = "linear-samples")

## for acf, increase lag
sax <- samples(b, term = c("linear-samples", "var-samples", "sx(x)"),
  acf = TRUE, lag.max = 200, coda = FALSE)
names(sax)
head(sax)


## plot maximum autocorrelation 
## of all parameters
sax <- samples(b, term = c("linear-samples", "var-samples", "sx(x)"), 
  acf = TRUE, lag.max = 50, coda = FALSE)
names(sax)
matplot(y = apply(sax, 1, max), type = "h", 
  ylab = "ACF", xlab = "lag")

## example using multiple chains
b <- bayesx(y ~ sx(x), data = dat, chains = 3)
sax <- samples(b, term = "sx(x)")
plot(sax)
}
}

\keyword{regression}
