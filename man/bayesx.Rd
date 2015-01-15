\name{bayesx}
\alias{bayesx}

\title{
  Estimate STAR Models with BayesX
}

\description{
  This is the documentation of the main model fitting function of the interface. Within function
  \code{bayesx}, three inferential concepts are available for estimation: Markov chain Monte Carlo 
  simulation (MCMC), estimation based on mixed model technology and restricted maximum likelihood 
  (REML), and a penalized least squares (respectively penalized likelihood) approach for 
  estimating models using model selection tools (STEP).
}

\usage{  
bayesx(formula, data, weights = NULL, subset = NULL, 
  offset = NULL, na.action = NULL, contrasts = NULL, 
  control = bayesx.control(\dots), model = TRUE,
  chains = NULL, cores = NULL, \dots)
}

\arguments{
  \item{formula}{symbolic description of the model (of type \code{y ~ x}), also see 
    \code{\link{sx}}, \code{\link[mgcv]{formula.gam}} and \code{\link[mgcv]{s}}.}
  \item{data}{a \code{\link{data.frame}} or \code{\link{list}} containing the model response 
    variable and covariates required by the formula. By default the variables are taken from 
    \code{environment(formula)}: typically the environment from which \code{bayesx} is called. 
    Argument \code{data} may also be a character string defining the directory the data is stored, 
    where the first row in the data set must contain the variable names and columns should be tab
    separated. Using this option will avoid loading the complete data into \R, only the \pkg{BayesX}
    output files will be imported, which might be helpful using large datasets.}
  \item{weights}{prior weights on the data.}
  \item{subset}{an optional vector specifying a subset of observations to be used in the fitting 
    process.}
  \item{offset}{can be used to supply a model offset for use in fitting.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}'s.
    The default is set by the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.omit}} if set to \code{NULL}.}
  \item{contrasts}{an optional list. See the \code{contrasts.arg} of 
    \code{\link[stats]{model.matrix.default}}.}
  \item{control}{specify several global control parameters for \code{bayesx}, see 
    \code{\link{bayesx.control}}.}
  \item{model}{a logical value indicating whether \code{\link[stats]{model.frame}} should be
    included as a component of the returned value.}
  \item{chains}{integer. The number of sequential chains that should be run, the default is one
    chain if \code{chains = NULL}. For each chain a separate seed for the random number generator is
    used. The return value of \code{bayesx} is a list of class \code{"bayesx"}, i.e. each list
    element represents a seperate model, for which the user can e.g. apply all plotting methods or
    extractor functions. Convergence diagnostics can then be computed using function
    \code{\link{GRstats}}.}
  \item{cores}{integer. How many cores should be used? The default is one core if
    \code{cores = NULL}. The return value is again a list of class \code{"bayesx"}, for which all
    plotting and extractor functions can be applied, see argument \code{chains}. Note that this
    option is not available on Windows systems, see the documentation of function
    \code{\link[parallel]{mclapply}}.}
  \item{\dots}{arguments passed to \code{\link{bayesx.control}}, e.g. \code{family} and 
    \code{method}, defaults are \code{family = "gaussian"}, \code{method = "MCMC"}.}
}

\details{  
  In \pkg{BayesX}, estimation of regression parameters is based on three inferential concepts: 

  \bold{Full Bayesian inference via MCMC}: 
  A fully Bayesian interpretation of structured additive regression models is obtained by specifying 
  prior distributions for all unknown parameters. Estimation can be facilitated using Markov chain 
  Monte Carlo simulation techniques. \pkg{BayesX} provides numerically efficient implementations of MCMC 
  schemes for structured additive regression models. Suitable proposal densities have been developed 
  to obtain rapidly mixing, well-behaved sampling schemes without the need for manual tuning. 

  \bold{Inference via a mixed model representation}:
  The other concept used for estimation is based on mixed model methodology. Within \pkg{BayesX}
  this concept has been extended to structured additive regression models and several types of 
  non-standard regression situations. The general idea is to take advantage of the close connection 
  between penalty concepts and corresponding random effects distributions. The smoothing parameters 
  of the penalties then transform to variance components in the random effects (mixed) model. While 
  the selection of smoothing parameters has been a difficult task for a long time, several 
  estimation procedures for variance components in mixed models are already available since the 
  1970's. The most popular one is restricted maximum likelihood in Gaussian mixed models with 
  marginal likelihood as the non-Gaussian counterpart. While regression coefficients are estimated 
  based on penalized likelihood, restricted maximum likelihood or marginal likelihood estimation 
  forms the basis for the determination of smoothing parameters. From a Bayesian perspective, this 
  yields empirical Bayes/posterior mode estimates for the structured additive regression models. 
  However, estimates can also merely be interpreted as penalized likelihood estimates from a 
  frequentist perspective. 

  \bold{Penalized likelihood including variable selection}:
  As a third alternative \pkg{BayesX} provides a penalized least squares (respectively penalized 
  likelihood) approach for estimating structured additive regression models. In addition, a powerful 
  variable and model selection tool is included. Model choice and estimation of the parameters is 
  done simultaneously. The algorithms are able to
  \itemize{
    \item decide whether a particular covariate enters the model,
    \item decide whether a continuous covariate enters the model linearly or nonlinearly,
    \item decide whether a spatial effect enters the model,
    \item decide whether a unit- or cluster specific heterogeneity effect enters the model
    \item select complex interaction effects (two dimensional surfaces, varying coefficient terms)
    \item select the degree of smoothness of nonlinear covariate, spatial or cluster specific
      heterogeneity effects.
  }
  Inference is based on penalized likelihood in combination with fast algorithms for selecting 
  relevant covariates and model terms. Different models are compared via various goodness of fit 
  criteria, e.g. AIC, BIC, GCV and 5 or 10 fold cross validation. 

  Within the model fitting function \code{bayesx}, the different inferential concepts may be chosen 
  by argument \code{method} of function \code{\link{bayesx.control}}. Options are \code{"MCMC"}, 
  \code{"REML"} and \code{"STEP"}.
  
  The wrapper function \code{bayesx} basically starts by setting up the necessary \pkg{BayesX}
  program file using function \code{\link{bayesx.construct}}, \code{\link{parse.bayesx.input}} and 
  \code{\link{write.bayesx.input}}. Afterwards the generated program file is send to the 
  command-line binary executable version of \pkg{BayesX} with \code{\link[BayesXsrc]{run.bayesx}}.
  As a last step, function \code{\link{read.bayesx.output}} will read the estimated model object
  returned from \pkg{BayesX} back into \R.
  
  For estimation of STAR models, function \code{bayesx} uses formula syntax as provided in package 
  \code{\link[mgcv]{mgcv}} (see \code{\link[mgcv]{formula.gam}}), i.e., models may be specified using
  the \code{\link{R2BayesX}} main model term constructor functions \code{\link{sx}} or the
  \code{\link[mgcv]{mgcv}} constructor functions \code{\link[mgcv]{s}}. For a detailed description
  of the model formula syntax used within \code{bayesx} models see also
  \code{\link{bayesx.construct}} and \code{\link{bayesx.term.options}}.

  After the \pkg{BayesX} binary has successfully finished processing an object of class \code{"bayesx"} is
  returned, wherefore a set of standard extractor functions and methods is available, including 
  methods to the generic functions \code{\link[base]{print}}, \code{\link[base]{summary}}, 
  \code{\link[graphics]{plot}}, \code{\link[stats]{residuals}} and \code{\link[stats]{fitted}}.
  
  See \code{\link{fitted.bayesx}}, \code{\link{plot.bayesx}}, and \code{\link{summary.bayesx}} for 
  more details on these methods.
}

\value{ 
  A list of class \code{"bayesx"}, see function \code{\link{read.bayesx.output}}.
}

\note{
  If a model is specified with a structured and an unstructured spatial effect, e.g. the model 
  formula is something like \code{y ~ sx(id, bs = "mrf", map = MapBnd) + sx(id, bs = "re")}, the
  model output contains of one additional total spatial effect, named with \code{"sx(id):total"}.
  Also see the last example.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\section{WARNINGS}{
  For geographical effects, note that \pkg{BayesX} may crash if the region identification covariate
  is a \code{\link{factor}}, it is recommended to code these variables as \code{\link{integer}},
  please see the example below.
}

\references{
  Belitz C, Brezger A, Kneib T, Lang S (2011). \pkg{BayesX} - Software for Bayesian Inference in 
    Structured Additive Regression Models. Version 2.0.1. 
    URL \url{http://www.BayesX.org}.

  Belitz C, Lang S (2008). Simultaneous selection of variables and smoothing parameters in 
    structured additive regression models. \emph{Computational Statistics & Data Analysis}, 
    \bold{53}, 61--81.

  Brezger A, Kneib T, Lang S (2005). \pkg{BayesX}: Analyzing Bayesian Structured Additive Regression 
    Models. \emph{Journal of Statistical Software}, \bold{14}(11), 1--22. 
    URL \url{http://www.jstatsoft.org/v14/i11/}.

  Brezger A, Lang S (2006). Generalized Structured Additive Regression Based on Bayesian P-Splines.
    \emph{Computational Statistics \& Data Analysis}, \bold{50}, 947--991.

  Fahrmeir L, Kneib T, Lang S (2004). Penalized Structured Additive Regression for Space Time Data: 
    A Bayesian Perspective. \emph{Statistica Sinica}, \bold{14}, 731--761.

  Umlauf N, Adler D, Kneib T, Lang S, Zeileis A (2015).
    Structured Additive Regression Models: An R Interface to BayesX.
    \emph{Journal of Statistical Software}, \bold{63}(21), 1--46.
    \url{http://www.jstatsoft.org/v63/i21/}
}

\seealso{
  \code{\link{parse.bayesx.input}}, \code{\link{write.bayesx.input}},
  \code{\link[BayesXsrc]{run.bayesx}}, \code{\link{read.bayesx.output}},
  \code{\link{summary.bayesx}}, \code{\link{plot.bayesx}},
  \code{\link{fitted.bayesx}}, \code{\link{bayesx.construct}}, \code{\link{bayesx.term.options}},
  \code{\link{sx}}, \code{\link[mgcv]{formula.gam}}, \code{\link[mgcv]{s}}.
}

\examples{
## generate some data
set.seed(111)
n <- 200

## regressor
dat <- data.frame(x = runif(n, -3, 3))

## response
dat$y <- with(dat, 1.5 + sin(x) + rnorm(n, sd = 0.6))

## estimate models with
## bayesx REML and MCMC
b1 <- bayesx(y ~ sx(x), method = "REML", data = dat)

## same using mgcv syntax
b1 <- bayesx(y ~ s(x, bs = "ps", k = 20), method = "REML", data = dat)

## now with MCMC
b2 <- bayesx(y ~ sx(x), method = "MCMC", 
  iter = 1200, burnin = 200, data = dat)

## compare reported output
summary(c(b1, b2))

## plot the effect for both models
plot(c(b1, b2), residuals = TRUE)

## use confint
confint(b1, level = 0.99)
confint(b2, level = 0.99)

\dontrun{
## more examples
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, -3, 3),
  w = runif(n, 0, 6), fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + sin(x) + cos(z) * sin(w) +
  c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac] + rnorm(n, sd = 0.6))

## estimate models with
## bayesx MCMC and REML
## and compare with
## mgcv gam()
b1 <- bayesx(y ~ sx(x) + sx(z, w, bs = "te") + fac,
  data = dat, method = "MCMC")
b2 <- bayesx(y ~ sx(x) + sx(z, w, bs = "te") + fac,
  data = dat, method = "REML")
b3 <- gam(y ~ s(x, bs = "ps") + te(z, w, bs = "ps") + fac, 
  data = dat)

## summary statistics
summary(b1)
summary(b2)
summary(b3)

## plot the effects
op <- par(no.readonly = TRUE)
par(mfrow = c(3, 2))
plot(b1, term = "sx(x)")
plot(b1, term = "sx(z,w)")
plot(b2, term = "sx(x)")
plot(b2, term = "sx(z,w)")
plot(b3, select = 1)
vis.gam(b3, c("z","w"), theta = 40, phi = 40)
par(op)

## combine models b1 and b2
b <- c(b1, b2)

## summary
summary(b)

## only plot effect 2 of both models
plot(b, term = "sx(z,w)") 

## with residuals
plot(b, term = "sx(z,w)", residuals = TRUE) 

## same model with kriging
b <- bayesx(y ~ sx(x) + sx(z, w, bs = "kr") + fac, 
  method = "REML", data = dat)
plot(b)


## now a mrf example
## note: the regional identification
## covariate and the map regionnames
## should be coded as integer
set.seed(333)
     
## simulate some geographical data
data("MunichBnd")
N <- length(MunichBnd); n <- N*5
     
## regressors
dat <- data.frame(x1 = runif(n, -3, 3),
  id = as.factor(rep(names(MunichBnd), length.out = n)))
dat$sp <- with(dat, sort(runif(N, -2, 2), decreasing = TRUE)[id])
     
## response
dat$y <- with(dat, 1.5 + sin(x1) + sp + rnorm(n, sd = 1.2))

## estimate models with
## bayesx MCMC and REML
b1 <- bayesx(y ~ sx(x1) + sx(id, bs = "mrf", map = MunichBnd), 
  method = "MCMC", data = dat)
b2 <- bayesx(y ~ sx(x1) + sx(id, bs = "mrf", map = MunichBnd), 
  method = "REML", data = dat)

## summary statistics
summary(b1)
summary(b2)

## plot the spatial effects
plot(b1, term = "sx(id)", map = MunichBnd, 
  main = "bayesx() MCMC estimate")
plot(b2, term = "sx(id)", map = MunichBnd, 
  main = "bayesx() REML estimate")
plotmap(MunichBnd, x = dat$sp, id = dat$id, 
  main = "Truth")

## try geosplines instead
b <- bayesx(y ~ sx(id, bs = "gs", map = MunichBnd) + sx(x1), data = dat)
summary(b)
plot(b, term = "sx(id)", map = MunichBnd)

## geokriging
b <- bayesx(y ~ sx(id, bs = "gk", map = MunichBnd) + sx(x1), 
  method = "REML", data = dat)
summary(b)
plot(b, term = "sx(id)", map = MunichBnd)

## perspective plot of the effect
plot(b, term = "sx(id)")

## image and contour plot 
plot(b, term = "sx(id)", image = TRUE, 
  contour = TRUE, grid = 200)


## model with random effects
set.seed(333)
N <- 30
n <- N*10

## regressors
dat <- data.frame(id = sort(rep(1:N, n/N)), x1 = runif(n, -3, 3))
dat$re <- with(dat, rnorm(N, sd = 0.6)[id])

## response
dat$y <- with(dat, 1.5 + sin(x1) + re + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x1) + sx(id, bs = "re"), data = dat)
summary(b)
plot(b)

## extract estimated random effects
## and compare with true effects
plot(fitted(b, term = "sx(id)")$Mean ~ unique(dat$re))


## now a spatial example
## with structured and
## unstructered spatial 
## effect
set.seed(333)

## simulate some geographical data
data("MunichBnd")
N <- length(MunichBnd); names(MunichBnd) <- 1:N
n <- N*5

## regressors
dat <- data.frame(id = rep(1:N, n/N), x1 = runif(n, -3, 3))
dat$sp <- with(dat, sort(runif(N, -2, 2), decreasing = TRUE)[id])
dat$re <- with(dat, rnorm(N, sd = 0.6)[id])

## response
dat$y <- with(dat, 1.5 + sin(x1) + sp + re + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x1) + 
  sx(id, bs = "mrf", map = MunichBnd) +
  sx(id, bs = "re"), method = "MCMC", data = dat)
summary(b)

## plot all spatial effects
plot(b, term = "sx(id):mrf", map = MunichBnd, 
  main = "Structured spatial effect")
plot(b, term = "sx(id):re", map = MunichBnd, 
  main = "Unstructured spatial effect")
plot(b, term = "sx(id):total", map = MunichBnd, 
  main = "Total spatial effect", digits = 4)


## some experiments with the
## stepwise algorithm
## generate some data
set.seed(321)
n <- 1000

## regressors
dat <- data.frame(x1 = runif(n, -3, 3), x2 = runif(n),
  x3 = runif(n, 3, 6), x4 = runif(n, 0, 1))

## response
dat$y <- with(dat, 1.5 + sin(x1) + 0.6 * x2 + rnorm(n, sd = 0.6))

## estimate model with STEP
b <- bayesx(y ~ sx(x1) + sx(x2) + sx(x3) + sx(x4), 
  method = "STEP", algorithm = "cdescent1", CI = "MCMCselect", 
  iter = 10000, step = 10, data = dat)
summary(b)
plot(b)


## a probit example
set.seed(111)
n <- 1000
dat <- data.frame(x <- runif(n, -3, 3))

dat$z <- with(dat, sin(x) + rnorm(n))
dat$y <- rep(0, n)
dat$y[dat$z > 0] <- 1

b <- bayesx(y ~ sx(x), family = "binomialprobit", data = dat)
summary(b)
plot(b)


## estimate varying coefficient models
set.seed(333)
n <- 1000
dat <- data.frame(x = runif(n, -3, 3), id = factor(rep(1:4, n/4)))

## response
dat$y <- with(dat, 1.5 + sin(x) * c(-1, 0.2, 1, 5)[id] + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x, by = id, center = TRUE),
  method = "REML", data = dat)
summary(b)
plot(b, resid = TRUE, cex.resid = 0.1)
}
}

\keyword{regression}
