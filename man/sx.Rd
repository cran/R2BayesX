\name{sx}
\alias{sx}

\title{
  Construct BayesX Model Terms in A Formula
}

\description{
  Function \code{sx} is a model term constructor function for terms used within the \code{formula}
  argument of function \code{\link{bayesx}}. The function does not evaluate matrices etc., the 
  behavior is similar to function \code{\link[mgcv]{s}} from package \code{\link[mgcv]{mgcv}}. It 
  purely exists to build a basic setup for the model term which can be processed by function 
  \code{\link{bayesx.construct}}.
}

\usage{
sx(x, z = NULL, bs = "ps", by = NA, ...)
}
 
\arguments{
  \item{x}{the covariate the model term is a function of.}
  \item{z}{a second covariate.}
  \item{bs}{a \code{\link{character}} string, specifying the basis/type which is used for this model 
    term.}
  \item{by}{a \code{\link{numeric}} or \code{\link{factor}} variable of the same dimension as each 
    covariate. In the \code{numeric} vector case the elements multiply the smooth, evaluated at the 
    corresponding covariate values (a `varying coefficient model' results). In the \code{factor} 
    case the term is replicated for each \code{factor} level. Note that centering of the term may be 
    needed, please see the notes.}
  \item{\dots}{special controlling arguments or objects used for the model term, see also 
    the examples and function \code{\link{bayesx.term.options}} for all possible optional 
    parameters.}
}

\details{
  The following term types may be specified using argument \code{bs}:
  \itemize{
    \item \code{"rw1"}, \code{"rw2"}: Zero degree P-splines: Defines a zero degree P-spline with first or
                            second order difference penalty. A zero degree P-spline typically
                            estimates for every distinct covariate value in the dataset a separate
                            parameter. Usually there is no reason to prefer zero degree P-splines
                            over higher order P-splines. An exception are ordinal covariates or
                            continuous covariates with only a small number of different values.
                            For ordinal covariates higher order P-splines are not meaningful while
                            zero degree P-splines might be an alternative to modeling nonlinear
                            relationships via a dummy approach with completely unrestricted
                            regression parameters.
    \item \code{"season"}: Seasonal effect of a time scale. 
    \item \code{"ps"}, \code{"psplinerw1"}, \code{"psplinerw2"}: P-spline with first or second order 
                                                        difference penalty. 
    \item \code{"te"}, \code{"pspline2dimrw1"}: Defines a two-dimensional P-spline based on the tensor
              product of one-dimensional P-splines with a two-dimensional first order random walk
              penalty for the parameters of the spline. 
    \item \code{"kr"}, \code{"kriging"}: Kriging with stationary Gaussian random fields.  
    \item \code{"gk"}, \code{"geokriging"}: Geokriging with stationary Gaussian random fields: Estimation
              is based on the centroids of a map object provided in
              boundary format (see function \code{\link{read.bnd}} and \code{\link{shp2bnd}}) as an additional
              argument named \code{map} within function \code{\link{sx}}, or supplied within argument
              \code{xt} when using function \code{\link[mgcv]{s}}, e.g., \code{xt = list(map = MapBnd)}. 
    \item \code{"gs"}, \code{"geospline"}: Geosplines based on two-dimensional P-splines with a
              two-dimensional first order random walk penalty for the parameters of the spline.	
	            Estimation is based on the coordinates of the centroids of the regions
              of a map object provided in boundary format (see function \code{\link{read.bnd}} and
              \code{\link{shp2bnd}}) as an additional argument named \code{map} (see above). 
    \item \code{"mrf"}, \code{"spatial"}: Markov random fields: Defines a Markov random field prior for a
               spatial covariate, where geographical information is provided by a map object in
               boundary or graph file format (see function \code{\link{read.bnd}}, \code{\link{read.gra}} and
               \code{\link{shp2bnd}}), as an additional argument named \code{map} (see above).
    \item \code{"bl"}, \code{"baseline"}: Nonlinear baseline effect in hazard regression or multi-state
              models: Defines a P-spline with second order random walk penalty for the parameters of
              the spline for the log-baseline effect \eqn{log(\lambda(time))}. 
    \item \code{"factor"}: Special \pkg{BayesX} specifier for factors, especially meaningful if
                  \code{method = "STEP"}, since the factor term is then treated as a full term,
                  which is either included or removed from the model. 
    \item \code{"ridge"}, \code{"lasso"}, \code{"nigmix"}: Shrinkage of fixed effects: defines a
                                                shrinkage-prior for the corresponding parameters
                                                \eqn{\gamma_j}, \eqn{j = 1, \ldots, q}, \eqn{q \geq 1} of the
                                                linear effects \eqn{x_1, \ldots, x_q}. There are three
                                                priors possible: ridge-, lasso- and Normal Mixture
                                                of inverse Gamma prior.
    \item \code{"re"}: Gaussian i.i.d. Random effects of a unit or cluster identification covariate.
  }
}

\note{
  Some care has to be taken with the identifiability of varying coefficients terms. The standard in
  \pkg{BayesX} is to center nonlinear main effects terms around zero whereas varying coefficient terms are
  not centered. This makes sense since main effects nonlinear terms are not identifiable and varying
  coefficients terms are usually identifiable. However, there are situations where a varying
  coefficients term is not identifiable. Then the term must be centered. Since centering is not
  automatically accomplished it has to be enforced by the user by adding option
  \code{center = TRUE} in function \code{f}. To give an example, the varying coefficient terms in 
  \eqn{\eta = \ldots + g_1(z_1)z + g_2(z_2)z + \gamma_0 + \gamma_1 z + \ldots} are not
  identified, whereas in \eqn{\eta = \ldots + g_1(z_1)z + \gamma_0 + \ldots}, the varying 
  coefficient term is identifiable. In the first case, centering is necessary, in the second case, 
  it is not.
}

\value{
  A \code{list} of class \code{"xx.smooth.spec"}, where \code{"xx"} is a basis/type identifying code
  given by the \code{bs} argument of \code{f}. 
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{bayesx}}, \code{\link{bayesx.term.options}}, \code{\link[mgcv]{s}},
  \code{\link{bayesx.construct}}.
}

\examples{
## funktion sx() returns a list
## which is then processed by function
## bayesx.construct to build the
## BayesX model term structure
sx(x)

bayesx.construct(sx(x))
bayesx.construct(sx(x, bs = "rw1"))
bayesx.construct(sx(x, bs = "factor"))
bayesx.construct(sx(x, bs = "offset"))
bayesx.construct(sx(x, z, bs = "te"))

## varying coefficients
bayesx.construct(sx(x1, by = x2))
bayesx.construct(sx(x1, by = x2, center = TRUE))

## using a map for markov random fields
data("FantasyBnd")
plot(FantasyBnd)
bayesx.construct(sx(id, bs = "mrf", map = FantasyBnd))

## random effects
bayesx.construct(sx(id, bs = "re"))

## examples using optional controlling
## parameters and objects
bayesx.construct(sx(x, bs = "ps", knots = 20))
bayesx.construct(sx(x, bs = "ps", nrknots = 20))
bayesx.construct(sx(x, bs = "ps", knots = 20, nocenter = TRUE))

## use of bs with original 
## BayesX syntax 
bayesx.construct(sx(x, bs = "psplinerw1"))
bayesx.construct(sx(x, bs = "psplinerw2"))
bayesx.construct(sx(x, z, bs = "pspline2dimrw2"))

bayesx.construct(sx(id, bs = "spatial", map = FantasyBnd))
bayesx.construct(sx(x, z, bs = "kriging"))
bayesx.construct(sx(id, bs = "geospline", map = FantasyBnd, nrknots = 5))
bayesx.construct(sx(x, bs = "catspecific"))


\dontrun{
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

## increase inner knots
## decrease degree of the P-spline
b2 <- bayesx(y ~ sx(x, knots = 30, degree = 2), method = "REML", data = dat)


## compare reported output
summary(c(b1, b2))

## plot the effect for both models
plot(c(b1, b2), residuals = TRUE)


## more examples
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, -3, 3),
  w = runif(n, 0, 6), fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + sin(x) + cos(z) * sin(w) +
  c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac] + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x) + sx(z, w, bs = "te") + fac,
  data = dat, method = "MCMC")

summary(b)
plot(b)


## now a mrf example
## note: the regional identification
## covariate and the map regionnames
## should be coded as integer
set.seed(333)
     
## simulate some geographical data
data("MunichBnd")
N <- length(MunichBnd); n <- N*5
names(MunichBnd) <- 1:N
     
## regressors
dat <- data.frame(x1 = runif(n, -3, 3),
  id = as.factor(rep(names(MunichBnd), length.out = n)))
dat$sp <- with(dat, sort(runif(N, -2, 2), decreasing = TRUE)[id])
     
## response
dat$y <- with(dat, 1.5 + sin(x1) + sp + rnorm(n, sd = 1.2))

## estimate models with
## bayesx MCMC and REML
b <- bayesx(y ~ sx(x1) + sx(id, bs = "mrf", map = MunichBnd), 
  method = "REML", data = dat)

## summary statistics
summary(b)

## plot the effects
op <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
plot(b, term = "sx(id)", map = MunichBnd, 
  main = "bayesx() estimate")
plotmap(MunichBnd, x = dat$sp, id = dat$id, 
  main = "Truth")
par(op)


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
b <- bayesx(y ~ sx(x1, bs = "psplinerw1") + sx(id, bs = "re"), data = dat)
summary(b)
plot(b)

## extract estimated random effects
## and compare with true effects
plot(fitted(b, term = "sx(id)")$Mean ~ unique(dat$re))
}
}

\keyword{regression} 
