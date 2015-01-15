\name{bayesx.control}
\alias{bayesx.control}

\title{
  Control Parameters for BayesX
}

\description{
  Various parameters that control fitting of regression models
  using \code{\link{bayesx}}.
}

\usage{
bayesx.control(model.name = "bayesx.estim", 
  family = "gaussian", method = "MCMC", verbose = FALSE, 
  dir.rm = TRUE, outfile = NULL, replace = FALSE, iterations = 12000L,
  burnin = 2000L, maxint = NULL, step = 10L, predict = TRUE,
  seed = NULL, hyp.prior = NULL, distopt = NULL, reference = NULL,
  zipdistopt = NULL, begin = NULL, level = NULL, eps = 1e-05,
  lowerlim = 0.001, maxit = 400L, maxchange = 1e+06, leftint = NULL,
  lefttrunc = NULL, state = NULL, algorithm = NULL, criterion = NULL, 
  proportion = NULL, startmodel = NULL, trace = NULL, 
  steps = NULL, CI = NULL, bootstrapsamples = NULL, \dots)
}

\arguments{
  \item{model.name}{character, specify a base name model output files are named 
    with in \code{outfile}.}
  \item{family}{character, specify the distribution used for the model, options 
    for all methods, \code{"MCMC"}, \code{"REML"} and \code{"STEP"} are: \code{"binomial"},
    \code{"binomialprobit"}, \code{"gamma"}, \code{"gaussian"}, \code{"multinomial"},
    \code{"poisson"}. For \code{"MCMC"} and \code{"REML"} only: \code{"cox"}, \code{"cumprobit"} and
    \code{"multistate"}. For \code{"REML"} only use:
    \code{"binomialcomploglog"}, \code{"cumlogit"}, \code{"multinomialcatsp"},
    \code{"multinomialprobit"}, \code{"seqlogit"}, \code{"seqprobit"}.}
  \item{method}{character, which method should be used for estimation, options 
    are \code{"MCMC"}, \code{"HMCMC"} (hierarchical MCMC), \code{"REML"} and \code{"STEP"}.}
  \item{verbose}{logical, should output be printed to the \code{R} console 
    during runtime of \code{\link{bayesx}}.}
  \item{dir.rm}{logical, should the the \code{output} files and directory 
    removed after estimation?}
  \item{outfile}{character, specify a directory where \code{\link{bayesx}} 
    should store all output files, all output files will be named with \code{model.name} as the 
    base name.}
  \item{replace}{if set to \code{TRUE}, the files in the output directory specified in argument
    \code{outfile} will be replaced.}
  \item{iterations}{integer, sets the number of iterations for the sampler.}
  \item{burnin}{integer, sets the burn-in period of the sampler.}
  \item{maxint}{integer, if first or second order random walk priors are 
    specified, in some cases the data will be slightly grouped: The range between the minimal and 
    maximal observed covariate values will be divided into (small) intervals, and for each interval 
    one parameter will be estimated. The grouping has almost no effect on estimation results as long 
    as the number of intervals is large enough. With the \code{maxint} option the amount of grouping 
    can be determined by the user. integer is the maximum number of intervals allowed. for 
    equidistant data, the default \code{maxint = 150} for example, means that no grouping will be 
    done as long as the number of different observations is equal to or below 150. for non 
    equidistant data some grouping may be done even if the number of different observations is below 
    150.}
  \item{step}{integer, defines the thinning parameter for MCMC simulation.  
    E.g., \code{step = 50} means, that only every 50th sampled parameter will be stored and used to 
    compute characteristics of the posterior distribution as means, standard deviations or 
    quantiles. The aim of thinning is to reach a considerable reduction of disk storing and 
    autocorrelations between sampled parameters.}
  \item{predict}{logical, option \code{predict} may be specified to compute 
    samples of the deviance \code{D}, the effective number of parameters \code{pD} and the deviance 
    information criterion \code{DIC} of the model. In addition, if \code{predict = FALSE}, only
    output files of estimated effects will be returned, otherwise an expanded dataset using all
    observations would be written in the output directory, also containing the data used for
    estimation. Hence, this option is useful when dealing with large data sets, that might cause
    memory problems if \code{predict} is set to \code{TRUE}.}
  \item{seed}{integer, set the seed of the random number generator in 
    \pkg{BayesX}, usually set using function \code{\link[base]{set.seed}}.}
  \item{hyp.prior}{numeric, defines the value of the hyper-parameters \code{a} 
    and \code{b} for the inverse gamma prior of the overall variance parameter \eqn{\sigma^2}, if 
    the response distribution is Gaussian. \code{numeric}, must be a positive real valued number.
    The default is \code{hyp.prior = c(1, 0.005)}.}
  \item{distopt}{character, defines the implemented formulation for the negative 
    binomial model if the response distribution is negative binomial. The two possibilities are to 
    work with a negative binomial likelihood (\code{distopt = "nb"}) or to work with the Poisson 
    likelihood and the multiplicative random effects (\code{distopt = "poga"}).}
  \item{reference}{character, option \code{reference} is meaningful only if 
    either \code{family = "multinomial"} or \code{family = "multinomialprobit"} is specified as the 
    response distribution. In this case \code{reference} defines the \code{reference} category to be 
    chosen. Suppose, for instance, that the response is three categorical with categories 1, 2 and 
    3. Then \code{reference = 2} defines the value 2 to be the \code{reference} category.}
  \item{zipdistopt}{character, defines the zero inflated distribution for the 
    regression analysis. The two possibilities are to work with a zero infated Poisson distribution 
    (\code{zipdistopt = "zip"}) or to work with the zero inflated negative binomial likelihood 
    (\code{zipdistopt = "zinb"}).}
  \item{begin}{character, option \code{begin} is meaningful only if 
    \code{family = "cox"} is specified as the response distribution. In this case begin specifies 
    the variable that records when the observation became at risk. This option can be used to handle 
    left truncation and time-varying covariates. If \code{begin} is not specified, all observations 
    are assumed to have become at risk at time 0.}
  \item{level}{integer, besides the posterior means and medians, \pkg{BayesX} 
    provides point-wise posterior credible intervals for every effect in the model. In a Bayesian 
    approach based on MCMC simulation techniques credible intervals are estimated by computing the 
    respective quantiles of the sampled effects. By default, \pkg{BayesX} computes (point-wise) 
    credible intervals for nominal levels of 80\eqn{\%} and 95\eqn{\%}. The option \code{level[1]} 
    allows to redefine one of the nominal levels (95\eqn{\%}). Adding, for instance, 
    \code{level[1] = 99} to the options list computes credible intervals for a nominal level of 
    99\eqn{\%} rather than 95\eqn{\%}. Similar to argument \code{level[1]} the option 
    \code{level[2]} allows to redefine one of the nominal levels (80\eqn{\%}). Adding, for instance, 
    \code{level[2] = 70} to the options list computes credible intervals for a nominal level of 
    70\eqn{\%} rather than 80\eqn{\%}.}
  \item{eps}{numeric, defines the termination criterion of the estimation 
    process. If both the relative changes in the regression coefficients and the variance parameters 
    are less than \code{eps}, the estimation process is assumed to have converged.}
  \item{lowerlim}{numeric, since small variances are close to the boundary of 
    their parameter space, the usual fisher-scoring algorithm for their determination has to be 
    modified. If the fraction of the penalized part of an effect relative to the total effect is 
    less than \code{lowerlim}, the estimation of the corresponding variance is stopped and the 
    estimator is defined to be the current value of the variance (see section 6.2 of the BayesX 
    methodology manual for details).}
  \item{maxit}{integer, defines the maximum number of iterations to be used in 
    estimation. Since the estimation process will not necessarily converge, it may be useful to 
    define an upper bound for the number of iterations. Note, that \pkg{BayesX} returns results 
    based on the current values of all parameters even if no convergence could be achieved within 
    \code{maxit} iterations, but a warning message will be printed in the output window.}
  \item{maxchange}{numeric, defines the maximum value that is allowed for 
    relative changes in parameters in one iteration to prevent the program from crashing because of 
    numerical problems. Note, that \pkg{BayesX} produces results based on the current values of all 
    parameters even if the estimation procedure is stopped due to numerical problems, but an error 
    message will be printed in the output window.}
  \item{leftint}{character, gives the name of the variable that contains the 
    lower (left) boundary \eqn{T_{lo}} of the interval \eqn{[T_{lo}, T_{up}]} for an interval 
    censored observation. for right censored or uncensored observations we have to specify 
    \eqn{T_{lo} = T_{up}} . If leftint is missing, all observations are assumed to be right censored 
    or uncensored, depending on the corresponding value of the censoring indicator.}
  \item{lefttrunc}{character, option \code{lefttrunc} specifies the name of the 
    variable containing the left truncation time \eqn{T_{tr}}. For observations that are not 
    truncated, we have to specify \eqn{T_{tr} = 0}. If \code{lefttrunc} is missing, all observations 
    are assumed to be not truncated. for multi-state models variable \code{lefttrunc} specifies the 
    left endpoint of the corresponding time interval.}
  \item{state}{character, for multi-state models, \code{state} specifies the 
    current state variable of the process.}
  \item{algorithm}{character, specifies the selection algorithm. Possible values 
    are \code{"cdescent1"} (adaptive algorithms in the methodology manual, see subsection 6.3), 
    \code{"cdescent2"} (adaptive algorithms 1 and 2 with backfitting, see remarks 1 and 2 of section 
    3 in Belitz and Lang (2008)), \code{"cdescent3"} (search according to cdescent1 followed by 
    cdescent2 using the selected model in the first step as the start model) and \code{"stepwise"} 
    (stepwise algorithm implemented in the \code{gam} routine of S-plus, see Chambers and 
    Hastie, 1992). This option will rarely be specified by the user.}
  \item{criterion}{character, specifies the goodness of fit criterion. If 
    \code{criterion = "MSEP"} is specified the data are randomly divided into a test- and validation 
    data set. The test data set is used to estimate the models and the validation data set is used 
    to estimate the mean squared prediction error (MSEP) which serves as the goodness of fit 
    criterion to compare different models. The proportion of data used for the test and validation 
    sample can be specified using option proportion, see below. The default is to use 75\emph{\%} of 
    the data for the training sample.}
  \item{proportion}{numeric, this option may be used in combination with option 
    \code{criterion = "MSEP"}, see above. In this case the data are randomly divided into a training 
    and a validation sample. proportion defines the fraction (between 0 and 1) of the original data 
    used as training sample.} 
  \item{startmodel}{character, defines the start model for variable selection. 
    Options are \code{"linear"}, \code{"empty"}, \code{"full"} and \code{"userdefined"}.} 
  \item{trace}{character, specifies how detailed the output in the output window 
    will be. Options are \code{"trace_on"}, \code{"trace_half"} and \code{"trace_off"}.} 
  \item{steps}{integer, defines the maximum number of iterations. If the 
    selection process has not converged after \code{steps} iterations the algorithm terminates and a 
    warning is raised. Setting \code{steps = 0} allows the user to estimate a certain model without 
    any model choice. This option will rarely be specified by the user.}
  \item{CI}{character, compute confidence intervals for linear and nonlinear 
    terms. Option \code{CI} allows to compute confidence intervals. Options are \code{CI = "none"}, 
    confidence intervals conditional on the selected model \code{CI = "MCMCselect"} and 
    unconditional confidence intervals where model uncertainty is taken into account 
    \code{CI = "MCMCbootstrap"}. Both alternatives are computer intensive. Conditional confidence 
    intervals take much less computing time than unconditional intervals. The advantage of 
    unconditional confidence intervals is that sampling distributions for the degrees of freedom or 
    smoothing parameters are obtained.} 
  \item{bootstrapsamples}{integer, defines the number of bootstrap samples used 
    for \code{"CI = MCMCbootstrap"}.}
  \item{\dots}{not used}
%  \item{phi}{logical indicating whether the precision parameter
%    phi should be treated as a full model parameter (\code{TRUE}, default)
%    or as a nuisance parameter.} 
}

\value{
  A list with the arguments specified is returned.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\references{
  For methodological and reference details see the \pkg{BayesX} manuals available at:  
  \url{http://www.BayesX.org}.

  Belitz C, Lang S (2008). Simultaneous selection of variables and smoothing parameters in 
    structured additive regression models. \emph{Computational Statistics \& Data Analysis}, 
    \bold{53}, 61--81.

  Chambers JM, Hastie TJ (eds.) (1992). \emph{Statistical Models in S}. Chapman \& Hall, 
    London.

  Umlauf N, Adler D, Kneib T, Lang S, Zeileis A (2015).
    Structured Additive Regression Models: An R Interface to BayesX.
    \emph{Journal of Statistical Software}, \bold{63}(21), 1--46.
    \url{http://www.jstatsoft.org/v63/i21/}
}

\seealso{
  \code{\link{bayesx}}.
}

\examples{
bayesx.control()

\dontrun{
set.seed(111)
n <- 500
## regressors
dat <- data.frame(x = runif(n, -3, 3))
## response
dat$y <- with(dat, 10 + sin(x) + rnorm(n, sd = 0.6))

## estimate models with
## bayesx MCMC and REML
b1 <- bayesx(y ~ sx(x), method = "MCMC", data = dat)
b2 <- bayesx(y ~ sx(x), method = "REML", data = dat)

## compare reported output
summary(b1)
summary(b2)
}
}

\keyword{regression}
