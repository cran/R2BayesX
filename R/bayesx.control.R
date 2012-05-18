bayesx.control <-
function(model.name = "bayesx.estim", family = "gaussian", method = "MCMC",  
  verbose = TRUE, dir.rm = TRUE, outfile = NULL, replace = FALSE,
  iterations = 12000L, burnin = 2000L, maxint = NULL, step = 10L, predict = TRUE,
  seed = NULL, hyp.prior = NULL, distopt = NULL,  reference = NULL, zipdistopt = NULL,
  begin = NULL, level = NULL,  eps = 1e-05, lowerlim = 0.001, maxit = 400L, maxchange = 1e+06,
  leftint = NULL, lefttrunc = NULL, state = NULL, algorithm = NULL, criterion = NULL,
  proportion = NULL, startmodel = NULL, trace = NULL, steps = NULL, CI = NULL,
  bootstrapsamples = NULL, ...)
{
  control <- list(...)
  start <- 6L + length(control)
  control$model.name <- model.name 
  if(method == "mcmc")
    method <- "MCMC"
  if(method == "hmcmc")
    method <- "HMCMC"
  if(method == "reml")
    method <- "REML"
  if(method == "step")
    method <- "STEP"
  if(is.null(method))
    control$method <- "MCMC"
  else
    control$method <- method
  control$verbose <- verbose
  control$dir.rm <- dir.rm
  if(!is.null(outfile))
    control$outfile <- path.expand(outfile)
  else
    control$outfile <- outfile
  control$replace <- replace
  if(is.null(family))
    family <- "gaussian"
  if(is.function(family))
    family <- family()$family
  control$family <- family
  if(!is.null(level))
    if(length(level) < 2L)
      level <- c(level , level)
  if(burnin > iterations)
    burnin <- 1L
  if(is.null(seed))
    seed <- round(runif(1L) * .Machine$integer.max)
  if(method == "MCMC" || method == "HMCMC" || method == "quantreg") {
    control$iterations <- iterations
    control$burnin <- burnin
    control$maxint <- maxint
    control$step <- step
    control$predict <- predict
    control$setseed <- seed
    control$aresp <- hyp.prior[1L]
    control$bresp <- hyp.prior[2L]
    control$begin <- begin
    control$level1 <- level[1L]
    control$level2 <- level[2L]
    if(method == "quantreg") {
      control$family <- "quantreg"
      control$method <- method <- "HMCMC"
      if(is.null(control$quantile))
        control$quantile <- 0.5
    }
    if(family == "multinomial" || family == "multinomialprobit")
      control$reference <- reference
    if(family == "zip" || family == "nbinomial") {
      control$distopt <- distopt
      control$zipdistopt <- zipdistopt
    }
    if(method == "HMCMC") {
      control$method <- "MCMC"
      control$hmcmc <- TRUE
      control$hlevel <- 1L
    } else control$hmcmc <- FALSE
  }
  if(method == "REML") {
    control$eps <- eps
    control$lowerlim <- lowerlim
    control$maxit <- maxit
    control$maxchange <- maxchange
    control$leftint <- leftint
    control$lefttrunc <- lefttrunc
    control$state <- state
    if(family == "multinomial" || family == "multinomialprobit")
      control$reference <- reference
    control$hmcmc <- FALSE
  }
  if(method == "STEP") {
    if(!is.null(CI) && (CI == "MCMCselect" || CI == "MCMCbootstrap")) {
      control$iterations <- iterations
      control$burnin <- burnin
      control$step <- step
    }
    control$predict <- predict
    control$setseed <- seed
    control$algorithm <- algorithm 
    control$criterion <- criterion
    control$proportion <- proportion
    control$startmodel <- startmodel
    control$trace <- trace
    control$steps <- steps
    control$CI <- CI
    if(!is.null(CI) && CI == "MCMCbootstrap")
      control$bootstrapsamples <- bootstrapsamples
    control$level1 <- level[1L]
    control$level2 <- level[2L]
    if(family == "multinomial" || family == "multinomialprobit")
      control$reference <- reference
    if(family == "zip" || family == "nbinomial") {
      control$distopt <- distopt
      control$zipdistopt <- zipdistopt
    }
    control$hmcmc <- FALSE
  }
  if(!is.null(outfile))
    start <- start + 1L
  attr(control,"co.id") <- start:length(control)

  return(control)
}

