GRstats <- function(object, term = NULL, ...)
{
  require("coda")
  if((n <- length(object)) < 2L)
    stop("at least two models ar needed for calculation!")
  return(gelman.diag(samples(object, model = NULL, term, coda = TRUE), ...))
}

