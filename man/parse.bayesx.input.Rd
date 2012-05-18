\name{parse.bayesx.input}
\alias{parse.bayesx.input}

\title{
  Parse BayesX Input
}

\description{
  Funtion to parse \code{bayesx} input parameters which are then send to 
  \code{\link{write.bayesx.input}}.
}

\usage{
parse.bayesx.input(formula, data, weights = NULL, 
  subset = NULL, offset = NULL, na.action = na.fail, 
  contrasts = NULL, control = bayesx.control(\dots), \dots)
}

\arguments{
  \item{formula}{symbolic description of the model (of type \code{y ~ x}). For more details see 
    \code{\link{bayesx}} and \code{\link{sx}}.}
  \item{data}{a \code{\link{data.frame}} or \code{\link{list}} containing the model response 
    variable and covariates required by the formula. By default the variables are taken from 
    \code{environment(formula)}: typically the environment from which \code{bayesx} is called. 
    Argument \code{data} may also be a character string defining the directory the data is stored, 
    where the first row in the data set must contain the variable names and columns should be tab
    separated.}
  \item{weights}{prior weights on the data.}
  \item{subset}{an optional vector specifying a subset of observations to be used in the fitting 
    process.}
  \item{offset}{can be used to supply a model offset for use in fitting.}
  \item{na.action}{a function which indicates what should happen when the data contain \code{NA}'s.}
  \item{contrasts}{an optional list. See the \code{contrasts.arg} of 
    \code{\link[stats]{model.matrix.default}}.}
  \item{control}{specify several global control parameters for \code{bayesx}, see 
    \code{\link{bayesx.control}}.}
  \item{\dots}{arguments passed to \code{\link{bayesx.control}}.}
}

\value{
  Returns a list of class \code{"bayesx.input"} which is send to \code{\link{write.bayesx.input}} 
  for processing within \code{\link{bayesx}}.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\examples{
parse.bayesx.input(y ~ x1 + sx(x2), data = "")
}

\keyword{regression}
