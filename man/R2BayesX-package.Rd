\name{R2BayesX-package}
\alias{R2BayesX-package}
\alias{R2BayesX}
\alias{r2bayesx}
\alias{r2bayesx-package}
\docType{package}
 
\title{
  Estimate STAR Models with BayesX
}

\description{
  This package interfaces the \pkg{BayesX} (\url{http://www.BayesX.org}) command-line binary from
  \R. The main model fitting function is called \code{\link{bayesx}}. 

  Before STAR models can be estimated, the command-line version of \pkg{BayesX} needs to be
  installed, which is done by installing the \R source code package \pkg{BayesXsrc}. Please see
  function \code{\link{bayesx}} and \code{\link{bayesx.control}} for more details on model fitting
  and controlling.

  The package also provides functionality for high level graphics of estimated effects, see function
  \code{\link{plot.bayesx}}, \code{\link{plot2d}}, \code{\link{plot3d}}, \code{\link{plotblock}},
  \code{\link{plotmap}}, \code{\link{plotsamples}} and \code{\link{colorlegend}}.

  More standard extractor functions and methods for the fitted model objects may be applied, e.g., 
  see function \code{\link{summary.bayesx}}, \code{\link{fitted.bayesx}}, 
  \code{\link{residuals.bayesx}}, \code{\link{samples}}, \code{\link{plot.bayesx}}, as well as
  \code{\link[stats]{AIC}}, \code{\link[stats]{BIC}} etc., please see the examples of the help 
  sites. Predictions for new data based on refitting with weights can be obtained by function
  \code{\link{predict.bayesx}}.

  In addition, it is possible to run arbitrary \pkg{BayesX} program files using function 
  \code{\link[BayesXsrc]{run.bayesx}}. \pkg{BayesX} output files that are stored in a directory may
  be read into \R calling function \code{\link{read.bayesx.output}}.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\keyword{ package }

\examples{
  ## to see the package demos
  demo(package = "R2BayesX") 
}

\keyword{regression} 
