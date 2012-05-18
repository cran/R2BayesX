\name{read.bayesx.output}
\alias{read.bayesx.output}

\title{
  Read BayesX Output from Directories
}

\description{
  This function automatically reads in \pkg{BayesX} estimation output which is stored in an output 
  directory.
}

\usage{
read.bayesx.output(dir, model.name = NULL)
}

\arguments{
  \item{dir}{a character string, specifies the directory file where \pkg{BayesX} output is stored.}
  \item{model.name}{a character string, specifies the base name of the model that should be read in,
    also see the examples. If not supplied \code{read.bayesx.output} tries to read in all existing
    model outputs in \code{dir}, every model is then stored as one element in the output 
    \code{list}. By convention, \code{read.bayesx.output} searches for existing \code{.tex} output 
    files, and others, to identify different models in the \code{dir} folder.}
}

\details{
  The function searches for model term objects in the specified directory, which are then stored in 
  a \code{list}. Each model term object will be of class \code{xx.bayesx}, so the generic functions 
  described in \code{\link{plot.bayesx}} may be applied for visualizing the results. In addition 
  summary statistics of the models may be printed to the \R console with 
  \code{\link{summary.bayesx}}.
}

\value{ 
  \code{read.bayesx.output} typically returns a list of class \code{"bayesx"} with the first element 
  containing a list with the following objects:
  \item{formula}{the STAR formula used,}
  \item{bayesx.setup}{an object of class \code{"bayesx.input"}, see 
    \code{\link{parse.bayesx.input}},}
  \item{bayesx.prg}{a character containing the \code{.prg} file used for estimation with 
    \code{\link[BayesXsrc]{run.bayesx}},}
  \item{bayesx.run}{details on processing with \code{\link[BayesXsrc]{run.bayesx}},}
  \item{call}{the original function call,}
  \item{fitted.values}{the fitted values of the estimated model,}
  \item{residuals}{the residuals of the estimated model,}
  \item{effects}{a \code{list} containing fitted effects of model terms, also see 
    \code{\link{fitted.bayesx}} and \code{\link{samples}},}
  \item{fixed.effects}{a \code{matrix} with estimation results for fixed effects,}
  \item{variance}{estimation results for the variance parameter of the model,}
  \item{smooth.hyp}{a \code{matrix} with estimation results smooth terms,}
  \item{model.fit}{list containing additional information to be supplied to
    \code{\link{summary.bayesx}}.}
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\seealso{
  \code{\link{summary.bayesx}}, \code{\link{plot.bayesx}}, \code{\link{samples}}.
}

\examples{
## load example data from
## package example folder
dir <- file.path(find.package("R2BayesX"), "/examples/ex01")
b <- read.bayesx.output(dir)

## some model summaries
print(b)
summary(b)

## now plot estimated effects
plot(b)

## 2nd example
dir <- file.path(find.package("R2BayesX"), "/examples/ex02")
list.files(dir)

## dir contains of 2 different
## base names
## 01 only one nonparametric effect
b <- read.bayesx.output(dir, model.name = "nonparametric")
plot(b)

## 02 only one bivariate
## nonparametric effect
b <- read.bayesx.output(dir, model.name = "surface")
plot(b)
}

\keyword{regression}
