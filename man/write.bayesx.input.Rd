\name{write.bayesx.input}
\alias{write.bayesx.input}

\title{
  Write the BayesX Program
}

\description{
  Function \code{write.bayesx.input} takes an object from \code{\link{parse.bayesx.input}} and 
  translates the input to an executable program file which may be send to the \pkg{BayesX} binary.
}

\usage{
write.bayesx.input(object)
}

\arguments{
  \item{object}{An object of class \code{"bayesx.input"}, see \code{\link{parse.bayesx.input}}}
}

\details{
  This function translates the model specified in the \code{formula} within
  \code{\link{parse.bayesx.input}} or \code{\link{bayesx}} into a \pkg{BayesX} executable program
  file, secondly the 
  function writes a data file into the specified directory chosen in \code{\link{bayesx.control}},
  \code{\link{parse.bayesx.input}} or \code{\link{bayesx}}, where \pkg{BayesX} will find the 
  necessary variables for estimation.
}

\value{
  Function returns a \code{list} containing a character string with all commands used within the 
  executable of \pkg{BayesX}, the program name, model name and the file directory where the program 
  file is stored.
}

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\examples{
## generate some data
set.seed(111)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, -3, 3),
   w = runif(n, 0, 6), fac = factor(rep(1:10, n/10)))

## response
dat$y <- with(dat, 1.5 + sin(x) + cos(z) * sin(w) +
   c(2.67, 5, 6, 3, 4, 2, 6, 7, 9, 7.5)[fac] + rnorm(n, sd = 0.6))

## create BayesX .prg
pars <- parse.bayesx.input(y ~ sx(x) + sx(z, w, bs = "te") + fac,
  data = dat)
prg <- write.bayesx.input(pars)
print(prg)

## have a look at the generated files
## which are used within BayesX
print(list.files(paste(tempdir(), "/bayesx", sep = "")))
}

\keyword{regression}
