\name{bayesx.term.options}
\alias{bayesx.term.options}
\alias{bxopts}

\title{Show BayesX Term Options}

\description{
  \pkg{BayesX} model terms specified using functions \code{\link{sx}} may have 
  additional optional control arguments. Therefore function 
  \code{bayesx.term.options} displays the possible additional controlling parameters for a 
  particular model term.
}

\usage{
bayesx.term.options(bs = "ps", method = "MCMC")
}

\arguments{
  \item{bs}{character, the term specification for which controlling parameters should be shown.}
  \item{method}{character, for which method should additional arguments be shown, options are
    \code{"MCMC"}, \code{"REML"} and \code{"STEP"}.}
}

\details{
  At the moment the following model terms are implemented, for which additional controlling 
  parameters may be specified:
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

\author{
  Nikolaus Umlauf, Thomas Kneib, Stefan Lang, Achim Zeileis.
}

\examples{
## show arguments for P-splines
bayesx.term.options(bs = "ps")
bayesx.term.options(bs = "ps", method = "REML")

## Markov random fields
bayesx.term.options(bs = "mrf")
}

\keyword{regression}
