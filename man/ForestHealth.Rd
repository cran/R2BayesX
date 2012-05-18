\name{ForestHealth}
\alias{ForestHealth}
\alias{forest}

\title{Forest Health Data}

\description{
  The data set consists of 16 variables with 1796 observations on forest health to identify 
  potential factors influencing the health status of trees and therefore the vital status of the 
  forest. In addition to covariates characterizing a tree and its stand, the exact locations of the 
  trees are known. The interest is on detecting temporal and spatial trends while accounting for 
  further covariate effects in a flexible manner.
}

\usage{data("ForestHealth")}

\format{
A data frame containing 1793 observations on 16 variables.
\describe{
  \item{id:}{tree location identification number.}
  \item{year:}{year of census.}
  \item{defoliation:}{percentage of tree defoliation in three ordinal categories, 
    `defoliation < 12.5\%', `12.5\% <= defoliation < 50\%' and  `defoliation >= 50\%'}
  \item{x:}{x-coordinate of the tree location.}
  \item{y:}{y-coordinate of the tree location.}
  \item{age:}{age of stands in years.}
  \item{canopy:}{forest canopy density in percent.}
  \item{inclination:}{slope inclination in percent.}
  \item{elevation:}{elevation (meters above sea level).}
  \item{soil:}{soil layer depth in cm.}
  \item{ph:}{soil pH at 0-2cm depth.}
  \item{moisture:}{soil moisture level with categories `moderately dry', `moderately moist' and
    `moist or temporarily wet'.}
  \item{alkali:}{proportion of base alkali-ions with categories `very low', `low', `high' and
    `very high'.}
  \item{humus:}{humus layer thickness in cm, categorical coded.}
  \item{stand:}{stand type with categories `deciduous' and `mixed'.}
  \item{fertilized:}{fertilization applied with categories `yes' and `no'.}
}
}

\source{
  \url{http://www.BayesX.org}.
}

\references{
  Kneib, T. & Fahrmeir, L. (2010): A Space-Time Study on Forest Health. In: Chandler, R. E. \& 
    Scott, M. (eds.): Statistical Methods for Trend Detection and Analysis in the Environmental 
    Sciences, Wiley.

  G\"ottlein A, Pruscha H (1996). Der Einfuss von Bestandskenngr\"ossen, Topographie, Standord und 
    Witterung auf die Entwicklung des Kronenzustandes im Bereich des Forstamtes Rothenbuch. 
    \emph{Forstwissens. Zent.}, \bold{114}, 146--162.
}

\seealso{\code{\link{bayesx}}}

\examples{
\dontrun{
## load zambia data and map
data("ForestHealth")
data("BeechBnd")

fm <- bayesx(defoliation ~  stand + fertilized + 
  humus + moisture + alkali + ph + soil + 
  sx(age) + sx(inclination) + sx(canopy) +
  sx(year) + sx(elevation),
  family = "cumlogit", method = "REML", data = ForestHealth)

summary(fm)
plot(fm, term = c("sx(age)", "sx(inclination)", 
  "sx(canopy)", "sx(year)", "sx(elevation)"))
}
}

\keyword{datasets}
