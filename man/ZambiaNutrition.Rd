\name{ZambiaNutrition}
\alias{ZambiaNutrition}
\alias{zambia}

\title{Determinants of Childhood Malnutrition in Zambia}

\description{
  The Demographic Health Surveys (DHS) of Zambia was conducted 1992. The survey is produced jointly
  by Macro International, a USAIDfunded firm specializing in demographic research, and the national
  statistical agency of the country.

  Malnutrition among children is usually determined by assessing an anthropometric status of the
  children relative to a reference standard. In our example, malnutrition is measured by stunting
  or insufficient height for age, indicating chronic malnutrition. Stunting for a child \eqn{i} 
  is determined using a \eqn{Z}-score defined as
  \deqn{stunting_i = \frac{AI_i - MAI}{\sigma}}{stunting_i = (AI_i - MAI)/\sigma}
  where \eqn{AI} refers to the child's anthropometric indicator (height at a certain age in our 
  example), while \eqn{MAI} and \eqn{\sigma} correspond to the median and the standard deviation in 
  the reference population, respectively. 

  The main interest is on modeling the dependence of malnutrition on covariates including the
  age of the child, the body mass index of the child's mother, the district the child lives in and 
  some further categorial covariates. 
}

\usage{data("ZambiaNutrition")}

\format{
A data frame containing 4847 observations on 8 variables.
\describe{
  \item{stunting:}{standardised \eqn{Z}-score for stunting.}
  \item{mbmi:}{body mass index of the mother.}
  \item{agechild:}{age of the child in months.}
  \item{district:}{district where the mother lives.}
  \item{memployment:}{mother's employment status with categories `working' and `not working'.}
  \item{meducation:}{mother's educational status with categories for complete primary but 
    incomplete secondary `no/incomplete', complete secondary or higher `minimum primary' and no 
    education or incomplete primary `minimum secondary'.}
  \item{urban:}{locality of the domicile with categories `yes' and `no'.}
  \item{gender:}{gender of the child with categories `male' and `female'.}
}
}

\source{
  \url{http://www.BayesX.org}.
}

\references{
Kandala, N. B., Lang, S., Klasen, S., Fahrmeir, L. (2001): Semiparametric Analysis of the 
  Socio-Demographic and Spatial Determinants of Undernutrition in Two African Countries. 
  \emph{Research in Official Statistics}, \bold{1}, 81--100.
}

\seealso{\code{\link{bayesx}}}

\examples{
\dontrun{
## load zambia data and map
data("ZambiaNutrition")
data("ZambiaBnd")

## estimate model
zm <- bayesx(stunting ~ memployment + meducation + urban + gender + 
  sx(mbmi) + sx(agechild) + sx(district, bs = "mrf", map = ZambiaBnd) +
  sx(district, bs = "re"), iter = 12000, burnin = 2000, step = 10,
  data = ZambiaNutrition)

summary(zm)

## plot smooth effects
plot(zm, term = c("sx(bmi)", "sx(agechild)", "sx(district)"), map = ZambiaBnd)

## for more examples 
demo("zambia")
}
}

\keyword{datasets}
