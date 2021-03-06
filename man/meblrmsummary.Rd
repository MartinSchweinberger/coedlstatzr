% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meblrmsummary.r, R/meblrsummary.r
\name{meblrmsummary}
\alias{meblrmsummary}
\title{Summary Tables for Mixed-Effects Binomial Logistic Regressions}
\usage{
meblrmsummary(glm0, glm1, glmer0, glmer1, dpvar)

meblrmsummary(glm0, glm1, glmer0, glmer1, dpvar)
}
\arguments{
\item{glm0}{A base-line glm object of family "binomial" with the intercept as the sole predictor.}

\item{glm1}{A glm object of family "binomial" which is analogous to the final glmer object.}

\item{glmer0}{A base-line glmer object of family "binomial" with the intercept as the sole predictor.}

\item{glmer1}{The final glmer object of family "binomial".}

\item{dpvar}{A vector containign the values of the dependent variable on which the models were fit.}
}
\description{
This function produces summary tables for mixed-effects binomial logistic regressions by extracting the relevent information from the final and a base-line glmer objects as well as their glm analogs.

This function produces summary tables for mixed-effects binomial logistic regressions by extracting the relevent information from the final and a base-line glmer objects as well as their glm analogs.
}
\examples{
\dontrun{
#glm0 = glm(depvar ~ 1, data = data, family = "binomial")
#glm1 = glm(depvar ~ indepvar, data = data, family = "binomial")
#glmer0 = glmer(depvar ~ (1|ranvar), data = data, family = "binomial")
#glmer1 = glmer(depvar ~ indepvar + (1|ranvar), data = data, family = "binomial")
#dpvar = data = data$depvar
#meblrmsummary(glm0, glm1, glmer0, glmer1, dpvar)
}
\dontrun{
glm0 = glm(depvar ~ 1, data = data, family = "binomial")
glm1 = glm(depvar ~ indepvar, data = data, family = "binomial")
glmer0 = glmer(depvar ~ (1|ranvar), data = data, family = "binomial")
glmer1 = glmer(depvar ~ indepvar + (1|ranvar), data = data, family = "binomial")
dpvar = data = data$depvar
meblrmsummary(glm0, glm1, glmer0, glmer1, dpvar)
}
}
\keyword{binomial}
\keyword{function}
\keyword{logistic}
\keyword{mixed-Effects}
\keyword{mixed-effects}
\keyword{regression,}
\keyword{summary}
\keyword{table,}
