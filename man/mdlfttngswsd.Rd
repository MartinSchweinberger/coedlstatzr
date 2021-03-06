% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdlfttngswsd.r
\name{mdlfttngswsd}
\alias{mdlfttngswsd}
\title{Summary Tables for the Step-Wise Step-Down Model Fitting Process of Mixed-Effects Binomial Logistic Regression Models}
\usage{
mdlfttngswsd(mdlcmp)
}
\arguments{
\item{mdlcmp}{A list of model comparisons that result from anova results (e.g. m0m1: anova(m0, m1, test = "Chi))}
}
\description{
This function produces summary tables for fixed-effects binomial logistic regressions by extracting the relevent information from a glm and an lrm object.
}
\examples{
\dontrun{
m2 = glm(depvar ~ indepvar + inpepvar2, data = data, family = "binomial")
m1 = glm(depvar ~ indepvar1, data = data, family = "binomial")
m0 =glm(depvar ~ 1, data = data, family = "binomial")
m2m1 = anova(m2, m1, test = "Chi)
m1m0 = anova(m1, m0, test = "Chi)
mdlfttngswsd(m2m1, m1m0)
}
}
\keyword{binomial}
\keyword{function}
\keyword{logistic}
\keyword{regression,}
\keyword{summary}
\keyword{table,}
