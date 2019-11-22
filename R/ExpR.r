#' @title Function for Estimating Expected Type II Error Rates
#'
#' @description This function returns the likelihood od type II errors (beta errors) for multiple linear regressions given their sample size.
#' @param x A glm object of family "gaussian".
#' @export
#' @keywords beta error, type II error, multiple linear regression, linear regression
#' @seealso
#' @return NULL
#' @examples \dontrun{
#' Example code will come later!
#' }
expR <- function(x) {
 ifelse(((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)) > .05,
 return(paste("A random sample is expected to cause a correlation of the size",
 ((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)),
 "between the predictors and the predicted", collapse = "")),
 return(paste("Based on the sample size expect a false positive correlation of",
 round(((ncol(summary(x)$coefficients)-1)/(length(x$fitted)-1)), 4),
 "between the predictors and the predicted",
 collapse = "")))}
