#' @title Function which evaluates Sample Size
#'
#' @description This function evaluates whether the sample size is sufficient for a regression analysis based on  is sufficient based on Green, Samuel B. (1991) How many subjects does it take to do a regression analysis. Multivariate Behavioral Research. 26(3): 499-510. summary tables for simple linear regressions by extracting the relevent information from a regression object created with the lm function.
#' @param x A multiple linear regression model object.
#' @export
#' @keywords regression, linear regression, sample size, function
#' @return NULL
#' @examples \dontrun{
#' model.glm = glm(depvar ~ indepvar, data = data, family = gaussian)
#' smplesz(model.glm)
#' }
smplesz <- function(x) {
 ifelse((length(x$fitted)<(104 + ncol(summary(x)$coefficients)-1)) == TRUE,
 return(
 paste("Sample too small: please increase your sample by ",
 104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
 " data points", collapse = "")),
 return("Sample size sufficient")) }

