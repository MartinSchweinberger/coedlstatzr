#' @title Extract Effect Size (Pearson's rho) from Linear Regressions
#'
#' @description This function returns the Pearson's rho values from fixed-effects linear regressions from a glm object.
#' @param x A glm object of family "gaussian".
#' @export
#' @keywords linear regression, pearson's rho, effect size, function
#' @return NULL
#' @examples \dontrun{
#' model.glmer = glmer(depvar ~ indepvar + (1|ranvar), data = data, family = "gaussian")
#' eflm(model.glmer)
#' }
eflm <- function(x) {
  df <- summary(x)[[20]][6]
  t <-  summary(x)[[20]][8]
  r <- sqrt((t^2)/((t^2)+df))
  return(paste("Pearson's r = ", round(r, 3)))
}
