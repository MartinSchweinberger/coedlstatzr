#' @title Summary Tables for Step_Wise Step_Down Model Fitting
#'
#' @description This function produces summary tables for step-wise step-down model fitting effects binomial logistic regressions by extracting the relevent information from a glm and an lrm object.
#' @param mdlcmp is a list of model comparisons of which individual elements are created by anova calls.
#' @export
#' @keywords model fitting, step-wise step-down, summary table, function
#' @return NULL
#' @examples \dontrun{
#' m0 <-glm(depvar ~ 1, data = data, family = "binomial")
#' m1 = glm(depvar ~ indepvar1, data = data, family = "binomial")
#' m2 = glm(depvar ~ indepvar + inpepvar2, data = data, family = "binomial")
#' m1m0 = anova(m1, m0, test = "Chi)
#' m2m1 = anova(m2, m1, test = "Chi)
#' mdlfttngswsd(m1m0, m2m1)
#' }
mdlfttngswsu <- function(mdlcmp){
  mdl.cmp.df1 <- sapply(mdlcmp, function(x) {
  p.nice <- function(z) {
    as.vector(unlist(sapply(z, function(w) {
      ifelse(w < .001, return("p < .001***"),
      ifelse(w < .01, return("p <  .01 **"),
      ifelse(w < .05, return("p <  .05  *"),
      ifelse(w < .1, return("p <  .10(*)"), return("n.s."))))) } ))) }

  formula <- ifelse(length(attr(x, "heading")) == 4, gsub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4]),
    ifelse(length(attr(x, "heading")) == 5,
      paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4])),
        gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[5]))),
      ifelse(length(attr(x, "heading")) == 6,
        paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[5])),
          gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[6]))), paste("error"))))
  formula <- gsub(" ", "", formula)
  formula.cm <- ifelse(length(attr(x, "heading")) == 4, gsub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3]),
    ifelse(length(attr(x, "heading")) == 5,
      paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3]))),
      ifelse(length(attr(x, "heading")) == 6,
        paste(gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[3])),
          gsub(" ", "", sub("m[0-9]{1,2}.glmer: ", "", attr(x ,"heading")[4]))), paste("error"))))
  formula.cm <- gsub(" ", "", formula.cm)
  splt.formula <- strsplit(formula, "[~|+|*]")
  splt.formula.cm <- strsplit(formula.cm, "[~|+|*]")
  model <- ifelse(length(attr(x, "heading")) == 4, gsub(": .*", "", attr(x, "heading")[4]),
    ifelse(length(attr(x, "heading")) == 5, gsub(": .*", "", attr(x, "heading")[5]),
    ifelse(length(attr(x, "heading")) == 6, gsub(": .*", "", attr(x, "heading")[6]), "NA")))
  added <- c(as.vector(unlist(splt.formula)), as.vector(unlist(splt.formula.cm)))
  added <- names(which(table(added)==1))
  added   <- paste(added, collapse = "+")
  comp    <- gsub(": .*", "", attr(x ,"heading")[3])
  df      <- x[[1]][[2]]
  aic     <- round(x[[2]][[2]], 2)
  bic     <- round(x[[3]][[2]], 2)
  ll      <- round(x[[4]][[2]], 2)
  dev     <- round(x[[5]][[2]], 2)
  x2      <- round(x[[6]][[2]], 2)
  x2df    <- x[[7]][[2]]
  p       <- round(x[[8]][[2]], 5)
  sig <-  p.nice(z = p)
  mdl.fttng <- cbind(model, formula, added, comp, df, aic, bic, ll, dev, x2, x2df, p, sig)
}  )
mdl.cmp.df2 <- t(mdl.cmp.df1)
colnames(mdl.cmp.df2) <- c("Model", "Formula", "Term Added", "Compared to...", "DF", "AIC", "BIC", "LogLikelihood", "Residual Deviance", "X2", "X2DF", "p-value", "Significance")
mdl.cmp.df2 <- as.data.frame(mdl.cmp.df2)
return(mdl.cmp.df2)
}
