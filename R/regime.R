#' Spatial Regime Model
#'
#' Estimates the stability of a given model across a set of
#' user-specified regimes.
#'
#' \command{regime} compares the fit of a restricted or global model in which a
#' given set of coefficients are treated as fixed to the fit of an unrestricted
#' or local model in which the coefficients in question are allowed to vary
#' across a set of user-specified regimes.  Regime models are specified using
#' two formulas, the first of which depicts the global model, the second of
#' which depicts the variable used to specify regimes.  Spatial regime models
#' are estimated using the \command{errorsarlm} and \command{lagsarlm} routines
#' available as part of the \pkg{spreg} package.  The resulting test is
#' ultimately carried out using the \command{anova} command which, in the case
#' of non-spatial regime models, results in the standard F-test.  In contrast,
#' the use of the \command{anova} command in the context of likelihood-based
#' models such as \command{errorsarlm} and \command{lagsarlm} results in a
#' likelihood ratio test.  The null model can be adjusted to allow for
#' regime-specific intercepts.
#'
#' @param restrict \command{lm}-style formula for the restricted regression.
#' @param group RHS formula depicting only the factor variable
#' used to identify regimes (i.e. no dependent variable needed).
#' @param data model data frame.
#' @param var_int a logical value indicating whether or not to include
#' group-specific intercepts in the restricted model.  Default is \code{FALSE}
#' @param lw_lst a list of \code{listw} objects corresponding to the levels of 
#' the factor used to define regimes.  The default value is \code{NULL}.
#' @param error a logical value indicating type of spatial regime model to use
#' when \code{!is.null(lw_lst)}.  A value of \code{TRUE} produces a spatial
#' error model, while a value of \code{FALSE} produces a spatial lag model.
#' #' @param robust a character string to be passed to \code{vcovHC} denoting
#' the type of standard errors to be used.  The default value is \code{const},
#' which produces conventional standard errors.
#' @param ... additional arguments to be passed to the model-fitting routine.
#'
#' @return A spregime object containing the following elements:
#' \item{mods}{a list of objects of containing information from the
#' regime-specific models, including the models themselves, as well as the
#' coefficient names, the regime names, the model type, the parameter estimates,
#' the variance-covariance matrix, the number of predictors, the number of
#' regimes, and the number of observations per regime.
#' \item{chow_test}{a \code{spchow} class object containing the components used
#' to test the overall stability of the coefficients, including the original 
#' regime-specific models, the Wald statistic, and the p-value.}
#' \item{coef_test}{a \code{spcoef} class object containing the components used
#' to test the stability of individual coefficients, including the original 
#' regime-specific models, the Wald statistics, the p-values, and the names of
#' the coefficients.}
#' \item{het_test}{a \code{sphet} class object containing the components of a
#' test for groupwise heteroskedasticity, including the original regime-specific
#' models, the regime-specific variances, the variance-covariance matrix, the
#' Wald statistic, and the p-value.}
#'
#' @references Anselin, L.  1988.
#' \emph{Spatial Econometrics: Methods and Models}.
#' Durdrecht: Kluwer Academic Publishers, pp. 123-124.
#' @references Chow, G.  1960.  "Tests of Equality Between Sets of Coefficients
#' in Two Linear Regressions."  \emph{Econometrica} 28:591-605.
#'
#' @author Adam Slez \email{aslez@@virginia.edu}
#'
#' @export
#' @examples
#' data(nat60_sf)
#' reg_mod60 <- regime(HR60 ~ RD60 + PS60 + MA60 + DV60 + UE60, 
#'      group = ~ SOUTH, data = nat60_sf)
#' reg_mod60

regime <- function(restrict, group, data, var_int = FALSE,
                   lw_lst = NULL, error = TRUE, robust = "const") {
  mods <- regmod(restrict, group, data, lw_lst, error, robust)
  chow_test <- spchow(mods, var_int = var_int)
  coef_test <- spcoef(mods)
  het_test <- sphet(mods)
  result <- list(mods = mods,
                 chow_test = chow_test,
                 coef_test = coef_test,
                 het_test = het_test)
  class(result) <- "spregime"
  result
}
