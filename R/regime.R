regime <- function(restrict, group, data, var_int = FALSE, 
                   lw_lst = NULL, error = TRUE, robust = NULL) {
  mods <- regmod(restrict, group, data, lw_lst, error, robust)
  chow_test <- spchow(mods, var_int = var_int)
  coef_test <- spcoef(mods)
  het_test <- sphet(mods)
  result <- list(mods = mods,
                 chow_test = chow_test,
                 coef_test = coef_test, het_test = het_test)
  class(result) <- 'spregime'
  result
}
