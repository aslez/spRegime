print.spregime <- function(x) {
  lapply(x$chow_test$reg_lst$reg_lst, function(x) print(summary(x)))
  print(x$chow_test)
  print(x$coef_test)
  print(x$het_test)
}

print.spchow <- function(x) {
  cat("\nTests for Regime Stability\n")
  cat("\nWald Test\n\n")
  wald_df <- data.frame(w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c('Test Statistic', 'p-value')
  print(wald_df, row.names = FALSE)
}

print.spcoef <- function(x) {
  cat("\nTests for Coefficient Stability\n")
  cat("\nWald Tests\n\n")
  wald_df <- data.frame(name = x$coef_names,
                        w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c('Coefficient', 'Test Statistic', 'p-value')
  print(wald_df, row.names = FALSE)
}

print.sphet <- function(x) {
  cat("\nTest for Groupwise Heteroskedasticity\n")
  cat("\nHeteroskedasticity Coefficients\n\n")
  print(x$het_coef)
  cat("\nWald Test\n\n")
  wald_df <- data.frame(w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c('Test Statistic', 'p-value')
  print(wald_df, row.names = FALSE)
}
