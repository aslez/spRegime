print.spregime <- function(x) {
  reg_out <- x$chow_test$reg_lst$reg_lst
  cat("Group-Specific Regression\n")  
  for (i in seq_along(reg_out)) {
    cat("\nGroup:", paste0(x$mods$group_names[i]), "\n")
    print(summary(reg_out[[i]]))
  }
  print(x$chow_test)
  print(x$coef_test)
  print(x$het_test)
}

print.spchow <- function(x) {
  cat("\nTests for Regime Stability\n")
  cat("\nWald Test\n\n")
  wald_df <- data.frame(w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c("Test Statistic", "p-value")
  print(wald_df, row.names = FALSE)
}

print.spcoef <- function(x) {
  cat("\nTests for Coefficient Stability\n")
  cat("\nWald Tests\n\n")
  wald_df <- data.frame(name = x$coef_names,
                        w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c("Coefficient", "Test Statistic", "p-value")
  print(wald_df, row.names = FALSE)
}

print.sphet <- function(x) {
  cat("\nTest for Groupwise Heteroskedasticity\n")
  cat("\nHeteroskedasticity Coefficients\n\n")
  print(x$het_coef)
  cat("\nWald Test\n\n")
  wald_df <- data.frame(w = round(x$wald, 2), p = round(x$wald.p, 4))
  names(wald_df) <- c("Test Statistic", "p-value")
  print(wald_df, row.names = FALSE)
}
