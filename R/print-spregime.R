print.spregime <- function(x) {
  mod_lst <- x$mods
  reg_lst <- mod_lst$reg_lst
  K <- mod_lst$K
  G <- mod_lst$G
  vm_lst <- lapply(1:G, function(z) 
    mod_lst$vm[(((z - 1) * K) + 1):(z * K), (((z - 1) * K) + 1):(z * K)])
  
  cat("Group-Specific Regression\n")  
  for (i in seq_along(reg_lst)) {
    cat("\nGroup:", paste0(mod_lst$group_names[i]), "\n")
    print(lmtest::coeftest(reg_lst[[i]], as.matrix(vm_lst[[i]])))
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
