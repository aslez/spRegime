sphet <- function(mods) {
  if (mods$type != "ols") {
    sigma2 <- sapply(mods$reg_lst, function(x) summary(x)$s2)
  }
  else {
    sigma2 <- sapply(mods$reg_lst, function(x) summary(x)$sigma^2)
  }
  het_vcov <- bdiag(as.list(2 * sigma2^2 / sapply(mods$n_lst, identity)))
  R <- cbind(1, diag(-1, nrow = mods$G - 1))
  wald <- wald_test(R, sigma2, het_vcov)
  p <- pchisq(wald, 1, lower.tail = FALSE)
  result <- list(reg_lst = mods,
                  het_coef = sigma2, het_vcov = het_vcov,
                  wald = wald, wald.p = p)
  class(result) <- "sphet"
  result
}
