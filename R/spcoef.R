spcoef <- function(mods) {
  G <- mods$G
  K <- mods$K
  zero_mat <- matrix(0, nrow = G - 1, ncol = G * K)
  R_lst <- lapply(1:K, mat_rep, zero_mat = zero_mat)
  wald <- sapply(1:K, single_coef, R_lst = R_lst, reg_dat = mods)
  p <- pchisq(wald, G - 1, lower.tail = FALSE)
  result <- list(reg_lst = mods,
                 wald = wald, wald.p = p,
                 coef_names = mods$coef_names)
  class(result) <- 'spcoef'
  result
}

single_coef <- function(coef_num, R_lst, reg_dat) {
  R <- R_lst[[coef_num]]
  wald_test(R, reg_dat$b, reg_dat$vm)
}
