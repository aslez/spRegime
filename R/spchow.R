spchow <- function(mods) {
  G <- mods$G
  K <- mods$K
  zero_mat <- matrix(0, nrow = G - 1, ncol = G * K)
  R_lst <- lapply(1:K, mat_rep, zero_mat = zero_mat)
  R <- do.call(rbind, R_lst)
  wald <- wald_test(R, mods$b, mods$vm)
  p <- pchisq(wald, (G - 1) * K, lower.tail = FALSE)
  result <- list(reg_lst = mods, wald = wald, wald.p = p)
  class(result) <- 'spchow'
  result
}

mat_rep <- function(k, zero_mat) {
  new_mat <- zero_mat
  new_mat[, k] <- 1
  K <- ncol(new_mat) / (nrow(new_mat) + 1)
  for (i in 1:nrow(new_mat)) {
    new_mat[i, (i * K) + k] <- -1
  }
  new_mat
}
