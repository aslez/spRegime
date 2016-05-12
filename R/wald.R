wald_test <- function(R, b, vm) {
  rb <- R %*% b
  rvri <- R %*% vm %*% t(R)
  as.numeric(t(rb) %*% solve(rvri) %*% rb)
}
