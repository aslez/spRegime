regmod <- function(restrict, group, data, lw_lst, error, robust) {
  mmat <- as.data.frame(model.matrix(update(group, ~ . -1), data))
  if (!is.null(lw_lst) & error) {
    reg_lst <- lapply(seq_along(mmat), function(x) 
      errorsarlm(restrict, data[as.logical(mmat[[x]]), ], lw_lst[[x]]))
    n_lst <- lapply(reg_lst, function(x) length(residuals(x)))
    type <- 'error'
  }
  if (!is.null(lw_lst) & !error) {
    reg_lst <- lapply(seq_along(mmat), function(x) 
      lagsarlm(restrict, data[as.logical(mmat[[x]]), ], lw_lst[[x]]))
    n_lst <- lapply(reg_lst, function(x) length(residuals(x)))
    type <- 'lag'
  }
  if (is.null(lw_lst)) { 
    reg_lst <- lapply(mmat, function(x) lm(restrict, data[as.logical(x), ]))
    n_lst <- lapply(reg_lst, function(x) length(residuals(x)))
    type <- 'ols'
  }
  reg_lst <- unname(reg_lst)
  valid_b <- lapply(reg_lst, function(x) !is.na(coef(x)))
  knum <- Reduce(intersect, mapply(which, valid_b, SIMPLIFY = FALSE))
  kstr <- names(valid_b[[1]])[knum]
  b <- do.call(c, lapply(reg_lst, function(x) coefficients(x)[kstr]))
  if (is.null(robust)) {
    vm <- bdiag(lapply(reg_lst, function(x) vcov(x)[kstr, kstr]))
  }
  else   vm <- bdiag(lapply(reg_lst, function(x) vcovHC(x, robust)[kstr, kstr]))
  G <- length(reg_lst)
  K <- length(kstr)
  list(reg_lst = reg_lst, coef_names = kstr, group_names = colnames(mmat), 
       type = type, b = b, vm = vm, K = K, G = G, n_lst = n_lst)
}
