# Exact (conservative) sampling bound for the lower bound of PS under exogeneity.

#' Exact sampling bound for the lower bound of PS
#'
#' Evaluates the exogeneity lower bound of PS, \eqn{(p_1 - p_0)/(1 - p_0)}, at the
#' one-sided 97.5\% Clopper-Pearson lower limit of \eqn{p_1 = P(Y=1 \mid X=1)} and
#' the one-sided 97.5\% upper limit of \eqn{p_0 = P(Y=1 \mid X=0)}. By Bonferroni
#' the two limits hold jointly with probability at least 95\%, and the bound is
#' increasing in \eqn{p_1} and decreasing in \eqn{p_0}, so the result is a
#' conservative 95\% lower confidence bound for PS_lo for one fixed term.
#' Coverage is per term, not simultaneous across terms. A value of 0 means the
#' bound is uninformative. Unlike the bootstrap, it does not degenerate when the
#' term has no counterexamples.
#'
#' @param k1 cases with X = 1 and Y = 1
#' @param n1 cases with X = 1
#' @param k0 cases with X = 0 and Y = 1
#' @param n0 cases with X = 0
#' @param level joint confidence level (default 0.95)
#' @return a number in [0, 1]
#' @export
ps_exact_bound <- function(k1, n1, k0, n0, level = 0.95) {
  if (n1 <= 0 || n0 <= 0) return(NA_real_)
  a <- (1 - level) / 2
  p1L <- if (k1 == 0) 0 else stats::qbeta(a, k1, n1 - k1 + 1)
  p0U <- if (k0 == n0) 1 else stats::qbeta(1 - a, k0 + 1, n0 - k0)
  if (p0U >= 1) return(0)
  max(0, (p1L - p0U) / (1 - p0U))
}
