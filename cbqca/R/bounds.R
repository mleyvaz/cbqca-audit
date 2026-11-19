# Bounds on Pearl's probabilities of causation (PN, PS, PNS) for a binary
# exposure X (a QCA configuration) and a binary outcome Y, computed exactly
# by linear programming over response types.
#
# Response types t = (Y0, Y1):
#   "never"      (0,0)   Y = 0 whatever X
#   "causal"     (0,1)   Y = 1 only if X = 1
#   "preventive" (1,0)   Y = 1 only if X = 0
#   "always"     (1,1)   Y = 1 whatever X
# Unknowns q[x, t] = P(X = x, type = t), 8 cells.
#
# Targets (Tian and Pearl, 2000):
#   PNS = P(Y1 = 1, Y0 = 0)                = sum_x q[x, causal]
#   PN  = P(Y0 = 0 | X = 1, Y = 1)         = q[1, causal] / P(X = 1, Y = 1)
#   PS  = P(Y1 = 1 | X = 0, Y = 0)         = q[0, causal] / P(X = 0, Y = 0)
#
# Confounding model, indexed by delta in [0, 1]:
#   the share of each response type within X = 1 and within X = 0 differs from
#   a common distribution pi by at most delta:
#   | q[x, t] / P(X = x) - pi[t] | <= delta   for all x, t.
#   delta = 0  is exogeneity (types independent of X);
#   delta = 1  imposes nothing beyond the observational data.

.types <- c("never", "causal", "preventive", "always")

#' Joint observational probabilities from a 2 x 2 table of counts
#'
#' @param n11 cases with X = 1, Y = 1
#' @param n10 cases with X = 1, Y = 0
#' @param n01 cases with X = 0, Y = 1
#' @param n00 cases with X = 0, Y = 0
#' @return named numeric vector p11, p10, p01, p00 summing to one
#' @export
joint_from_counts <- function(n11, n10, n01, n00) {
  n <- c(p11 = n11, p10 = n10, p01 = n01, p00 = n00)
  if (any(n < 0)) stop("counts must be non-negative")
  if (sum(n) == 0) stop("the table is empty")
  n / sum(n)
}

# Build and solve the LP for one target and direction.
.pc_lp <- function(p, target, delta, direction) {
  px1 <- p[["p11"]] + p[["p10"]]
  px0 <- p[["p01"]] + p[["p00"]]
  # variables: q[1, 4 types], q[0, 4 types], pi[4 types]  -> 12 variables
  nv <- 12
  idx_q1 <- 1:4; idx_q0 <- 5:8; idx_pi <- 9:12
  A <- NULL; dir <- NULL; rhs <- NULL
  add <- function(row, d, r) {
    A <<- rbind(A, row); dir <<- c(dir, d); rhs <<- c(rhs, r)
  }
  row0 <- function() numeric(nv)
  # observational margins. X = 1: Y = Y1 -> Y = 1 for causal, always
  r <- row0(); r[idx_q1[c(2, 4)]] <- 1; add(r, "=", p[["p11"]])
  r <- row0(); r[idx_q1[c(1, 3)]] <- 1; add(r, "=", p[["p10"]])
  # X = 0: Y = Y0 -> Y = 1 for preventive, always
  r <- row0(); r[idx_q0[c(3, 4)]] <- 1; add(r, "=", p[["p01"]])
  r <- row0(); r[idx_q0[c(1, 2)]] <- 1; add(r, "=", p[["p00"]])
  # pi is a distribution
  r <- row0(); r[idx_pi] <- 1; add(r, "=", 1)
  # confounding constraints | q[x,t] - P(x) pi[t] | <= delta P(x)
  for (t in 1:4) {
    for (arm in list(list(idx = idx_q1, px = px1), list(idx = idx_q0, px = px0))) {
      r <- row0(); r[arm$idx[t]] <- 1; r[idx_pi[t]] <- -arm$px
      add(r, "<=", delta * arm$px)
      add(r, ">=", -delta * arm$px)
    }
  }
  obj <- row0()
  if (target == "PNS") {
    obj[c(idx_q1[2], idx_q0[2])] <- 1; scale <- 1
  } else if (target == "PN") {
    if (p[["p11"]] <= 0) return(NA_real_)
    obj[idx_q1[2]] <- 1; scale <- 1 / p[["p11"]]
  } else if (target == "PS") {
    if (p[["p00"]] <= 0) return(NA_real_)
    obj[idx_q0[2]] <- 1; scale <- 1 / p[["p00"]]
  } else stop("unknown target")
  sol <- lpSolve::lp(direction, obj, A, dir, rhs)
  if (sol$status != 0) return(NA_real_)
  val <- sol$objval * scale
  min(1, max(0, val))
}

#' Bounds on the probabilities of causation
#'
#' Exact lower and upper bounds on PN, PS and PNS for a binary exposure and a
#' binary outcome, given the observational joint distribution and a
#' confounding level \code{delta}.
#'
#' @param p named vector from \code{joint_from_counts}
#' @param delta confounding level in [0, 1]; 0 = exogeneity, 1 = no assumption
#' @return data frame with columns target, lower, upper
#' @export
pc_bounds <- function(p, delta = 0) {
  if (delta < 0 || delta > 1) stop("delta must lie in [0, 1]")
  out <- lapply(c("PN", "PS", "PNS"), function(tg) {
    data.frame(target = tg,
               lower = .pc_lp(p, tg, delta, "min"),
               upper = .pc_lp(p, tg, delta, "max"))
  })
  do.call(rbind, out)
}

#' Closed-form bounds under exogeneity (Tian and Pearl, 2000)
#'
#' Used as a cross-check of the linear programme at delta = 0.
#' @param p named vector from \code{joint_from_counts}
#' @export
pc_bounds_exogenous <- function(p) {
  p1 <- p[["p11"]] / (p[["p11"]] + p[["p10"]])   # P(Y = 1 | X = 1)
  p0 <- p[["p01"]] / (p[["p01"]] + p[["p00"]])   # P(Y = 1 | X = 0)
  data.frame(
    target = c("PN", "PS", "PNS"),
    lower = c(max(0, (p1 - p0) / p1),
              max(0, (p1 - p0) / (1 - p0)),
              max(0, p1 - p0)),
    upper = c(min(1, (1 - p0) / p1),
              min(1, p1 / (1 - p0)),
              min(p1, 1 - p0))
  )
}

#' Configurational breakdown point
#'
#' The smallest confounding level delta at which the lower bound of a
#' probability of causation falls to \code{tau} or below. A large value means
#' that the causal reading survives strong confounding.
#'
#' @param p named vector from \code{joint_from_counts}
#' @param target "PS", "PN" or "PNS"
#' @param tau threshold for the lower bound (default 0: the causal effect is
#'   no longer guaranteed to exist; use 0.5 for "more likely than not")
#' @param tol bisection tolerance
#' @return delta*, or 0 if the bound is already at or below tau under
#'   exogeneity, or 1 if it stays above tau even without assumptions
#' @export
breakdown_point <- function(p, target = "PS", tau = 0, tol = 1e-4) {
  lb <- function(d) .pc_lp(p, target, d, "min")
  l0 <- lb(0)
  if (is.na(l0)) return(NA_real_)
  if (l0 <= tau) return(0)
  if (lb(1) > tau) return(1)
  lo <- 0; hi <- 1
  while (hi - lo > tol) {
    mid <- (lo + hi) / 2
    if (lb(mid) > tau) lo <- mid else hi <- mid
  }
  hi
}
