# QCA layer: evaluate solution terms written in QCA notation and report, for
# each term, its consistency, its bounds on the probabilities of causation and
# its configurational breakdown point.

# Membership of a QCA expression such as "A*~B + C" (fuzzy min/max/negation).
.term_membership <- function(data, expr) {
  disj <- trimws(strsplit(expr, "+", fixed = TRUE)[[1]])
  vals <- sapply(disj, function(cj) {
    lits <- trimws(strsplit(cj, "*", fixed = TRUE)[[1]])
    m <- sapply(lits, function(l) {
      neg <- startsWith(l, "~")
      v <- sub("^~", "", l)
      if (!v %in% names(data)) stop("condition '", v, "' not found in data")
      x <- suppressWarnings(as.numeric(as.character(data[[v]])))
      if (any(x < 0 | x > 1, na.rm = TRUE)) stop("memberships must lie in [0, 1]")
      if (neg) 1 - x else x
    })
    if (is.null(dim(m))) m <- matrix(m, nrow = 1)
    apply(m, 1, min)
  })
  if (is.null(dim(vals))) vals <- matrix(vals, nrow = 1)
  apply(vals, 1, max)
}

#' Causal-bounds QCA
#'
#' For each solution term, reports the classic sufficiency consistency, the
#' bounds on PN, PS and PNS under exogeneity, the bounds without assumptions,
#' and the configurational breakdown point. Probabilities of causation are
#' defined for crisp sets: fuzzy memberships are dichotomised at
#' \code{threshold} for the causal part (the classic fuzzy consistency is
#' reported alongside).
#'
#' @param data data frame of set memberships in [0, 1]
#' @param outcome name of the outcome column
#' @param terms character vector of QCA expressions, e.g. c("A*~B", "C")
#' @param threshold crossover used to dichotomise memberships (default 0.5;
#'   cases exactly at the threshold are counted as outside)
#' @param tau threshold for the breakdown point of PS (default 0.5)
#' @param boot number of bootstrap replicates for sampling intervals of the
#'   exogeneity bounds (0 = none)
#' @param seed random seed for the bootstrap
#' @return an object of class "cbqca"
#' @export
cbqca <- function(data, outcome, terms, threshold = 0.5, tau = 0.5,
                  boot = 0, seed = 1) {
  if (!outcome %in% names(data)) stop("outcome not found in data")
  y_f <- suppressWarnings(as.numeric(as.character(data[[outcome]])))
  y <- as.integer(y_f > threshold)
  rows <- lapply(terms, function(tm) {
    x_f <- .term_membership(data, tm)
    ok <- !is.na(x_f) & !is.na(y_f)
    if (any(!ok)) warning("term '", tm, "': ", sum(!ok), " cases with missing membership dropped")
    x_f <- x_f[ok]; yf_t <- y_f[ok]; y_t <- y[ok]
    x <- as.integer(x_f > threshold)
    n11 <- sum(x == 1 & y_t == 1); n10 <- sum(x == 1 & y_t == 0)
    n01 <- sum(x == 0 & y_t == 1); n00 <- sum(x == 0 & y_t == 0)
    if (n11 + n10 == 0 || n01 + n00 == 0) {
      warning("term '", tm, "': all cases on one side of the threshold; skipped")
      return(NULL)
    }
    p <- joint_from_counts(n11, n10, n01, n00)
    ex <- pc_bounds(p, 0)
    na <- pc_bounds(p, 1)
    res <- data.frame(
      term = tm, n_in = n11 + n10, n_out = n01 + n00,
      incl_fuzzy = sum(pmin(x_f, yf_t)) / sum(x_f),
      incl_crisp = n11 / (n11 + n10),
      PS_lo = ex$lower[ex$target == "PS"], PS_up = ex$upper[ex$target == "PS"],
      PN_lo = ex$lower[ex$target == "PN"], PN_up = ex$upper[ex$target == "PN"],
      PNS_lo = ex$lower[ex$target == "PNS"], PNS_up = ex$upper[ex$target == "PNS"],
      PS_lo_noassump = na$lower[na$target == "PS"],
      breakdown_PS = breakdown_point(p, "PS", tau),
      breakdown_PNS = breakdown_point(p, "PNS", 0),
      PS_lo_exact95 = ps_exact_bound(n11, n11 + n10, n01, n01 + n00),
      stringsAsFactors = FALSE)
    if (boot > 0) {
      set.seed(seed)
      bs <- replicate(boot, {
        i <- sample.int(length(x), replace = TRUE)
        xb <- x[i]; yb <- y_t[i]
        nb <- c(sum(xb & yb), sum(xb & !yb), sum(!xb & yb), sum(!xb & !yb))
        if (nb[1] + nb[2] == 0 || nb[3] + nb[4] == 0) return(NA_real_)
        pc_bounds_exogenous(joint_from_counts(nb[1], nb[2], nb[3], nb[4]))$lower[2]
      })
      res$PS_lo_ci_low <- stats::quantile(bs, 0.025, na.rm = TRUE)
      res$PS_lo_ci_high <- stats::quantile(bs, 0.975, na.rm = TRUE)
    }
    res
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  structure(list(table = out, outcome = outcome, threshold = threshold,
                 tau = tau, n = nrow(data), boot = boot),
            class = "cbqca")
}

#' @export
print.cbqca <- function(x, digits = 3, ...) {
  cat("Causal-bounds QCA (CB-QCA)\n")
  cat("Outcome:", x$outcome, " | cases:", x$n,
      " | crossover:", x$threshold, " | tau for PS breakdown:", x$tau, "\n\n")
  tb <- x$table
  num <- vapply(tb, is.numeric, logical(1))
  tb[num] <- lapply(tb[num], round, digits)
  print(tb, row.names = FALSE)
  cat("\nPS/PN/PNS bounds assume exogeneity (no confounding between the configuration\n",
      "and the outcome). breakdown_PS: smallest confounding level at which the lower\n",
      "bound of PS falls to tau; breakdown_PNS: at which PNS can be zero.\n", sep = "")
  invisible(x)
}
