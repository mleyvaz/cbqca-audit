test_that("LP bounds at delta = 0 equal the closed-form exogeneity bounds", {
  set.seed(42)
  for (i in 1:300) {
    n <- sample(1:200, 4, replace = TRUE)
    p <- joint_from_counts(n[1], n[2], n[3], n[4])
    lp <- pc_bounds(p, 0)
    cf <- pc_bounds_exogenous(p)
    expect_equal(lp$lower, cf$lower, tolerance = 1e-6)
    expect_equal(lp$upper, cf$upper, tolerance = 1e-6)
  }
})

test_that("bounds widen monotonically as confounding grows", {
  p <- joint_from_counts(60, 15, 20, 55)
  prev <- pc_bounds(p, 0)
  for (d in seq(0.1, 1, by = 0.1)) {
    cur <- pc_bounds(p, d)
    expect_true(all(cur$lower <= prev$lower + 1e-9))
    expect_true(all(cur$upper >= prev$upper - 1e-9))
    prev <- cur
  }
})

test_that("random feasible response-type tables respect the LP bounds", {
  # brute force: draw type distributions consistent with the data and delta = 1
  set.seed(7)
  p <- joint_from_counts(40, 10, 15, 35)
  b <- pc_bounds(p, 1)
  for (i in 1:2000) {
    a <- stats::runif(1); c <- stats::runif(1)
    q1 <- c(never = (1 - a) * p[["p10"]], causal = c * p[["p11"]],
            preventive = a * p[["p10"]], always = (1 - c) * p[["p11"]])
    e <- stats::runif(1); f <- stats::runif(1)
    q0 <- c(never = (1 - e) * p[["p00"]], causal = e * p[["p00"]],
            preventive = f * p[["p01"]], always = (1 - f) * p[["p01"]])
    PN <- q1[["causal"]] / p[["p11"]]
    PS <- q0[["causal"]] / p[["p00"]]
    PNS <- q1[["causal"]] + q0[["causal"]]
    expect_true(PN >= b$lower[1] - 1e-9 && PN <= b$upper[1] + 1e-9)
    expect_true(PS >= b$lower[2] - 1e-9 && PS <= b$upper[2] + 1e-9)
    expect_true(PNS >= b$lower[3] - 1e-9 && PNS <= b$upper[3] + 1e-9)
  }
})

test_that("breakdown point is 0 when there is no association and positive otherwise", {
  p_null <- joint_from_counts(25, 25, 25, 25)
  expect_equal(breakdown_point(p_null, "PNS", 0), 0)
  p_strong <- joint_from_counts(90, 5, 5, 90)
  expect_gt(breakdown_point(p_strong, "PNS", 0), 0.3)
})

test_that("cbqca runs on QCA expressions with fuzzy data", {
  set.seed(3)
  n <- 300
  A <- stats::runif(n); B <- stats::runif(n)
  Y <- pmin(1, pmax(0, 0.7 * pmin(A, 1 - B) + 0.3 * stats::runif(n)))
  d <- data.frame(A = A, B = B, Y = Y)
  r <- cbqca(d, "Y", c("A*~B", "A + B"), boot = 50)
  expect_s3_class(r, "cbqca")
  expect_equal(nrow(r$table), 2)
  expect_true(all(r$table$PS_lo <= r$table$PS_up))
})

test_that("ps_exact_bound reproduces the R17 worked example and is conservative", {
  # TIMELY*PHARMA in Ziemann et al. (2022): 7/7 inside, 0/8 outside
  expect_equal(round(ps_exact_bound(7, 7, 0, 8), 2), 0.35)
  # ~TIMELY: 4/4 inside, 3/11 outside -> uninformative
  expect_equal(ps_exact_bound(4, 4, 3, 11), 0)
  # never above the point bound
  p <- joint_from_counts(30, 5, 10, 55)
  expect_lte(ps_exact_bound(30, 35, 10, 65), pc_bounds_exogenous(p)$lower[2])
})
