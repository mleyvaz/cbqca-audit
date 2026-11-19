ESTUDIO <- "R02 Bing Wan 2025"
## Réplica + CB-QCA: Bing & Wan (2025), Scientific Reports 15:41146, doi 10.1038/s41598-025-24859-y.
## Datos: Nature Global Postdoctoral Survey 2023 (figshare 24236875). Anclas 2 / 4 / 6 (artículo).
## El artículo no dice cómo combina los dos ítems de Growth y de Responsibility: se prueban media, mínimo y máximo.
suppressPackageStartupMessages({ library(QCA); library(readxl); library(cbqca) })
s <- as.data.frame(read_excel("../datos/bing_wan_2025/survey.xlsx", sheet = 1, col_types = "text", .name_repair = "minimal"))
num <- function(v) { v <- sub(" =.*$", "", v); v[v == "Not applicable"] <- NA; suppressWarnings(as.numeric(v)) }
col <- function(pat) num(s[[grep(pat, names(s), fixed = TRUE)[1]]])
raw <- data.frame(
  SAT  = col("How satisfied are you in your current postdoc?"),
  Adv  = col("-Career advancement opportunities"),
  TWI  = col("-My interest in the work"),
  Gr1  = col("-Opportunity to work on interesting projects"),
  Gr2  = col("-Access to workplace-sponsored training and seminars"),
  Resp1 = col("-My degree of independence"),
  Resp2 = col("-Ability to influence decisions that affect you"),
  Rec  = col("-Recognition for achievements"),
  Ach  = col("-Personal sense of accomplishment"))
cat("Filas:", nrow(raw), "| completas:", sum(complete.cases(raw)), "\n")
cal <- function(x) calibrate(x, type = "fuzzy", thresholds = c(2, 4, 6))
H <- c(H1 = "Adv*Gr*TWI*~Rec", H2 = "Adv*Gr*TWI*Resp", H3 = "Adv*Gr*TWI*Ach", H4 = "~Gr*Rec*Resp*TWI*Ach", H5 = "Adv*Gr*Rec*Ach*Resp")
L <- c(L1 = "~TWI*~Rec*~Adv*~Ach", L2 = "~Gr*~Ach*~Adv*~Rec*~Resp", L3 = "~Gr*~Ach*~Adv*~TWI*~Resp")
pubH <- c(0.902, 0.926, 0.922, 0.906, 0.940); covH <- c(0.192, 0.542, 0.533, 0.173, 0.487)
pubL <- c(0.910, 0.918, 0.925); covL <- c(0.374, 0.367, 0.312)
build <- function(comb, rows) {
  r <- raw[rows, ]
  f <- switch(comb, mean = function(a, b) (a + b) / 2, min = pmin, max = pmax)
  data.frame(Adv = cal(r$Adv), TWI = cal(r$TWI), Gr = cal(f(r$Gr1, r$Gr2)), Resp = cal(f(r$Resp1, r$Resp2)),
             Rec = cal(r$Rec), Ach = cal(r$Ach), Y = cal(r$SAT))
}
best <- NULL
for (rows_rule in c("complete", "outcome_only")) {
  rows <- if (rows_rule == "complete") complete.cases(raw) else !is.na(raw$SAT)
  for (comb in c("mean", "min", "max")) {
    X <- build(comb, rows); X <- X[complete.cases(X), ]
    pH <- sapply(H, function(t) as.numeric(pof(t, "Y", data = X, relation = "sufficiency")$incl.cov$inclS[1]))
    X$nY <- 1 - X$Y
    pL <- sapply(L, function(t) as.numeric(pof(t, "nY", data = X, relation = "sufficiency")$incl.cov$inclS[1]))
    err <- max(abs(c(pH - pubH, pL - pubL)))
    cat(sprintf("%-12s %-4s N=%4d  max|dif consistencia|=%.4f  H: %s  L: %s\n", rows_rule, comb, nrow(X), err,
                paste(round(pH, 3), collapse = " "), paste(round(pL, 3), collapse = " ")))
    if (is.null(best) || err < best$err) best <- list(err = err, comb = comb, rows = rows_rule, X = X)
  }
}
cat("\nMejor réplica:", best$rows, best$comb, "| max dif =", round(best$err, 4), "\n")
X <- best$X
rH <- cbqca(X, "Y", unname(H), tau = 0.5, boot = 1000); rH$table$term <- names(H)
print(rH)
rL <- cbqca(X, "nY", unname(L), tau = 0.5, boot = 1000); rL$table$term <- names(L)
print(rL)

# Complemento 17-nov: cota exacta y sensibilidad a tau (desviación declarada del protocolo)
source("csqca_comun.R")
extra_cs(X[, setdiff(names(X), "nY")], "Y", unname(H), names(H))
extra_cs(X[, setdiff(names(X), "Y")], "nY", unname(L), names(L))
