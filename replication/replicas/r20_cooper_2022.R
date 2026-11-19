# R20 (csQCA). Cooper et al. 2022, Implementation Science Communications, 10.1186/s43058-022-00303-4
# (PMC9137145). Sostenimiento de programas comunitarios (N = 32 con datos completos).
# Datos: tabla de verdad publicada (Tabla 3) con número de casos y consistencia por fila;
# casos con resultado = round(n x consistencia). Solución intermedia: Tabla 4.
source("replicas/csqca_comun.R")
tt <- read.table(header = TRUE, text = "
n SUP FUND PART ORG EVAL COMM PLAN cons
1 1 0 0 1 1 1 0 1
1 1 0 1 1 1 0 0 1
1 1 1 0 1 0 0 0 1
1 1 1 0 1 0 1 0 1
1 1 1 0 1 1 1 0 1
1 1 1 1 0 0 1 0 1
1 1 1 1 0 0 1 1 1
1 1 1 1 1 0 1 1 1
6 1 1 1 1 1 1 1 .83
3 1 0 1 1 1 1 1 .67
2 1 1 1 0 1 1 1 .50
11 0 0 0 0 0 0 0 .09
1 0 0 0 1 0 0 0 0
1 0 1 0 0 0 0 0 0")
d <- do.call(rbind, lapply(seq_len(nrow(tt)), function(i) {
  k <- round(tt$n[i] * tt$cons[i])
  cbind(tt[rep(i, tt$n[i]), 2:8], ROUT = c(rep(1, k), rep(0, tt$n[i] - k)))
})); rownames(d) <- NULL
cat("N =", nrow(d), "| con resultado:", sum(d$ROUT), "\n")
terms <- c("SUP*ORG*FUND", "SUP*ORG*COMM*EVAL*~PLAN", "SUP*ORG*EVAL*PART*~PLAN", "SUP*COMM*PART*FUND*~EVAL")
r1 <- replicar_cs(d, "ROUT", terms, id = "R20 Cooper 2022 (sostenimiento)",
  publicados = data.frame(term = terms, incl_pub = c(.90, 1, 1, 1), cov_pub = c(.53, .12, .06, .18)))
saveRDS(list(r1), "replicas/r20_cooper_2022.rds")
