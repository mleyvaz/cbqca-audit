# R21 (csQCA). Xin et al. 2025, Healthcare (Basel), 10.3390/healthcare13233128 (PMC12692109).
# Identificación de ideación suicida e intervención en 47 casos escolares en China.
# Datos: tablas de verdad publicadas (Tablas 4 y 8) con n y consistencia por fila; casos con
# resultado = round(n x consistencia). APROXIMADA: las tablas omiten filas con 1 caso (umbral de
# frecuencia), quedan 42 de 47 casos en cada análisis.
# Soluciones intermedias (Tablas 5 y 9), notación de núcleo/periferia convertida a términos.
# Discrepancia publicada: el camino 2 (PD*SC*ST*SL) se reporta con consistencia 1, pero la fila
# PD=1, SH=0, SC=1, ST=1, SL=1 (11 casos, consistencia 0,82) pertenece a él; su PRI 0,857 lo delata.
source("replicas/csqca_comun.R")
expandir <- function(tt, conds, y) {
  d <- do.call(rbind, lapply(seq_len(nrow(tt)), function(i) {
    k <- round(tt$n[i] * tt$cons[i])
    cbind(tt[rep(i, tt$n[i]), conds], Y = c(rep(1, k), rep(0, tt$n[i] - k)))
  })); rownames(d) <- NULL; names(d)[names(d) == "Y"] <- y; d
}
t4 <- read.table(header = TRUE, text = "
PD SH SC ST SL n cons
1 1 1 1 1 3 1
0 0 1 1 0 2 1
0 0 1 1 1 2 1
1 0 1 1 1 11 0.8181812
1 1 0 1 1 4 0.5
1 0 1 0 1 2 0.5
1 0 0 1 1 7 0
1 1 0 1 0 3 0
1 0 0 0 0 2 0
1 0 0 1 0 2 0
0 0 0 0 1 2 0
0 0 0 1 1 2 0")
d1 <- expandir(t4, c("PD", "SH", "SC", "ST", "SL"), "IDENT"); d1$nIDENT <- 1 - d1$IDENT
cat("Identificación: N =", nrow(d1), "| éxitos:", sum(d1$IDENT), "(publicado: 47 casos, 20 éxitos)\n")
p1 <- c("~PD*~SH*SC*ST", "PD*SC*ST*SL"); n1 <- c("PD*~SH*~SC*~SL", "~PD*~SH*~SC*SL", "PD*~SC*ST*~SL", "PD*~SH*~SC*ST")
r1 <- replicar_cs(d1[, c(1:5, 6)], "IDENT", p1, id = "R21 Xin 2025 (identificación)",
  publicados = data.frame(term = p1, incl_pub = 1, cov_pub = c(0.2, 0.15)))
r2 <- replicar_cs(d1[, c(1:5, 7)], "nIDENT", n1, id = "R21 Xin 2025 (identificación fallida)",
  publicados = data.frame(term = n1, incl_pub = 1, cov_pub = c(0.148, 0.148, 0.185, 0.333)))
t8 <- read.table(header = TRUE, text = "
SII SM PS SS FS n cons
1 0 0 1 1 9 1
1 0 1 1 1 7 1
0 0 1 1 1 6 1
0 0 1 0 1 4 1
1 0 0 1 0 3 1
0 1 1 1 1 3 1
0 0 1 1 0 2 1
0 0 0 1 1 2 1
0 1 0 1 1 2 1
0 1 0 0 0 4 0")
d2 <- expandir(t8, c("SII", "SM", "PS", "SS", "FS"), "INTERV"); d2$nINTERV <- 1 - d2$INTERV
cat("Intervención: N =", nrow(d2), "| éxitos:", sum(d2$INTERV), "(publicado: 47 casos, 41 éxitos)\n")
p2 <- c("~SM*SS*FS", "~SII*SS*FS", "SII*~SM*~PS*SS", "~SII*~SM*PS*SS", "~SII*~SM*PS*FS")
n2 <- "~SII*SM*~PS*~SS*~FS"
r3 <- replicar_cs(d2[, c(1:5, 6)], "INTERV", p2, id = "R21 Xin 2025 (intervención)",
  publicados = data.frame(term = p2, incl_pub = 1, cov_pub = c(0.585, 0.317, 0.293, 0.195, 0.244)))
r4 <- replicar_cs(d2[, c(1:5, 7)], "nINTERV", n2, id = "R21 Xin 2025 (intervención fallida)",
  publicados = data.frame(term = n2, incl_pub = 1, cov_pub = 0.667))
saveRDS(list(r1, r2, r3, r4), "replicas/r21_xin_2025.rds")
