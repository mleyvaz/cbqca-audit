# R90 Ma et al. (2023) PLOS ONE 10.1371/journal.pone.0294630 — fsQCA, 47 empresas manufactureras (desempeño
# innovador, marco TOE). Datos: S2 Data (pertenencias calibradas, 2 decimales, con 0,501); S1 Data = anclas.
# Solución: Tabla 4 (configuraciones 1-4, resultado MIP alto).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r90_pone0294630/s002_journal.pone.0294630.s002.csv", check.names = FALSE)
d <- data.frame(RDC = r[["R&DC"]], TAC = r$TAC, P = r$P, CSR = r$CSR, GS = r$GS, MC = r$MC, MIP = r$MIP)
cat("N =", nrow(d), "; casos en 0,501:\n"); print(sapply(d, function(z) sum(z == 0.501)))
hi <- c("~RDC*TAC*~P*GS*~MC", "RDC*TAC*P*~CSR*GS", "RDC*TAC*~P*CSR*GS", "RDC*~TAC*P*CSR*GS*~MC")
r1 <- replicar_cs(d, "MIP", hi, id = "R90 Ma 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.943, 0.949, 0.982, 0.967), cov_pub = c(0.373, 0.326, 0.338, 0.260)))
saveRDS(list(alta = r1), "replicas/r90_ma_2023.rds")
