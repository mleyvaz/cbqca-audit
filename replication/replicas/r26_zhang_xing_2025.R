# R26 Zhang, Xing et al. (2025) PLOS ONE 10.1371/journal.pone.0325811 — fsQCA, 17 innovaciones ambientales locales.
# Datos: Tabla 3 (puntuaciones ya calibradas por los autores; 0,501 en el original). Resultado: TRBPD.
# Solución intermedia: Tabla 7 (signos ●/∘ presentes, ⊗/⨂ ausentes). La Tabla 5 escribe los mismos términos
# con mayúsculas/minúsculas incoherentes; se usan los signos de la Tabla 7, que son los únicos que cuadran.
source("replicas/csqca_comun.R")
d <- read.csv("datos/zhang_xing_2025/tabla3.csv")[, -1]
hi <- c("CAS*~RR*~CD*~DP", "CAS*FE*~CD*~DP", "CAS*~RR*FE*~DP", "~RR*~FE*CD*DP", "CAS*~FE*CD*DP")
r1 <- replicar_cs(d[, c("CAS", "RR", "FE", "CD", "DP", "Y")], "Y", hi, id = "R26 Zhang Xing 2025 (TRBPD)",
  publicados = data.frame(term = hi, incl_pub = c(0.829049, 0.865476, 0.652609, 0.899687, 0.916166),
                          cov_pub = c(0.229268, 0.246079, 0.169356, 0.323818, 0.309489)))
saveRDS(list(alta = r1), "replicas/r26_zhang_xing_2025.rds")
