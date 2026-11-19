# R51 Bai et al. (2026) PLOS ONE 10.1371/journal.pone.0361299 — csQCA, 24 eventos deportivos (polarización en línea).
# Datos: Tabla 3 (matriz de codificación caso a caso, binaria). Consistencia 0,80, frecuencia 1.
# Solución intermedia: Tabla 5 (5 caminos; • presente, ⊗ ausente). Sin solución publicada para ~ESS.
source("replicas/csqca_comun.R")
d <- read.csv("datos/bai_2026/tabla3.csv")
cd <- c("PT", "NNF", "OL", "CI", "FM", "EA", "SP")
hi <- c("NNF*OL*~CI*EA", "~PT*NNF*CI*~FM*~EA", "NNF*OL*~FM*EA*~SP", "NNF*OL*FM*~EA*SP", "~PT*OL*~CI*FM*EA*SP")
r1 <- replicar_cs(d[, c(cd, "ESS")], "ESS", hi, id = "R51 Bai 2026 (polarización)",
  publicados = data.frame(term = hi, incl_pub = rep(1, 5), cov_pub = c(0.5, 0.3125, 0.0625, 0.0625, 0.125)))
saveRDS(list(alta = r1), "replicas/r51_bai_2026.rds")
# CLASIFICACIÓN: exacta (dif. 0).
