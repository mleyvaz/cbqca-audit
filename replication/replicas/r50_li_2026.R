# R50 Li et al. (2026) PLOS ONE 10.1371/journal.pone.0354468 — csQCA, 30 vídeos de Douyin (Shaoguan).
# Datos: Zenodo 10.5281/zenodo.17084333 (dataset.xlsx), ya dicotomizados (resultado: mediana del índice DCI).
# Coinciden con la Tabla 3 (tabla de verdad caso a caso). Solución: Tabla 6 (7 configuraciones).
source("replicas/csqca_comun.R")
d <- read.csv("datos/li_2026/datos.csv")
cat("Casos con resultado 1:", sum(d$DCI), "\n")
cd <- c("DUR", "HAS", "VIS", "PRO", "THE", "EMO", "NAR", "CAM")
hi <- c("DUR*HAS*~VIS*~PRO*THE*~EMO*~NAR*CAM", "DUR*HAS*VIS*~PRO*THE*EMO*~NAR*~CAM",
        "~DUR*HAS*~VIS*PRO*THE*EMO*NAR*~CAM", "~DUR*HAS*~VIS*PRO*THE*EMO*~NAR*CAM",
        "~DUR*HAS*~VIS*PRO*~THE*EMO*NAR*CAM", "DUR*HAS*~VIS*PRO*THE*~EMO*NAR*CAM",
        "DUR*HAS*VIS*PRO*~THE*EMO*NAR*CAM")
r1 <- replicar_cs(d[, c(cd, "DCI")], "DCI", hi, id = "R50 Li 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = rep(1, 7), cov_pub = c(rep(0.067, 6), 0.200)))
saveRDS(list(alta = r1), "replicas/r50_li_2026.rds")
# CLASIFICACIÓN: exacta (dif. 0).
