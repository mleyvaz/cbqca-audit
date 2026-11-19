# R27 Li, Zhang et al. (2024) PLOS ONE 10.1371/journal.pone.0303687 — fsQCA, 52 zonas piloto (comunidad inteligente).
# Datos: S1 Datasheet (s001.xls = figshare 10.6084/m9.figshare.25380343), puntuaciones ya codificadas 0/0,33/0,67/1.
# fsQCA 3.0, consistencia 0,8, PRI 0,7, frecuencia 1. Solución: Tabla 8 (4 configuraciones).
source("replicas/csqca_comun.R")
d <- read.csv("datos/li_zhang_2024/s001.csv")[, -1]
hi <- c("V1*~V2*V4*~V5*V6", "~V1*V2*~V3*~V4*V5*V6", "V1*V2*~V3*V4*V5*V6", "~V1*V2*V3*V4*V5*V6")
r1 <- replicar_cs(d[, c(paste0("V", 1:6), "Y")], "Y", hi, id = "R27 Li Zhang 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.951079, 1, 0.946074, 0.976696),
                          cov_pub = c(0.236156, 0.141836, 0.426224, 0.50911)))
saveRDS(list(alta = r1), "replicas/r27_li_zhang_2024.rds")
