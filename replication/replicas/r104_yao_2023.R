# R104 Yao y Qin (2023) PLOS ONE 10.1371/journal.pone.0281115 — fsQCA, gobierno corporativo de bancos y crédito
# verde, 72 casos (banco-año). Datos: S1 File = membresías YA CALIBRADAS por los autores (hoja «校准完结果2»).
# Anclas en Tabla 2 (no hace falta recalibrar). Software declarado fsQCA 3.0. Soluciones: Tabla 4 (alta, 1a-2b)
# y Tabla 5 (no alta, 3a-6); ● / · presencia, ○ / ⊕ ausencia. Valores publicados con 4 decimales.
source("replicas/csqca_comun.R")
d <- read.csv("datos/r104_pone0281115/calibrados.csv", row.names = 1)
cat("N =", nrow(d), "\n")
cat("Necesidad (publicado OC 0,6565/0,6979; HHI 0,8990/0,8988; ~EI 0,6909/0,6735):\n")
for (k in c("OC", "HHI", "~EI")) print(round(pof(k, "GC", data = d, relation = "necessity")$incl.cov[, c(1, 3)], 4))
d$nGC <- 1 - d$GC
cd <- c("OC", "IB", "EI", "ASB", "HHI", "LQ")
hi <- c("OC*~EI*HHI*~LQ", "OC*~IB*~EI*~ASB*~LQ", "OC*IB*ASB*HHI*~LQ", "OC*~EI*~ASB*HHI", "OC*~IB*~ASB*HHI")
lo <- c("OC*EI*~HHI*LQ", "IB*EI*~HHI*LQ", "EI*ASB*~HHI*LQ", "~OC*~EI*~ASB*~HHI*LQ", "~OC*IB*~HHI*LQ",
        "~OC*IB*EI*~ASB*~HHI", "~OC*~IB*EI*ASB*~HHI", "~OC*IB*~EI*ASB*~HHI")
r1 <- replicar_cs(d[, c(cd, "GC")], "GC", hi, id = "R104 Yao 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.9914, 0.9126, 0.9467, 0.9843, 0.9746),
                          cov_pub = c(0.4134, 0.2173, 0.1626, 0.3643, 0.2980)))
r2 <- replicar_cs(d[, c(cd, "nGC")], "nGC", lo, id = "R104 Yao 2023 (no alta)",
  publicados = data.frame(term = lo, incl_pub = c(0.9831, 0.9875, 0.9671, 0.9788, 0.9582, 0.9960, 0.9301, 0.9205),
                          cov_pub = c(0.2108, 0.3073, 0.2778, 0.1410, 0.3056, 0.1396, 0.1849, 0.1352)))
saveRDS(list(alta = r1, baja = r2), "replicas/r104_yao_2023.rds")
# CLASIFICACIÓN: exacta con los datos calibrados publicados (dif. máx. 0,001 consistencia, 0,0002 cobertura; 13/13 términos).
