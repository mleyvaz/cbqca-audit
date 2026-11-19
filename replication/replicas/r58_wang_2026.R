# R58 Wang et al. (2026) PLOS ONE 10.1371/journal.pone.0344374 — fsQCA, 31 provincias chinas (universidades e innovación provincial).
# Datos: S1 Data (s001.xlsx). Columnas usadas según las anclas de la Tabla 1: PI, IUCR1, GAT, HC, ERC2, ISE, TSA1
# (el archivo trae dos indicadores para IUCR, ERC y TSA; las anclas publicadas coinciden con IUCR1, ERC2 y TSA1).
# Anclas: Tabla 1 (percentiles 5/50/95); 0,5 -> 0,51 (declarado). Frecuencia 1, consistencia 0,80, PRI 0,70.
# Solución: Tabla 3 (S1-S2 para PI alta; NS1-NS4 para ~PI).
source("replicas/csqca_comun.R")
r <- read.csv("datos/wang_2026_univ/s001.csv")
x <- data.frame(PI = r$PI, IUCR = r$IUCR1, GAT = r$GAT, HC = r$HC, ERC = r$ERC2, ISE = r$ISE, TSA = r$TSA1)
an <- list(PI = c(19.585, 26.89, 52.975), IUCR = c(0.105, 0.23, 0.415), GAT = c(5240.67, 6659, 9285),
           HC = c(6121.67, 35940.33, 92187.5), ERC = c(568574.6, 6548816, 26676411), ISE = c(243.665, 2703.33, 13922.67),
           TSA = c(3906.165, 145704, 700394.3))
d <- as.data.frame(sapply(names(an), function(z) calibrate(x[[z]], type = "fuzzy", thresholds = an[[z]])))
cat("Casos en 0,5:", sum(abs(as.matrix(d) - 0.5) < 1e-9), "\n")
d[abs(d - 0.5) < 1e-9] <- 0.51
d$nPI <- 1 - d$PI
cd <- names(an)[-1]
cat("Necesidad PI (publicado .775 .734 .924 .915 .936 .889):\n")
print(round(sapply(cd, function(z) sum(pmin(d[[z]], d$PI)) / sum(d$PI)), 3))
hi <- c("HC*ERC*ISE*TSA", "~IUCR*GAT*HC*ERC*ISE")
lo <- c("~HC*~ERC*~ISE*~TSA", "~IUCR*GAT*~HC*~ERC*~TSA", "IUCR*~GAT*~ERC*~ISE*~TSA", "IUCR*~GAT*HC*ERC*ISE*~TSA")
r1 <- replicar_cs(d[, c(cd, "PI")], "PI", hi, id = "R58 Wang 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.909, 0.982), cov_pub = c(0.808, 0.5)))
r2 <- replicar_cs(d[, c(cd, "nPI")], "nPI", lo, id = "R58 Wang 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.991, 0.995, 0.99, 0.979), cov_pub = c(0.712, 0.421, 0.343, 0.237)))
saveRDS(list(alta = r1, baja = r2), "replicas/r58_wang_2026.rds")
# CLASIFICACIÓN: aproximada (dif. máx. 0,003 consistencia, 0,006 cobertura).
