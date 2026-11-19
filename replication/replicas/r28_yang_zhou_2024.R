# R28 Yang, Zhou et al. (2024) PLOS ONE 10.1371/journal.pone.0301031 — fsQCA, 10 pilotos del impuesto al agua.
# Datos: S1 Data (s001.csv; UI = resultado, eficiencia en el uso del agua). Umbral 0,75, frecuencia 1.
# Solución intermedia: Tabla 5 (H1-H4, 10 condiciones). NO REPLICABLE: S1 Data no coincide con la Tabla 2
# (p. ej., Beijing C5 0,2 vs TR 0; C10 0,6 vs TS 0,2), la Tabla 3 (verdad) no corresponde a ninguno de los dos,
# y ni la necesidad (Tabla 4) ni H2-H4 cuadran (consistencia 0,67-0,89 frente a 1,00/1,00/0,83).
source("replicas/csqca_comun.R")
d <- read.csv("datos/yang_zhou_2024/s001.csv", fileEncoding = "UTF-8-BOM", check.names = FALSE)
names(d) <- trimws(names(d)); d <- d[, -1]; names(d)[1] <- "Y"
hi <- c("~WAT*~STR*~TEC*TO*TR*TF*~TC*WTR2*SE*~TS", "WAT*~STR*TEC*TO*TR*TF*TC*~WTR2*~SE*TS",
        "~WAT*~STR*~TEC*TO*TR*TF*TC*WTR2*SE*~TS", "WAT*STR*TEC*~TO*TR*TF*TC*~WTR2*~SE*TS")
r1 <- replicar_cs(d, "Y", hi, id = "R28 Yang Zhou 2024 (eficiencia hidrica)",
  publicados = data.frame(term = hi, incl_pub = c(0.92, 1.00, 1.00, 0.83), cov_pub = c(0.25, 0.22, 0.14, 0.14)))
