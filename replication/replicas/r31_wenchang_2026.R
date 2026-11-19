# R31 Wenchang, Xuanhui et al. (2026) PLOS ONE 10.1371/journal.pone.0340674 — fsQCA, 273 usuarios (ansiedad informativa en salud).
# Datos: S1 Data (s001.xlsx), ya calibrados por los autores (anclas máx./media/mín., Tabla 4).
# fsQCA 3.0, frecuencia 4, consistencia 0,8. Soluciones: Tabla 6 (HIA, 6 configuraciones) y Tabla 7 (~HIA, 2),
# leídas de las imágenes de las tablas (los símbolos del XML son gráficos en línea).
source("replicas/csqca_comun.R")
d <- read.csv("datos/wenchang_2026/s001.csv")
d$nHIA <- 1 - d$HIA
cd <- c("SE", "HIL", "IV", "IO", "IA", "TS", "ES", "CE", "GRS")
hi <- c("SE*HIL*IV*~IA*TS*ES*CE*GRS", "SE*HIL*IV*~IO*TS*ES*CE*GRS", "SE*IV*IO*IA*TS*ES*CE*~GRS",
        "SE*HIL*IV*IO*IA*~TS*ES*CE*GRS", "SE*HIL*IV*IO*IA*TS*ES*~CE*GRS", "~SE*~HIL*~IV*IO*IA*TS*ES*CE*~GRS")
lo <- c("SE*HIL*~IO*~IA*~TS*~ES*~CE*GRS", "~SE*~HIL*~IV*IO*IA*~TS*~ES*~CE*~GRS")
r1 <- replicar_cs(d[, c(cd, "HIA")], "HIA", hi, id = "R31 Wenchang 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.9465, 0.9528, 0.9674, 0.9706, 0.9764, 0.9738),
                          cov_pub = c(0.3749, 0.3677, 0.3120, 0.3117, 0.2932, 0.2728)))
r2 <- replicar_cs(d[, c(cd, "nHIA")], "nHIA", lo, id = "R31 Wenchang 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.9245, 0.9445), cov_pub = c(0.3850, 0.3176)))
saveRDS(list(alta = r1, baja = r2), "replicas/r31_wenchang_2026.rds")
