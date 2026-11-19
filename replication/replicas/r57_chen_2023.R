# R57 Chen et al. (2023) PLOS ONE 10.1371/journal.pone.0289160 — fsQCA, 30 ciudades piloto bajas en carbono.
# Datos: S1 Data (s001.xlsx). Las cabeceras del archivo nombran OTRAS variables (entorno de negocios, mercado...),
# pero media/sd/mín./máx. de cada columna, en orden, coinciden con la Tabla 8 -> ALINEACIÓN por posición:
# col1 GD (desarrollo verde), col2 ER (dotación ecológica), col3 DE (economía digital), col4 LC (economía baja en carbono),
# col5 PO (importancia de la organización superior), col6 GT (transparencia/interacción del gobierno).
# Anclas: Tabla 8 (percentiles 5/50/95); pertenencias redondeadas a 2 decimales (sin redondeo la dif. máx. es 0,0051). Consistencia 0,8, PRI 0,70, frecuencia 1. Solución: Tabla 12.
source("replicas/csqca_comun.R")
r <- read.csv("datos/chen_2023_lowcarbon/s001.csv", check.names = FALSE)
names(r) <- c("city", "LCP", "GD", "ER", "DE", "LC", "PO", "GT")
cat("Descriptivos (Tabla 8):\n"); print(round(sapply(r[, -1], function(x) c(mean(x), sd(x), min(x), max(x))), 3))
an <- list(LCP = c(0.349, 0.612, 0.739), GD = c(0.271, 0.355, 0.606), ER = c(0.579, 0.858, 1), DE = c(0.353, 0.482, 0.715),
           LC = c(0.056, 0.195, 0.744), PO = c(0.484, 0.653, 0.765), GT = c(0.537, 0.740, 0.809))
d <- as.data.frame(sapply(names(an), function(z) round(calibrate(r[[z]], type = "fuzzy", thresholds = an[[z]]), 2)))
d$nLCP <- 1 - d$LCP
cd <- c("GD", "ER", "DE", "LC", "PO", "GT")
cat("Necesidad (publicado .793 .724 .636 .705 .833 .638):\n")
print(round(sapply(cd, function(z) sum(pmin(d[[z]], d$LCP)) / sum(d$LCP)), 3))
hi <- c("GD*DE*LC*PO*~GT", "GD*ER*DE*LC*PO", "GD*~ER*~DE*~LC*PO*~GT")
lo <- c("~GD*~ER*~DE*~LC*~PO*~GT", "~GD*ER*DE*~LC*~PO*GT", "~GD*ER*DE*LC*PO*GT")
r1 <- replicar_cs(d[, c(cd, "LCP")], "LCP", hi, id = "R57 Chen 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.956, 0.916, 0.95), cov_pub = c(0.312, 0.437, 0.293)))
r2 <- replicar_cs(d[, c(cd, "nLCP")], "nLCP", lo, id = "R57 Chen 2023 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.934, 0.967, 0.963), cov_pub = c(0.379, 0.4, 0.43)))
saveRDS(list(alta = r1, baja = r2), "replicas/r57_chen_2023.rds")
# CLASIFICACIÓN: reconstruida (alineación de columnas por descriptivos + redondeo a 2 decimales; dif. máx. 0,004/0,004).
