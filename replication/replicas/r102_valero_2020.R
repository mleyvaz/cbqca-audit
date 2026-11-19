# R102 Valero-Moreno et al. (2020) PLOS ONE 10.1371/journal.pone.0227888 — fsQCA, bienestar psicológico en (pre)adolescentes con y sin
# discinesia ciliar primaria (DCP), N = 48. Datos: S1 Data set (variables = productos de ítems, columnas *_fs crudas).
# Anclas declaradas: percentiles 10/50/90 (Tabla 4). Software declarado fsQCA 3 -> log-odds ±3, 2 decimales.
# Solución intermedia: Tabla 6 (● presencia, ○ ausencia); valores publicados con 2 decimales.
source("replicas/csqca_comun.R")
r <- read.delim("datos/r102_pone0227888/s001.dat", fileEncoding = "UTF-8-BOM")
cat("N =", nrow(r), "\n"); print(sapply(r[, 1:3], table))
cal <- function(x, a) round(calibrate(x, type = "fuzzy", thresholds = a, idm = plogis(3)), 2)
base <- data.frame(SE = cal(r$auto_fs, c(1101.6, 62208, 786432)), ANX = cal(r$ans_fs, c(2, 28, 576)),
                   DEP = cal(r$dep_fs, c(1, 4, 33.6)), WB = cal(r$biene_fs, c(6609.6, 314928, 1594323)))
cat("Percentiles 10/50/90 (tipo 7):\n"); print(t(sapply(r[, 4:7], quantile, c(0.1, 0.5, 0.9))))
# Codificación de las dicotómicas: se elige la que reproduce la Tabla 5 (necesidad; Girl 0,44/0,40, PCD 0,27/0,45, Teen 0,55/0,45).
# Codificación elegida (única que reproduce la Tabla 5): sexofs = 1 niña, enfermedad = 1 DCP, ETAPA = 1 adolescente.
d <- cbind(GIRL = as.numeric(r$sexofs == 1), PCD = as.numeric(r$enfermedad == 1), TEEN = as.numeric(r$ETAPA == 1), base)
cat("Necesidad (publicado Girl .44/.40, Boy .56/.43, PCD .27/.45, Teen .55/.45, High SE .35/.76, High anx .55/.56, High dep .50/.46):\n")
for (k in c("GIRL", "~GIRL", "PCD", "TEEN", "SE", "ANX", "DEP")) print(round(pof(k, "WB", data = d, relation = "necessity")$incl.cov[, c(1, 3)], 3))
cat("Casos en 0,5:", sum(base == 0.5), "\n")
d$nWB <- 1 - d$WB
cd <- c("GIRL", "PCD", "TEEN", "SE", "ANX", "DEP")
hi <- c("~PCD*SE*~ANX*~DEP", "GIRL*~PCD*TEEN*~ANX*~DEP", "~GIRL*~PCD*TEEN*ANX")
lo <- c("GIRL*~SE*DEP", "~PCD*~TEEN*~SE*DEP", "~GIRL*~TEEN*~SE*~ANX")
r1 <- replicar_cs(d[, c(cd, "WB")], "WB", hi, id = "R102 Valero-Moreno 2020 (alto)",
  publicados = data.frame(term = hi, incl_pub = c(0.90, 0.84, 0.79), cov_pub = c(0.26, 0.16, 0.12)))
r2 <- replicar_cs(d[, c(cd, "nWB")], "nWB", lo, id = "R102 Valero-Moreno 2020 (bajo)",
  publicados = data.frame(term = lo, incl_pub = c(0.93, 0.88, 0.79), cov_pub = c(0.30, 0.27, 0.16)))
saveRDS(list(alto = r1, bajo = r2), "replicas/r102_valero_2020.rds")
# CLASIFICACIÓN: no replicable — la autoestima (SE) del S1 no reproduce la Tabla 5 (High SE 0,73/0,70 vs 0,35/0,76 publicados) ni sus percentiles (Tabla 4); los términos sin SE cuadran (<= 0,005), los que llevan SE no.
