# R100 Chen y Ye (2023) PLOS ONE 10.1371/journal.pone.0291870 — PLS-SEM + fsQCA, intención de compra de ropa inteligente,
# 225 encuestados. Datos: S1 File (CSV, ítems Likert 1-5, 5 ítems por constructo); constructo = media de ítems.
# Anclas declaradas (Tabla 8): 5,0 / 3,5 / 1,0. Software declarado fsQCA 3 -> log-odds ±3 (idm = plogis(3)), 2 decimales.
# Solución: Tabla 10 (● presente, «Y» = ausente según la nota), umbral de frecuencia 3.
source("replicas/csqca_comun.R")
it <- read.csv("datos/r100_pone0291870/s001.csv", fileEncoding = "UTF-8-BOM")
cons <- c("FUN", "AES", "EXP", "PU", "PEOU", "ATT", "PI")
m <- as.data.frame(sapply(cons, function(k) rowMeans(it[, grep(paste0("^", k), names(it))])))
cat("N =", nrow(m), "\n")
d <- as.data.frame(lapply(m, function(z) round(calibrate(z, type = "fuzzy", thresholds = c(1, 3.5, 5), idm = plogis(3)), 2)))
cat("Descriptivos calibrados (publicado: medias 0,69 0,67 0,55 0,61 0,61 0,65 0,59):\n"); print(round(sapply(d, mean), 2))
cat("Casos en 0,5:", sum(d == 0.5), "\n")
cat("Necesidad (publicado FUN 0,910/0,777; ATT 0,952/0,860):\n")
for (k in c("FUN", "~FUN", "ATT", "~ATT")) print(round(pof(k, "PI", data = d, relation = "necessity")$incl.cov[, c(1, 3)], 3))
hi <- c("~EXP*PU*ATT", "FUN*AES*PU*ATT", "FUN*PU*PEOU*ATT", "AES*PU*PEOU*ATT", "~FUN*~EXP*AES*PEOU*ATT", "FUN*EXP*AES*PEOU*ATT")
r1 <- replicar_cs(d, "PI", hi, id = "R100 Chen 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.959, 0.925, 0.934, 0.937, 0.989, 0.947),
                          cov_pub = c(0.542, 0.795, 0.794, 0.788, 0.386, 0.739)))
saveRDS(list(alta = r1), "replicas/r100_chen_2023.rds")
# CLASIFICACIÓN: exacta (dif. máx. 0,003 consistencia, 0,000 cobertura; 6/6 términos).
