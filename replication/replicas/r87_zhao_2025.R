# R87 Zhao et al. (2025) PLOS ONE 10.1371/journal.pone.0326226 — fsQCA, 267 agricultores (gestión del servicio
# de polinización). Software declarado: fsQCA 3.0 -> calibración con log-odds ±3 (idm = plogis(3)) y 2 decimales.
# Datos: S3 Data (brutos). Anclas: Tabla 4. Soluciones: Tabla 7 (alta CPSM1-3; baja CPSM4-5).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r87_pone0326226/datos.csv")
anc <- list(ICSM = c(28.16, 53.25, 70.33), AT = c(4.56, 11.01, 15.73), SN = c(3.02, 6.92, 14.39),
            PBC = c(9.29, 13.92, 13.93), EI = c(392.33, 2986.73, 14007.67))
cat("Percentiles 5/50/95 (tipo 7):\n"); print(round(t(sapply(r[names(anc)], quantile, c(0.05, 0.5, 0.95))), 2))
cal <- function(fs) { d <- as.data.frame(lapply(names(anc), function(k)
  if (fs) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2)
  else calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]))); names(d) <- names(anc); d }
cat("Descriptivos publicados de los conjuntos (media/DE): ICSM 0,51/0,29; AT 0,51/0,33; SN 0,46/0,35; PBC 0,79/0,32; EI 0,45/0,31\n")
for (fs in c(FALSE, TRUE)) { x <- cal(fs); print(round(rbind(media = sapply(x, mean), de = sapply(x, sd)), 2)) }
hi <- c("AT*~SN*PBC", "AT*~SN*EI", "~SN*PBC*EI"); lo <- c("~AT*~PBC", "~AT*SN*~EI")
d2 <- cal(TRUE); d2$nICSM <- 1 - d2$ICSM; cat("Control estilo fsQCA:\n")
print(round(pof(paste(hi, collapse = "+"), "ICSM", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
print(round(pof(paste(lo, collapse = "+"), "nICSM", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
d <- cal(TRUE); d$nICSM <- 1 - d$ICSM
cd <- c("AT", "SN", "PBC", "EI")
r1 <- replicar_cs(d[, c(cd, "ICSM")], "ICSM", hi, id = "R87 Zhao 2025 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.832, 0.893, 0.831), cov_pub = c(0.394, 0.324, 0.419)))
r2 <- replicar_cs(d[, c(cd, "nICSM")], "nICSM", lo, id = "R87 Zhao 2025 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.879, 0.902), cov_pub = c(0.257, 0.334)))
saveRDS(list(alta = r1, baja = r2), "replicas/r87_zhao_2025.rds")
