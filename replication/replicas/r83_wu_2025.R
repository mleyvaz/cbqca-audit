# R83 Wu et al. (2025) PLOS ONE 10.1371/journal.pone.0331007 — fsQCA, 31 provincias (colaboración tripartita
# gobierno-plataforma-público). Datos: S1 Data (brutos). Anclas: Tabla 3 (percentiles 95/50/5).
# S2 File = salida de fsQCA 3.0 de los autores: muestra pertenencias a 2 decimales y 0,5 -> 0,501.
# Calibración como fsQCA 3.0 (log-odds ±3 en las anclas: idm = plogis(3) = 0,9526), software declarado.
# Solución intermedia: Tabla 6 (H1-H5, L1-L2; O/X = periférica presente/ausente, •/⊗ = núcleo).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r83_pone0331007/s001_journal.pone.0331007.s001.csv")
anc <- list(TCE = c(62, 147, 175), PS = c(12, 58, 87), SL = c(37, 66, 86), PP = c(12, 67, 92),
            RC = c(25, 64, 86), SI = c(38, 67, 73))
cat("Percentiles 5/50/95 (tipo 7):\n"); print(round(t(sapply(r[names(anc)], quantile, c(0.05, 0.5, 0.95))), 2))
d <- as.data.frame(lapply(names(anc), function(k) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2)))
names(d) <- names(anc)
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
d$nTCE <- 1 - d$TCE
cd <- c("PS", "SL", "PP", "RC", "SI")
hi <- c("PS*PP*RC*SI", "PS*SL*RC*SI", "~PS*SL*PP*RC", "~PS*~SL*PP*~RC", "~PS*~SL*~RC*SI")
lo <- c("~PS*~PP*~RC*~SI", "PS*SL*~PP*RC*~SI")
# Tabla 6 da 0,17 de cobertura bruta para H5; la salida S2 de los autores da 0,516892 (errata de transcripción).
r1 <- replicar_cs(d[, c(cd, "TCE")], "TCE", hi, id = "R83 Wu 2025 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.957, 0.912, 0.976, 0.995, 0.965),
                          cov_pub = c(0.802, 0.786, 0.510, 0.497, 0.517)))
r2 <- replicar_cs(d[, c(cd, "nTCE")], "nTCE", lo, id = "R83 Wu 2025 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.951, 0.940), cov_pub = c(0.735, 0.452)))
saveRDS(list(alta = r1, baja = r2), "replicas/r83_wu_2025.rds")
