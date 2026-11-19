# R82 Qu et al. (2026) PLOS ONE 10.1371/journal.pone.0336145 — fsQCA, 31 provincias chinas
# (comunidades de integración industria-educación, MIEC). Datos: S1 File (datos brutos por provincia).
# Calibración como el software declarado (fsQCA 3.1): log-odds ±3 en las anclas (idm = plogis(3)) y
# pertenencias a 2 decimales; con ello el PRI de H3 da 0,792, como dice el texto (con QCA por defecto, 0,779).
# Anclas: Tabla 1 (percentiles 85/50/15 según el texto); 0,5 -> 0,499 como declaran los autores (sección 4.1).
# Anclas: (no pertenencia / cruce / pertenencia plena). Soluciones: Tablas 3 (alta) y 4 (no alta).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r82_pone0336145/datos.csv")
anc <- list(MIEC = c(0, 2.391, 4.326), P = c(17.095, 30.364, 50.132), U = c(169.829, 210.930, 275.172),
            E = c(146.286, 202.218, 317.458), IE = c(60.414, 66.860, 76.464), InstE = c(5.312, 7.940, 9.806),
            DE = c(41.541, 59.818, 194.844), IEI = c(38.460, 43.370, 52.562))
cat("Percentiles 5/50/95 (tipo 7) para comparar con las anclas:\n")
print(round(t(sapply(r[names(anc)], quantile, c(0.05, 0.5, 0.95))), 3))
d <- as.data.frame(lapply(names(anc), function(k) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2)))
names(d) <- names(anc)
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(abs(z - 0.5) < 1e-9)))
d[abs(d - 0.5) < 1e-9] <- 0.499
d$nMIEC <- 1 - d$MIEC
cd <- c("P", "U", "E", "IE", "InstE", "DE", "IEI")
hi <- c("P*~U*E*IE*InstE*~DE", "P*~U*E*IE*InstE*IEI", "P*U*E*InstE*DE*IEI", "P*E*IE*InstE*DE*IEI",
        "~P*U*E*IE*DE*IEI", "P*U*~E*~IE*~InstE*~DE*~IEI")
lo <- c("P*~U*~IE*~InstE*DE*~IEI", "~P*~U*~E*~InstE*DE*~IEI", "~P*U*~E*~IE*~InstE*~IEI", "~P*~U*E*IE*InstE*~DE*~IEI")
r1 <- replicar_cs(d[, c(cd, "MIEC")], "MIEC", hi, id = "R82 Qu 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.981, 0.980, 0.986, 0.987, 0.954, 0.908),
                          cov_pub = c(0.273, 0.290, 0.283, 0.300, 0.245, 0.214)))
r2 <- replicar_cs(d[, c(cd, "nMIEC")], "nMIEC", lo, id = "R82 Qu 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.961, 0.974, 0.916, 0.940),
                          cov_pub = c(0.203, 0.215, 0.230, 0.149)))
saveRDS(list(alta = r1, baja = r2), "replicas/r82_qu_2026.rds")
