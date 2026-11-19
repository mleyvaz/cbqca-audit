# R30 Chen, Li et al. (2024) PLOS ONE 10.1371/journal.pone.0293763 — fsQCA (+NCA), 30 provincias chinas, 2019.
# Datos: S1 Data (crudos). Calibración declarada: percentiles 5/50/95 (resultado invertido: intensidad de carbono,
# 5 % = pertenencia plena). Se usan los percentiles (tipo 7) recalculados de S1 Data: coinciden con la Tabla 1 en
# todas las variables salvo economic development (Tabla 1: 0,364/1,572/5,067; datos: 0,164/1,577/5,291).
# Con las anclas literales de la Tabla 1 la diferencia máxima sube a 0,023 (cobertura de S3).
# Solución: Fig 3 (S1-S3, Q1-Q2 para alto desempeño; L1-L2 para no alto). El texto da S2 = 0,986 y Q1 = 0,931
# como consistencias, que contradicen la figura (0,963 y 0,995); se usa la figura.
source("replicas/csqca_comun.R")
r <- read.csv("datos/chen_li_2024/s001.csv")
q <- function(x) quantile(x, c(0.05, 0.5, 0.95), type = 7)
d <- as.data.frame(lapply(names(r)[-1], function(k) {
  a <- q(r[[k]]); if (k == "CEP") a <- rev(a)
  calibrate(r[[k]], type = "fuzzy", thresholds = a) }))
names(d) <- names(r)[-1]; d$nCEP <- 1 - d$CEP
cd <- c("EC", "IS", "TI", "GS", "ED", "DF")
hi <- c("~EC*IS*TI*GS*ED", "IS*TI*GS*ED*DF", "~EC*IS*~TI*~GS*ED*~DF", "~EC*~IS*~TI*GS*~ED*DF", "~EC*~IS*TI*ED*DF")
lo <- c("EC*~TI*~GS*~ED*~DF", "EC*~IS*~TI*~GS*~ED")
r1 <- replicar_cs(d[, c(cd, "CEP")], "CEP", hi, id = "R30 Chen Li 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.986585, 0.963014, 0.979499, 0.995261, 0.976526),
                          cov_pub = c(0.466014, 0.404954, 0.247696, 0.241936, 0.359447)))
r2 <- replicar_cs(d[, c(cd, "nCEP")], "nCEP", lo, id = "R30 Chen Li 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.810185, 0.822034), cov_pub = c(0.553797, 0.613924)))
saveRDS(list(alta = r1, baja = r2), "replicas/r30_chen_li_2024.rds")
