# R29 Xie, Hu et al. (2024) PLOS ONE 10.1371/journal.pone.0300655 — fsQCA, 19 puertos chinos (eficiencia técnica, DEA).
# Datos: S1 Data, hoja «Table 3» (crudos). Escalas: Openness/1e6, Hinterland/10, Informationization/10
# (así cuadran media/DE/máx de la Tabla 3). Anclas de la Tabla 3 (incl., cruce, excl.). Redondeo a 2 decimales
# (no declarado; es la variante que mejor cuadra). Solución intermedia y ~TE: Tabla 5.
# NO REPLICABLE: H1a, H1b y H3 cuadran (≤0,001) pero H2 (cobertura −0,009), H4 (consistencia −0,135) y las dos
# configuraciones de ~TE (−0,05 en consistencia, −0,02 en cobertura) no; ninguna combinación de signos ni de
# redondeo/0,501/TE crudo las reproduce. La necesidad (Tabla 4) también difiere hasta 0,007.
source("replicas/csqca_comun.R")
r <- read.csv("datos/xie_hu_2024/tabla3_raw.csv")
r$OPE <- r$OPE / 1e6; r$HIN <- r$HIN / 10; r$INFO <- r$INFO / 10
anc <- list(INF = c(2.32, 2.58, 3.01), PER = c(2.365, 3.63, 6.67), STR = c(45.47, 56.07, 73.88),
            HIN = c(126.25, 1002, 2877.33), GOV = c(4.23, 6.92, 12.46), OPE = c(4.86, 55.61, 446.94),
            INFO = c(7.04, 37, 68.93), TE = c(0.78, 0.95, 1))
d <- as.data.frame(lapply(names(anc), function(k) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]), 2)))
names(d) <- names(anc); d$nTE <- 1 - d$TE
cd <- names(anc)[1:7]
hi <- c("INF*PER*STR*HIN*OPE*INFO", "INF*PER*HIN*GOV*OPE*INFO", "INF*PER*~STR*HIN*GOV*~OPE*INFO",
        "~INF*~PER*HIN*~GOV*OPE*INFO", "~INF*~PER*~STR*~HIN*GOV*OPE*~INFO")
lo <- c("~INF*PER*~STR*~HIN*~GOV*~OPE*~INFO", "INF*~PER*STR*~HIN*~GOV*OPE*~INFO")
r1 <- replicar_cs(d[, c(cd, "TE")], "TE", hi, id = "R29 Xie Hu 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.9979, 0.9971, 0.9872, 0.9392, 0.9244),
                          cov_pub = c(0.3566, 0.2607, 0.1739, 0.1850, 0.1648)))
r2 <- replicar_cs(d[, c(cd, "nTE")], "nTE", lo, id = "R29 Xie Hu 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.8986, 0.8807), cov_pub = c(0.3934, 0.3363)))
