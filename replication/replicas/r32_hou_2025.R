# R32 Hou et al. (2025) PLOS ONE 10.1371/journal.pone.0302508 — fsQCA, 30 provincias chinas.
# Datos: S1 (s001.xlsx, hoja 汇总值): Y1 = desarrollo de alta calidad 2020; X1-X7 condiciones (media 2019).
# Anclas: percentiles 95/50/5 (texto); los valores exactos están en las filas 35-37 de la hoja.
# Solución: Tabla 5 (★ núcleo, ● periférica, ◎/☒ ausente). Alta H1-H5, no alta L1-L4.
source("replicas/csqca_comun.R")
x <- as.data.frame(readxl::read_excel("datos/r32_pone0302508/s001.xlsx", 1, col_names = FALSE))
raw <- as.data.frame(lapply(x[3:32, c(1, 3:9)], as.numeric)); names(raw) <- c("Y", paste0("X", 1:7))
anc_pub <- as.data.frame(lapply(x[35:37, c(1, 3:9)], as.numeric)); names(anc_pub) <- names(raw)
cat("Anclas de la hoja (95/50/5):\n"); print(anc_pub)
cat("Percentiles tipo 7 recalculados:\n"); print(round(sapply(raw, quantile, c(.95, .5, .05)), 4))
d <- as.data.frame(lapply(names(raw), function(k) round(calibrate(raw[[k]], type = "fuzzy",
       thresholds = rev(anc_pub[[k]])), 3)))
names(d) <- names(raw)
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
d$nY <- 1 - d$Y
cd <- paste0("X", 1:7)
hi <- c("X2*X3*~X4*X5*X6*~X7", "~X1*X2*X3*X4*X5*X6", "X1*X2*X3*X4*X6*X7",
        "X1*X2*X4*X5*X6*X7", "X1*X3*X4*X5*X6*X7")
# L4 aparece en la Tabla 5 con la MISMA fórmula que L3 pero otros valores (errata): se omite.
lo <- c("~X1*~X2*X3*~X4*~X5*~X6*~X7", "~X1*~X2*~X3*X4*~X5*~X6*~X7", "~X1*~X2*~X3*~X4*X5*~X6*~X7")
r1 <- replicar_cs(d[, c(cd, "Y")], "Y", hi, id = "R32 Hou 2025 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.977, 0.990, 1, 0.994, 0.982),
                          cov_pub = c(0.274, 0.270, 0.370, 0.381, 0.354)))
r2 <- replicar_cs(d[, c(cd, "nY")], "nY", lo, id = "R32 Hou 2025 (no alta)",
  publicados = data.frame(term = lo, incl_pub = c(0.919, 0.941, 0.909), cov_pub = c(0.297, 0.312, 0.291)))
saveRDS(list(alta = r1, baja = r2), "replicas/r32_hou_2025.rds")
