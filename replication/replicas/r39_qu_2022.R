# R39 Qu et al. (2022) Frontiers in Psychology 10.3389/fpsyg.2022.1068267 — fsQCA, 35 científicos (éxito profesional).
# Datos: Data_Sheet_1.CSV (Europe PMC, supplementaryFiles): puntajes suma de escalas (brutos).
# Anclas: Tabla 3 = mínimo, media y máximo (el texto lo declara; las columnas de la tabla están rotuladas al revés).
# Solución: Tabla 6 (● núcleo, • periférica, ⊗ ausente). Alta H1-H4; no alta NH1-NH2.
source("replicas/csqca_comun.R")
x <- read.csv("datos/qu_2022/supp/Data_Sheet_1.CSV")
raw <- data.frame(HC = x[[2]], PC = x[[3]], MI = x[[4]], TE = x[[5]], MA = x[[6]], CS = x[[7]])
cat("Mín / media / máx recalculados:\n"); print(round(sapply(raw, function(z) c(min(z), mean(z), max(z))), 2))
anc <- list(HC = c(60, 81.63, 105), PC = c(62, 87.63, 111), MI = c(51, 76.51, 105), TE = c(54, 87.71, 109),
            MA = c(1, 3.54, 5), CS = c(26, 38.91, 50))
v <- names(raw)
d <- as.data.frame(lapply(v, function(k) round(calibrate(raw[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- v
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
d$nCS <- 1 - d$CS
cd <- v[1:5]
hi <- c("HC*PC*MI*MA", "HC*PC*TE*MA", "HC*MI*TE*MA", "PC*MI*TE*MA")
lo <- c("~HC*~PC*~MI", "~PC*~MI*~TE*~MA")
r1 <- replicar_cs(d[, c(cd, "CS")], "CS", hi, id = "R39 Qu 2022 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.953, 0.929, 0.955, 0.950), cov_pub = c(0.595, 0.620, 0.598, 0.628)))
r2 <- replicar_cs(d[, c(cd, "nCS")], "nCS", lo, id = "R39 Qu 2022 (no alta)",
  publicados = data.frame(term = lo, incl_pub = c(0.890, 0.926), cov_pub = c(0.656, 0.565)))
saveRDS(list(alta = r1, baja = r2), "replicas/r39_qu_2022.rds")
