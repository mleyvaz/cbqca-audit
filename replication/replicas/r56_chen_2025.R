# R56 Chen et al. (2025) Scientific Reports 10.1038/s41598-025-09608-5 — fsQCA, 222 universitarios (silencio negativo en el aula).
# Datos: Supplementary Material 1 (22 ítems; static-content.springer.com). Constructos = medias de ítems en orden
# SE 1-4, FNE 5-9, INT 10-12, VI 13-15, NVI 16-18, NS 19-22 (orden inferido; reproduce exactamente Tablas 1 y 2).
# Anclas: Tabla 2 (percentiles 10/50/90); 0,5 -> 0,499 (resta de 0,001, declarado). Consistencia 0,75, PRI 0,6.
# Solución: Tabla 4 (⬤ núcleo y ▲ periférica, ambas PRESENCIA); valores publicados con 2 decimales.
source("replicas/csqca_comun.R")
it <- read.csv("datos/chen_2025_silence/items.csv")
g <- list(SE = 1:4, FNE = 5:9, INT = 10:12, VI = 13:15, NVI = 16:18, NS = 19:22)
m <- as.data.frame(sapply(g, function(k) rowMeans(it[, k])))
an <- list(SE = c(1.75, 2.75, 3.5), FNE = c(1.8, 2.8, 4), INT = c(1.33, 2, 3), VI = c(1.67, 2.67, 3.33),
           NVI = c(1.33, 2.33, 3), NS = c(2, 3, 3.75))
d <- as.data.frame(sapply(names(an), function(z) calibrate(m[[z]], type = "fuzzy", thresholds = an[[z]])))
d[abs(d - 0.5) < 1e-9] <- 0.499
cat("Necesidad (publicado .68 .68 .76 .70 .65):\n")
print(round(sapply(names(an)[-6], function(z) sum(pmin(d[[z]], d$NS)) / sum(d$NS)), 3))
cd <- names(an)[-6]
hi <- c("SE*FNE*VI", "SE*FNE*NVI", "SE*INT*VI*NVI")
r1 <- replicar_cs(d[, c(cd, "NS")], "NS", hi, id = "R56 Chen 2025 (silencio negativo)",
  publicados = data.frame(term = hi, incl_pub = c(0.86, 0.87, 0.89), cov_pub = c(0.43, 0.43, 0.37)))
saveRDS(list(alta = r1), "replicas/r56_chen_2025.rds")
# CLASIFICACIÓN: aproximada (dif. máx. 0,006 consistencia, 0,007 cobertura; publicados con 2 decimales).
