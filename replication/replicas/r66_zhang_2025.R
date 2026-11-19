# R66 Zhang, Yang, Liu (2025) PLOS ONE 10.1371/journal.pone.0319246 — fsQCA, 259 usuarias de plataformas de
# fitness digital (resultado: conducta de pago en línea, PB). Datos: S1 Table (s001.xls, ítems Likert 1-5);
# constructo = media de sus ítems. Anclas: Tabla 6 (declaradas; el texto dice cuartiles 25/50/75, pero los
# valores de la Tabla 6 no son los cuartiles de las medias; se usan los de la Tabla 6 tal cual).
# Frecuencia 3, consistencia 0,85. Solución intermedia: Tabla 8 (Y1-Y7, en texto). No se ajusta 0,5
# (el texto menciona restar 0,001 «a Fiss» sin precisar a qué casos).
source("replicas/csqca_comun.R")
it <- read.csv("datos/r66_pone0319246/s001.csv")
k <- list(PEU = paste0("PEU", 1:3), PU = paste0("PU", 1:3), PE = paste0("PE", 1:4), SA = paste0("SA", 1:2),
          UI = paste0("UI", 1:3), PB = paste0("PB", 1:3))
m <- as.data.frame(lapply(k, function(v) rowMeans(it[, v])))
cat("Cuartiles recalculados (Tabla 6 declara otros valores):\n")
print(round(t(sapply(m, quantile, c(0.25, 0.5, 0.75))), 3))
anc <- list(PEU = c(1.75, 3.5, 5), PU = c(2, 4, 5), PE = c(2, 3.333, 4.667), SA = c(1.45, 3.5, 5),
            UI = c(1.333, 3.667, 5), PB = c(1.97, 3.667, 4.667))
d <- as.data.frame(lapply(names(anc), function(v) round(calibrate(m[[v]], type = "fuzzy", thresholds = anc[[v]]), 3)))
names(d) <- names(anc)
cat("Casos en 0,500:\n"); print(sapply(d, function(z) sum(z == 0.5)))
cat("Necesidad (Tabla 7: PEU 0,6769; ~PEU 0,5342; PU 0,5968; ~PU 0,6345; UI 0,6301; ~UI 0,6547):\n")
print(round(QCA::pof("PEU + ~PEU + PU + ~PU + UI + ~UI", "PB", d, relation = "necessity")$incl.cov[1:6, 1:2], 4))
hi <- c("~UI*SA*~PU", "~UI*PE*~PU", "SA*PE*~PU", "UI*SA*PEU", "UI*PE*PEU", "SA*PE*PEU", "~UI*~SA*~PE*PU")
r1 <- replicar_cs(d, "PB", hi, id = "R66 Zhang 2025 (alta)",
  publicados = data.frame(term = hi,
    incl_pub = c(0.850917, 0.849400, 0.831892, 0.834810, 0.878226, 0.842742, 0.847480),
    cov_pub = c(0.379435, 0.389480, 0.362651, 0.435803, 0.456552, 0.436992, 0.256658)))
saveRDS(list(alta = r1), "replicas/r66_zhang_2025.rds")
