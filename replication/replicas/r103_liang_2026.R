# R103 Liang y Song (2026) PLOS ONE 10.1371/journal.pone.0358987 — PLS-SEM + fsQCA, experiencias ritualizadas y
# autenticidad -> disposición a apoyar el patrimonio inmaterial (Shehuo, Gansu), 390 turistas.
# Datos: Figshare 33150737 (ítems + medias por constructo, 2 decimales). Anclas declaradas: percentiles 95/50/5
# (valores no publicados; se calculan con quantile tipo 7). Software no declarado -> calibrate() por defecto del
# paquete QCA = aproximada (dif. 0,007); con log-odds ±3 y 2 decimales (convención fsQCA 3, NO declarada) cuadra
# todo -> CLASIFICACIÓN: reconstruida. Análisis principal con la convención fsQCA 3. Frecuencia 1, consistencia 0,80, PRI 0,75.
# Solución: Tabla 6 (H1-H6 alta; L1 baja).
source("replicas/csqca_comun.R")
it <- read.csv("datos/r103_pone0358987/items.csv")
v <- c("unq", "com", "cer", "nf", "oa", "ca", "ea", "ii")
m <- it[, v]; names(m) <- toupper(v); cat("N =", nrow(m), "\n")
cat("Medias de ítems vs. medias publicadas (máx. dif.):",
    max(abs(sapply(v[1:7], function(k) max(abs(rowMeans(it[, grep(paste0("^", k, "[0-9]"), names(it))]) - it[[k]]))))), "\n")
anc <- t(sapply(m, quantile, c(0.05, 0.5, 0.95))); cat("Anclas (P5/P50/P95):\n"); print(round(anc, 3))
cal <- function(fs) { d <- as.data.frame(lapply(names(m), function(k) if (fs)
  round(calibrate(m[[k]], type = "fuzzy", thresholds = anc[k, ], idm = plogis(3)), 2) else
  calibrate(m[[k]], type = "fuzzy", thresholds = anc[k, ]))); names(d) <- names(m); d$nII <- 1 - d$II; d }
hi <- c("UNQ*COM*CER*NF*OA", "UNQ*COM*CER*NF*CA", "UNQ*COM*CER*NF*EA", "UNQ*COM*CER*OA*EA", "UNQ*NF*OA*CA*EA",
        "COM*CER*NF*OA*CA*EA")
lo <- "~UNQ*~COM*~CER*~NF*~OA*~CA*~EA"
d2 <- cal(FALSE); cat("Control con calibrate() por defecto de QCA (idm 0,95; dif. máx. 0,007 en cobertura):
")
for (t in hi) print(round(pof(t, "II", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
print(round(pof(lo, "nII", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
d <- cal(TRUE); cat("Casos en 0,5:", sum(d == 0.5), "\n")
cat("Necesidad (publicado UNQ 0,731/0,814; OA 0,777/0,759; EA 0,700/0,806):\n")
for (k in c("UNQ", "OA", "EA")) print(round(pof(k, "II", data = d, relation = "necessity")$incl.cov[, c(1, 3)], 3))
cd <- toupper(v[1:7])
r1 <- replicar_cs(d[, c(cd, "II")], "II", hi, id = "R103 Liang 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.968, 0.966, 0.970, 0.974, 0.971, 0.966),
                          cov_pub = c(0.419, 0.404, 0.393, 0.399, 0.398, 0.367)))
r2 <- replicar_cs(d[, c(cd, "nII")], "nII", lo, id = "R103 Liang 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = 0.951, cov_pub = 0.358))
saveRDS(list(alta = r1, baja = r2), "replicas/r103_liang_2026.rds")
