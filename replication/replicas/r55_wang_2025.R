# R55 Wang et al. (2025) Scientific Reports 10.1038/s41598-024-84646-z — fsQCA, 1134 usuarios de WeChat (fatiga de privacidad).
# Datos: Supplementary Material 1 (41598_2024_84646_MOESM1_ESM.xlsx, static-content.springer.com); medias por constructo
# (columnas «average» del archivo). Anclas: Tabla 3 (percentiles 5/50/95), que coinciden con los cuantiles recalculados.
# DECISIÓN: con las anclas declaradas de CYN (1,667/3/4) la necesidad de CYN da 0,675 (publicado 0,852) y nada cuadra;
# con las anclas de SCB (1/2,5/4) aplicadas a CYN cuadran necesidad y suficiencia -> anclas NO declaradas (reconstruida).
# Consistencia 0,85, frecuencia 8. Solución intermedia: Tabla 6 (5 configuraciones).
source("replicas/csqca_comun.R")
r <- read.csv("datos/wang_2025_privacy/medias.csv")
an <- list(PF = c(1, 2.666667, 4), PCON = c(2, 3, 4.5), IO = c(2, 3, 4), PCOST = c(2.333333, 4, 5),
           SCB = c(1, 2.5, 4), CYN = c(1, 2.5, 4))   # CYN declarado: c(1.666667, 3, 4)
d <- as.data.frame(sapply(names(an), function(z) calibrate(r[[z]], type = "fuzzy", thresholds = an[[z]])))
cat("Necesidad (publicado .7279 .6521 .6931 .7435 .8521):\n")
print(round(sapply(names(an)[-1], function(z) sum(pmin(d[[z]], d$PF)) / sum(d$PF)), 4))
cd <- names(an)[-1]
hi <- c("PCON*PCOST*SCB", "PCON*SCB*CYN", "~IO*~PCOST*SCB*CYN", "PCON*~IO*PCOST*CYN", "IO*PCOST*SCB*CYN")
r1 <- replicar_cs(d[, c(cd, "PF")], "PF", hi, id = "R55 Wang 2025 (fatiga de privacidad)",
  publicados = data.frame(term = hi, incl_pub = c(0.909809, 0.895503, 0.902723, 0.901293, 0.91756),
                          cov_pub = c(0.472265, 0.545123, 0.396912, 0.40426, 0.433953)))
saveRDS(list(alta = r1), "replicas/r55_wang_2025.rds")
# CLASIFICACIÓN: reconstruida (anclas de CYN no declaradas; dif. máx. 0,003/0,005).
