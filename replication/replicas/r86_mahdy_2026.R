# R86 Mahdy et al. (2026) PLOS ONE 10.1371/journal.pone.0342645 — fsQCA, 208 expatriados (inteligencia cultural
# digital y ajuste laboral, EWA). Datos: S2 File (puntuaciones de constructos 1-7; DCT = columna TRN).
# Anclas declaradas 2/4/6 (sección 4.1). Comprobación previa: la necesidad (Tabla 2) no se reproduce
# (p. ej., METACOG 0,77 frente a 0,86 publicado) y los símbolos de la Tabla 3 (○, ◌) no están definidos;
# además el PRI publicado (~0,3) es incompatible con consistencias ~0,9. NO REPLICABLE: no se registra en el corpus.
suppressMessages(library(QCA))
r <- read.csv("datos/r86_pone0342645/datos.csv"); names(r)[names(r) == "TRN"] <- "DCT"
v <- c("METACOG", "COG", "MOT", "BEH", "POS", "IE", "LP", "DCT", "EWA")
d <- as.data.frame(lapply(r[v], function(x) calibrate(x, type = "fuzzy", thresholds = c(2, 4, 6))))
cat("Necesidad replicada (publicado: 0,86 0,83 0,84 0,88 0,87 0,79 0,83 0,85):\n")
print(round(pof(paste(v[1:8], collapse = " + "), "EWA", data = d, relation = "necessity")$incl.cov[1:8, 1:3], 3))
