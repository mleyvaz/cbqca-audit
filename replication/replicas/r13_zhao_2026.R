# R13 Zhao et al. (2026) PLOS ONE 10.1371/journal.pone.0361767 — fsQCA, 382 participantes.
# Datos: S1 Data (Likert 1-5); constructo = media de ítems. Anclas: texto de Métodos.
# Solución base (frecuencia 1, consistencia 0,80): Tabla 6 (todas las condiciones presentes).
source("replicas/csqca_comun.R")
it <- read.csv("datos/zhao_2026/items.csv")
pre <- c(BP = "BP", RE = "REC", CO = "COU", ST = "STA", EN = "ENE", AF = "AFF", AT = "ATT", PSE = "PSE")
m <- as.data.frame(sapply(pre, function(p) rowMeans(it[, grep(paste0("^", p, "[0-9]"), names(it))])))
# Las anclas por percentiles que declara el texto (p. ej., BP 2,6/4/5) NO reproducen la Tabla 6
# (cobertura 0,38 frente a 0,79; necesidad BP 0,68 frente a 0,893). La Tabla 6 se conservó
# «unchanged» desde la versión original (S3, Tabla 5) y se reproduce con las anclas de Likert
# 1/3/5 (necesidad BP 0,893/0,833 frente a 0,893/0,831). Se usan esas anclas: discrepancia de reporte.
anc <- setNames(rep(list(c(1, 3, 5)), 8), names(pre))
d <- as.data.frame(lapply(names(anc), function(k) round(calibrate(m[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- names(anc)
cat("N =", nrow(d), "| casos en 0,5 exacto:\n"); print(sapply(d, function(z) sum(z == 0.5)))
print(round(QCA::pof(d[, 1:7], outcome = d$PSE, relation = "necessity")$incl.cov[, 1:2], 3))
terms <- c("BP*ST*EN*AT", "BP*RE*CO*ST*AT", "BP*RE*CO*AF*AT", "BP*RE*ST*AF*AT",
           "BP*CO*ST*AF*AT", "RE*CO*ST*AF*AT", "BP*RE*CO*ST*~EN*AF")
# Término 7: la Tabla 6 marca EN como presente (●), pero solo ~EN reproduce 0,967/0,451.
pub <- data.frame(term = terms, incl_pub = c(0.948, 0.932, 0.942, 0.938, 0.935, 0.941, 0.967),
                  cov_pub = c(0.714, 0.789, 0.783, 0.789, 0.801, 0.813, 0.451))
r <- replicar_cs(d, "PSE", terms, id = "R13 Zhao 2026 (alta PSE)", publicados = pub)
saveRDS(r, "replicas/r13_zhao_2026.rds")
