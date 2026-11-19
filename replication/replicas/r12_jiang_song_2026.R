# R12 Jiang & Song (2026) PLOS ONE 10.1371/journal.pone.0361505 — fsQCA, 350 encuestados.
# Datos: S1 Data (ítems Likert 1-7); constructo = media de 4 ítems (S1 Appendix).
# Anclas: percentiles 5/50/95 (S1 Appendix, Tabla A); 0,500 -> 0,501 como los autores.
# Solución intermedia: Tabla 12 (● núcleo, ○ periférica presente, ⊗/⊕ ausente).
source("replicas/csqca_comun.R")
it <- read.csv("datos/jiang_song_2026/items.csv")
cons <- c("IC", "VA", "PE", "PA", "PCV", "CI", "CCEI")
m <- as.data.frame(sapply(cons, function(k) rowMeans(it[, paste0(k, 1:4)])))
anc <- list(IC = c(2, 4, 5.75), VA = c(2.25, 4.25, 6), PE = c(2, 4, 5.75), PA = c(2.113, 4.25, 6),
            PCV = c(2.25, 4.125, 6), CI = c(2, 4, 6), CCEI = c(2.25, 4.25, 6.25))
cat("Anclas recalculadas (percentiles 5/50/95, tipo 7):\n")
print(round(t(sapply(m, quantile, c(0.05, 0.5, 0.95))), 3))
d <- as.data.frame(lapply(cons, function(k) round(calibrate(m[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- cons
cat("Casos en 0,500 (publicado: 24, 28, 26, 39, 0, 23, 22):\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
d$nCCEI <- 1 - d$CCEI
cd <- cons[1:6]
hi <- c("IC*VA*PE*PA", "IC*PA*PCV*CI", "PE*PA*PCV*CI", "IC*VA*PE*PCV*CI")
lo <- c("~PE*~PA*~PCV*~CI", "~IC*~PE*~PA*~PCV", "~IC*~VA*~PA*~CI", "~IC*~VA*~PE*~PCV*~CI")
r1 <- replicar_cs(d[, c(cd, "CCEI")], "CCEI", hi, id = "R12 Jiang y Song 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.930, 0.929, 0.923, 0.942),
                          cov_pub = c(0.586, 0.610, 0.622, 0.564)))
r2 <- replicar_cs(d[, c(cd, "nCCEI")], "nCCEI", lo, id = "R12 Jiang y Song 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.944, 0.939, 0.938, 0.952),
                          cov_pub = c(0.615, 0.622, 0.606, 0.577)))
saveRDS(list(alta = r1, baja = r2), "replicas/r12_jiang_song_2026.rds")
