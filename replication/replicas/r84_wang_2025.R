# R84 Wang et al. (2025) PLOS ONE 10.1371/journal.pone.0318563 — fsQCA, 31 provincias (transferencia
# tecnológica universitaria). Datos: S1 File (brutos). Anclas: Tabla 4 (percentiles 95/50/5).
# Solución intermedia: Tabla 6 y fórmulas del texto (C1-C4); ● núcleo, · periférica presente, ο ausente.
# Software no declarado. Réplica principal al estilo fsQCA (±3 log-odds, 2 decimales): cuadra con dif. <= 0,003;
# con QCA por defecto la cobertura de C3 difiere 0,007. Mover 0,5 a 0,501/0,499 no cambia nada.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r84_pone0318563/s001_journal.pone.0318563.s001.csv")
names(r)[names(r) == "Transfer"] <- "UTT"
anc <- list(UTT = c(1001, 59442, 513825), FTS = c(51, 942, 3446.5), EXP = c(28329, 388555, 2680043.5),
            PAT = c(79, 2372, 12860.5), INS = c(43.5, 384, 888), ORI = c(0.117, 0.211, 0.325),
            IUR = c(0.066, 0.211, 0.325), TMT = c(128374.5, 3797786, 26775305))
cat("Percentiles 5/50/95 (tipo 7):\n"); print(signif(t(sapply(r[names(anc)], quantile, c(0.05, 0.5, 0.95))), 6))
cal <- function(fs) { d <- as.data.frame(lapply(names(anc), function(k) {
  x <- if (fs) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2)
       else calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]); x }))
  names(d) <- names(anc); d }
hi <- c("FTS*EXP*PAT*INS*TMT", "~FTS*~EXP*INS*ORI*~IUR*TMT", "FTS*~EXP*~PAT*~INS*ORI*IUR*~TMT",
        "FTS*EXP*PAT*~INS*ORI*IUR*~TMT")
pub <- data.frame(term = hi, incl_pub = c(0.976, 0.953, 0.965, 0.992), cov_pub = c(0.672, 0.256, 0.268, 0.254))
cd <- c("FTS", "EXP", "PAT", "INS", "ORI", "IUR", "TMT")
d2 <- cal(TRUE); cat("Control estilo fsQCA:\n")
print(round(pof(paste(hi, collapse = "+"), "UTT", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
d <- cal(TRUE)  # estilo fsQCA (no declarado) -> categoría «reconstruida»
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(abs(z - 0.5) < 1e-9)))
r1 <- replicar_cs(d[, c(cd, "UTT")], "UTT", hi, id = "R84 Wang 2025 (alta)", publicados = pub)
saveRDS(list(alta = r1), "replicas/r84_wang_2025.rds")
