# R94 Mo et al. (2026) PLOS ONE 10.1371/journal.pone.0361990 — fsQCA, 526 estudiantes (turismo «estilo fuerzas
# especiales»; conducta autodeclarada, BEH). Datos: Figshare 33440560, puntuaciones latentes estandarizadas
# (SFT_Standardized_Latent_Variable_Scores_fsQCA_526.xlsx). Anclas: Tabla 5 (percentiles 5/50/95, 6 decimales).
# Software declarado fsQCA 3.0 -> calibración con log-odds ±3 (idm = plogis(3)) y 2 decimales.
# Solución intermedia: Tabla 7 (HB1-HB2 alta; LB1-LB8 baja).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r94_pone0361990/scores.csv")
anc <- list(ATT = c(-1.507784, 0.098086, 1.413390), INT = c(-1.715293, 0.087582, 1.391414),
            BEH = c(-1.722260, 0.095909, 1.673270), PCA = c(-1.674220, 0.080568, 1.593948),
            PCT = c(-1.456064, -0.001564, 1.439105), PBC = c(-1.685244, 0.063831, 1.606070),
            SVPUI = c(-1.714191, 0.077704, 1.387602))
cat("Percentiles 5/50/95 (tipo 7):\n"); print(round(t(sapply(r[names(anc)], quantile, c(0.05, 0.5, 0.95))), 6))
cal <- function(fs) { d <- as.data.frame(lapply(names(anc), function(k)
  if (fs) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2)
  else calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]))); names(d) <- names(anc); d$nBEH <- 1 - d$BEH; d }
hi <- c("ATT*INT*PCT*PBC*SVPUI", "INT*PCA*PCT*PBC*SVPUI")
lo <- c("~INT*~PCT*~PBC", "~PCA*~PCT*~PBC*~SVPUI", "~INT*~PCA*~PCT*~SVPUI", "~ATT*~PCT*~PBC*~SVPUI",
        "~ATT*~INT*~PCT*~SVPUI", "~ATT*INT*~PCA*~PCT", "ATT*~INT*~PCA*~PBC*~SVPUI", "~ATT*~INT*~PCA*PBC*~SVPUI")
d2 <- cal(FALSE); cat("Control con QCA por defecto (idm 0,95):\n")
print(round(pof(paste(hi, collapse = "+"), "BEH", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
print(round(pof(paste(lo, collapse = "+"), "nBEH", data = d2, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
d <- cal(TRUE); cat("Casos en 0,5:", sum(d == 0.5), "\n")
cd <- c("ATT", "INT", "PCA", "PCT", "PBC", "SVPUI")
r1 <- replicar_cs(d[, c(cd, "BEH")], "BEH", hi, id = "R94 Mo 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.903, 0.907), cov_pub = c(0.483, 0.487)))
r2 <- replicar_cs(d[, c(cd, "nBEH")], "nBEH", lo, id = "R94 Mo 2026 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.877, 0.918, 0.908, 0.900, 0.894, 0.924, 0.948, 0.942),
                          cov_pub = c(0.530, 0.460, 0.458, 0.457, 0.459, 0.324, 0.300, 0.306)))
saveRDS(list(alta = r1, baja = r2), "replicas/r94_mo_2026.rds")
