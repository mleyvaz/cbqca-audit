# R63 Xia, Zhang, Song (2024) PLOS ONE 10.1371/journal.pone.0294662 — fsQCA, 52 agroempresas cotizadas
# (instrumentos de regulación ambiental -> innovación tecnológica, PTT).
# Datos: S1 File (s001.zip, Data.xlsx, hoja «Data», 52 casos, valores a 3 decimales; la hoja «Sheet2»
# trae 50 casos con precisión completa y omite los id 34 y 42). SSD en miles (Tabla 1: media 3107,694).
# NTL (control estatal) e ISO (ISO 14001) son binarios. Anclas: Tabla 2 (85 %/50 %/15 % declarados).
# fsQCA 3.1, consistencia 0,8, PRI 0,75, frecuencia 1. Solución intermedia: Tabla 4 y TRUTHTABLE.txt del S1.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r63_pone0294662/data.csv")
cat("Medias (Tabla 1: 0,446 0,305 0,423 2,051 3107,694 0,192 8981,346):\n"); print(round(colMeans(r[-1]), 3))
anc <- list(PTT = c(0, 0.182, 0.718), PNT = c(0.043, 0.147, 0.754), TAX = c(0.880, 1.670, 3.370),
            SSD = c(0, 646.295, 5082.100), EXP = c(2251, 4285, 17300))
cat("Percentiles 15/50/85 recalculados:\n")
print(round(t(sapply(r[names(anc)], quantile, c(0.15, 0.5, 0.85))), 3))
d <- r[, c("PTT", "PNT", "NTL", "TAX", "SSD", "ISO", "EXP")]
for (k in names(anc)) d[[k]] <- round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)
cat("Casos en 0,500:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nPTT <- 1 - d$PTT
cd <- c("PNT", "NTL", "TAX", "SSD", "ISO", "EXP")
cat("Necesidad (Tabla 3: TAX 0,657/0,628; ~ISO 0,803/0,456):\n")
print(round(QCA::pof("TAX + ~ISO", "PTT", d, relation = "necessity")$incl.cov[1:2, 1:2], 3))
hi <- c("PNT*NTL*TAX*~SSD*ISO*EXP", "PNT*~NTL*TAX*SSD*EXP", "PNT*~NTL*TAX*~SSD*~ISO*~EXP",
        "~PNT*NTL*TAX*~SSD*~ISO*EXP", "~PNT*NTL*~TAX*SSD*~ISO*EXP", "~PNT*~NTL*TAX*SSD*~ISO")
lo <- c("PNT*NTL*~SSD*~ISO", "~PNT*~TAX*~SSD*~ISO*~EXP", "PNT*NTL*~TAX*SSD*ISO*~EXP")
r1 <- replicar_cs(d[, c(cd, "PTT")], "PTT", hi, id = "R63 Xia 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.974359, 0.870897, 0.882353, 0.889313, 0.992857, 0.844706),
                          cov_pub = c(0.0318658, 0.166876, 0.132075, 0.0976939, 0.0582809, 0.150524)))
r2 <- replicar_cs(d[, c(cd, "nPTT")], "nPTT", lo, id = "R63 Xia 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.977273, 0.89881, 1),
                          cov_pub = c(0.152753, 0.214565, 0.035524)))
saveRDS(list(alta = r1, baja = r2), "replicas/r63_xia_2024.rds")
