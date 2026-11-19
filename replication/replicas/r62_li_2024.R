# R62 Li, Zhao et al. (PLOS ONE 2024) 10.1371/journal.pone.0309784 — fsQCA, 26 empresas de manufactura inteligente (TOE).
# Datos: S1 File (s001.docx, datos brutos). Resultado = log10(solicitudes de patentes de invención)
# (Tabla 3: mín. 0,8451 = log10 7; máx. 3,3642 = log10 2313).
# Anclas: Tabla 4 (cuartiles 25/50/75 declarados). Umbral 0,8, PRI 0,75, frecuencia 1.
# Solución: Tabla 6 (3 configuraciones, alta innovación); el texto confirma los signos.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r62_pone0309784/s001.csv", colClasses = c(code = "character"))
r$PAT <- log10(r$PAT)
cons <- c("RD", "DT", "HC", "ROA", "GS", "CP", "PAT")
anc <- list(RD = c(0.0441, 0.0564, 0.0891), DT = c(2.3477, 3.1728, 3.9266), HC = c(0.308, 0.4337, 0.5411),
            ROA = c(0.0239, 0.0405, 0.0576), GS = c(0.004, 0.0081, 0.0108), CP = c(0.1091, 0.1372, 0.186),
            PAT = c(1.5401, 2.0413, 2.7862))
cat("Cuartiles recalculados (tipo 7):\n")
print(round(t(sapply(r[cons], quantile, c(0.25, 0.5, 0.75))), 4))
d <- as.data.frame(lapply(cons, function(k) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- cons
cat("Casos en 0,500:\n"); print(sapply(d, function(z) sum(z == 0.5)))
cat("Necesidad (Tabla 5 publica DT 0,427/0,410; GS 0,601/0,597):\n")
print(round(QCA::pof("DT + ~DT + GS + ~GS", "PAT", d, relation = "necessity")$incl.cov[1:4, 1:2], 3))
hi <- c("~DT*HC*~ROA*GS*~CP", "~DT*~HC*ROA*GS*CP", "~RD*~DT*~HC*ROA*~GS*~CP")
r1 <- replicar_cs(d, "PAT", hi, id = "R62 Li 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.915385, 0.958525, 0.955801),
                          cov_pub = c(0.186813, 0.163265, 0.135793)))
saveRDS(list(alta = r1), "replicas/r62_li_2024.rds")
