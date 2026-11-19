# R96 Li (2026) Front Public Health 10.3389/fpubh.2026.1947309 (PMC13687033) — fsQCA, 52 países africanos
# (capacidad partidaria, capacidad estatal y mortalidad temprana por COVID-19). Datos: Supplementary Table 1
# (hoja «Processed Data»: pertenencias calibradas; Y = 1 - BAD). Solución: Tabla 4 (A: PC*PR; B: AE*~HC;
# C: TC*~HC), resultado Y (baja mortalidad temprana). Software declarado: fsQCA 4.1.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r96_PMC13687033/processed.csv")
d <- r[, c("PC", "AE", "TC", "HC", "PR", "Y")]
cat("N =", nrow(d), "; casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
cat("Necesidad (Tabla 2: PC 0,766/0,641; AE 0,575/0,713; TC 0,702/0,706; HC 0,507/0,587; PR 0,415/0,881):\n")
print(round(pof("PC + AE + TC + HC + PR", "Y", data = d, relation = "necessity")$incl.cov[1:5, c(1, 3)], 3))
hi <- c("PC*PR", "AE*~HC", "TC*~HC")
r1 <- replicar_cs(d, "Y", hi, id = "R96 Li 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.942, 0.896, 0.859), cov_pub = c(0.357, 0.463, 0.541)))
saveRDS(list(alta = r1), "replicas/r96_li_2026.rds")
