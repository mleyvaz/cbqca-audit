# R25 Lu, Gao et al. (2025) PLOS ONE 10.1371/journal.pone.0323134 — csQCA, 20 ciudades (formalismo en rectificación ambiental).
# Datos: Tabla 3 (matriz dicotómica publicada). fsQCA 4.1, consistencia 0,8, frecuencia 1.
# Solución intermedia: Tabla 5 (configuraciones 1a-4e); consistencia 1 en todas; coberturas a 2 decimales.
source("replicas/csqca_comun.R")
d <- read.csv("datos/lu_gao_2025/tabla3.csv")[, -1]
hi <- c("ENV*DIG*~ATT*~ECO*IND", "ENV*DIG*ACC*~ATT*~ECO", "ENV*ATT*~ECO*~IND", "DIG*ATT*~ECO*~IND",
        "~ENV*~DIG*ACC*ECO*~IND", "ENV*~DIG*ACC*~ECO*IND", "~ENV*DIG*ACC*~ECO*IND",
        "ENV*DIG*ACC*ATT*~IND", "~DIG*ACC*ATT*~ECO*IND")
r1 <- replicar_cs(d, "Y", hi, id = "R25 Lu Gao 2025 (formalismo)",
  publicados = data.frame(term = hi, incl_pub = rep(1, 9),
                          cov_pub = c(0.08, 0.08, 0.15, 0.15, 0.08, 0.23, 0.15, 0.08, 0.15)))
saveRDS(list(alta = r1), "replicas/r25_lu_gao_2025.rds")
