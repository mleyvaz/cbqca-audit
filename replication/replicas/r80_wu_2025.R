# R80 Wu et al. (2025), accidentes de turismo deportivo en Tíbet, PLOS ONE 10.1371/journal.pone.0334226 — fsQCA, 32 accidentes.
# Datos: S5 File «The Case-by-variable Matrix» (CSV, ya calibrado por reglas de asignación, Tablas 4-5).
# Corte de frecuencia 1; consistencia 0,824 (R) y 0,856 (~R) según S6 (salida fsQCA 3.0).
# Solución intermedia: Tabla 7 (imagen; coincide con S6). Alta: H1a, H1b, H2; baja (~R): L1a-L4.
source("replicas/csqca_comun.R")
d <- read.csv("datos/r80_pone0334226/s005_journal.pone.0334226.s005.csv")
d$ID <- NULL
cat("Valores distintos por variable:\n"); print(lapply(d, function(z) sort(unique(z))))
d$nR <- 1 - d$R
cd <- c("OP", "RL", "GL", "NE", "TV", "AB", "DB")
hi <- c("~OP*~RL*NE*~TV*~AB*~DB", "~OP*~GL*NE*~TV*AB*DB", "RL*~GL*NE*~TV*AB*DB")
lo <- c("OP*~RL*~GL*NE*TV*AB", "OP*~RL*NE*TV*AB*DB", "~OP*~RL*~GL*~NE*TV*~AB*DB",
        "OP*~RL*~GL*~NE*~TV*AB*~DB", "OP*~RL*GL*~NE*TV*AB*~DB", "OP*~RL*GL*NE*~TV*~AB*DB")
r1 <- replicar_cs(d[, c(cd, "R")], "R", hi, id = "R80 Wu 2025 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.880, 0.921, 0.923), cov_pub = c(0.108, 0.225, 0.233)))
r2 <- replicar_cs(d[, c(cd, "nR")], "nR", lo, id = "R80 Wu 2025 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.899, 0.810, 0.906, 1.000, 1.000, 0.923),
                          cov_pub = c(0.399, 0.288, 0.162, 0.155, 0.133, 0.266)))
saveRDS(list(alta = r1, baja = r2), "replicas/r80_wu_2025.rds")
