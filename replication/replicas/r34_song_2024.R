# R34 Song, Xu y Chen (2024) PLOS ONE 10.1371/journal.pone.0297587 — fsQCA, 35 casos de innovación en gobernanza de base.
# RESULTADO: NO REPLICABLE (consistencias publicadas 0,90-0,99; replicadas 0,58-1,00 con calibración logística o lineal).
# Datos: S1 (s001.xls): 4 criterios binarios de desempeño, TIA (0-100), 6 condiciones binarias, IEO (0-10).
# Codificación (Tabla 1): Y = 0 si <=1 criterio, 0,33 / 0,67 / 1 con 2 / 3 / 4; TIA anclas 0/50/100; IEO 0/5/10.
# Solución: Tabla 3 (S/X núcleo, s/x periférica), siete configuraciones del resultado alto.
source("replicas/csqca_comun.R")
x <- as.data.frame(readxl::read_excel("datos/r34_pone0297587/s001.xls", 1))
k <- rowSums(x[, 4:7])
d <- data.frame(TIA = as.numeric(x[[8]]), TFS = x[[9]], PHG = x[[10]], GAL = x[[11]], LG = x[[12]],
                IPC = x[[13]], PDP = x[[14]], IEO = as.numeric(x[[15]]))
d$Y <- c(0, 0, 0.33, 0.67, 1)[k + 1]
MODO <- get0("MODO", ifnotfound = "logistica")
if (MODO == "logistica") {
  d$TIA <- round(calibrate(d$TIA, type = "fuzzy", thresholds = c(0, 50, 100)), 3)
  d$IEO <- round(calibrate(d$IEO, type = "fuzzy", thresholds = c(0, 5, 10)), 3)
} else { d$TIA <- d$TIA / 100; d$IEO <- d$IEO / 10 }
cat("Calibración:", MODO, "\n")
hi <- c("TIA*PHG*~IPC*PDP*IEO", "TFS*GAL*~LG*~IPC*~PDP*IEO", "~TIA*TFS*~PHG*GAL*~LG*~IPC*IEO",
        "TIA*TFS*PHG*GAL*LG*~IPC*IEO", "TIA*TFS*PHG*GAL*LG*~PDP*IEO", "TIA*~TFS*PHG*~GAL*LG*~PDP*IEO",
        "TIA*~TFS*PHG*LG*IPC*~PDP*IEO")
r1 <- replicar_cs(d, "Y", hi, id = "R34 Song 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.9358, 0.9685, 0.9895, 0.9687, 0.9802, 0.9765, 0.8987),
                          cov_pub = c(0.2598, 0.1265, 0.0859, 0.0896, 0.1108, 0.0856, 0.0401)))
saveRDS(list(alta = r1), "replicas/r34_song_2024.rds")
