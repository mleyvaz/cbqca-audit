# R10 Zhang et al. (2024) PLOS ONE 10.1371/journal.pone.0304688 — fsQCA, 141 empresas SRDI.
# Datos brutos: S1 Data (CSV). Anclas: Tabla 1. Soluciones: Tablas 3 (alta) y 4 (no alta).
source("replicas/csqca_comun.R")
raw <- read.csv("datos/zhang_2024/s001.csv")
anc <- list(TFP = c(13.311, 14.061, 15.415), IC = c(0, 5, 313.8), DT = c(0, 8, 316.2),
            CSR = c(0, 23.45, 30.623), MSO = c(0, 31.109, 64.341),
            MC = c(1.415, 2.453, 3.320), BE = c(10.576, 31.755, 73.301))
d <- as.data.frame(lapply(names(anc), function(v)
  round(calibrate(raw[[v]], type = "fuzzy", thresholds = anc[[v]]), 3)))
names(d) <- names(anc)
cat("N =", nrow(d), "| casos en 0,5 exacto:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nTFP <- 1 - d$TFP
cs <- c("IC", "DT", "CSR", "MSO", "MC", "BE")
print(round(QCA::pof(d[, cs], outcome = d$TFP, relation = "necessity")$incl.cov[, 1:2], 3))
hi <- c("IC*DT*~CSR*MSO*~BE", "IC*DT*CSR*~MSO*~MC*~BE", "IC*~DT*MSO*MC*~BE", "IC*DT*CSR*MSO*~MC*BE")
# H4: la Tabla 3 omite ~MC; la Tabla 6 lo incluye y solo ese término reproduce 0,904/0,218.
# NH4 (~IC*DT*~CSR*~MSO*MC*BE) NO se reproduce (0,907/0,208 frente a 0,891/0,233; ninguna variante
# cercana coincide): se excluye del análisis principal como término no replicado.
lo <- c("~IC*~DT*~MC", "~IC*~CSR*MSO*BE", "~IC*~DT*MSO*BE")
r1 <- replicar_cs(d[, c(cs, "TFP")], "TFP", hi, id = "R10 Zhang 2024 (alta calidad)",
  publicados = data.frame(term = hi, incl_pub = c(0.939, 0.948, 0.949, 0.904),
                          cov_pub = c(0.205, 0.203, 0.224, 0.218)))
r2 <- replicar_cs(d[, c(cs, "nTFP")], "nTFP", lo, id = "R10 Zhang 2024 (no alta)",
  publicados = data.frame(term = lo, incl_pub = c(0.838, 0.852, 0.875),
                          cov_pub = c(0.528, 0.296, 0.317)))
saveRDS(list(alta = r1, no_alta = r2), "replicas/r10_zhang_2024.rds")
