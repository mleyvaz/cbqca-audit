# R09 Shi et al. (2025) PLOS ONE 10.1371/journal.pone.0323130 — fsQCA, 217 farmacéuticas chinas.
# Datos brutos: S1 (Sheet1). Anclas: Tabla 2 (pertenencia plena, cruce, no pertenencia).
# Solución: Tabla 4 (configuraciones para alta transformación digital; ● presente, ⨂ ausente).
source("replicas/csqca_comun.R")
raw <- read.csv("datos/shi_2025/shi_raw.csv")
anc <- list(DT = c(2.532, 3.989, 5.220), RD = c(1.748, 5.610, 25.062),
            RDP = c(4.088, 15.060, 33.392), SIZE = c(20.609, 22.058, 23.863),
            MRP = c(0.001, 0.046, 0.313), DFI = c(5.505, 5.681, 5.771),
            GST = c(0.016, 0.058, 0.130))
d <- as.data.frame(lapply(names(anc), function(v)
  round(calibrate(raw[[v]], type = "fuzzy", thresholds = anc[[v]]), 3)))
names(d) <- names(anc)
cat("Casos en 0,5 exacto por variable:\n"); print(sapply(d, function(z) sum(z == 0.5)))
terms <- c("SIZE*MRP*~DFI*~GST", "~RD*SIZE*MRP*~DFI", "~RD*~RDP*SIZE*GST",
           "~RDP*SIZE*~MRP*DFI*GST", "~RD*RDP*SIZE*MRP*~GST")
pub <- data.frame(term = terms, incl_pub = c(0.9118, 0.9144, 0.8745, 0.8871, 0.8873),
                  cov_pub = c(0.3018, 0.2961, 0.3895, 0.3260, 0.2479))
# Comprobación con la Tabla 3 (necesidad) para validar la calibración
nec <- QCA::pof(d[, -1], outcome = d$DT, relation = "necessity")$incl.cov
print(round(nec[, 1:2], 3))
r <- replicar_cs(d, "DT", terms, id = "R09 Shi 2025 (alta DT)", publicados = pub)
saveRDS(r, "replicas/r09_shi_2025.rds")
