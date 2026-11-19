# R89 Xu et al. (2021) PLOS ONE 10.1371/journal.pone.0259014 — fsQCA, 50 casos judiciales de violencia contra personal
# médico en China (sentencia judicial y sensación mediática). Datos: S1 Data (CSV, ya calibrados en 4 niveles
# 0/0,33/0,67/1; Perpetrator = columna Relation). Soluciones intermedias: Tabla 5 (~Judgment, A1-A13) y Tabla 6
# (Judgment, B1-B7); se usan las fórmulas de la columna «Configurations» (en A6 los círculos marcan Injury).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r89_pone0259014/s001_journal.pone.0259014.s001.csv")
d <- data.frame(Hospital = r$Hospital, Cost = r$Cost, Disease = r$Disease, Perpetrator = r$Relation,
                Weapon = r$Weapon, Injury = r$Injury, Media = r$Media, Judgment = r$Judgment)
cat("Medias (Tabla 2: 0,7536 0,4208 0,4798 0,7812 0,6134 0,5134 0,3264 0,4794):\n"); print(round(sapply(d, mean), 4))
d$nJudgment <- 1 - d$Judgment
cd <- names(d)[1:7]
lo <- c("Hospital*~Injury*~Media", "Cost*~Injury", "~Perpetrator*Injury", "Cost*~Disease*~Media", "~Cost*Weapon*Media",
        "~Perpetrator*Weapon", "~Hospital*~Cost*Injury", "~Hospital*~Disease*Weapon", "~Hospital*~Cost*Weapon",
        "~Hospital*~Disease*Injury", "~Hospital*Weapon*Media", "~Hospital*Injury*Media", "~Hospital*Cost*Media")
hi <- c("Hospital*~Cost*~Disease*Perpetrator*Weapon", "Hospital*~Disease*Perpetrator*Weapon*Injury",
        "Hospital*Cost*Disease*Weapon*Injury*~Media", "Cost*Disease*Perpetrator*Weapon*Injury*~Media",
        "Hospital*Cost*Disease*Perpetrator*Weapon*~Injury", "~Cost*Disease*Perpetrator*~Weapon*~Injury*Media",
        "~Hospital*~Cost*~Disease*~Perpetrator*~Weapon*~Injury*~Media")
r1 <- replicar_cs(d[, c(cd, "Judgment")], "Judgment", hi, id = "R89 Xu 2021 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.757, 0.873, 1, 0.946, 0.829, 1, 1),
                          cov_pub = c(0.305, 0.291, 0.250, 0.250, 0.138, 0.097, 0.055)))
r2 <- replicar_cs(d[, c(cd, "nJudgment")], "nJudgment", lo, id = "R89 Xu 2021 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.888, 0.940, 0.869, 0.900, 0.760, 0.812, 0.787, 0.901, 0.751, 0.819, 1, 1, 0.834),
                          cov_pub = c(0.511, 0.397, 0.254, 0.229, 0.204, 0.166, 0.140, 0.115, 0.115, 0.115, 0.076, 0.076, 0.064)))
saveRDS(list(alta = r1, baja = r2), "replicas/r89_xu_2021.rds")
