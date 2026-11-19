# R64 Liu, Wang, Li et al. (2024) PLOS ONE 10.1371/journal.pone.0315249 — fsQCA, 459 empresas (marco AMO,
# transformación digital). Datos: S1 Dataset (s001.zip, dataset.csv) con brutos y calibrados (2 decimales).
# Anclas 85/50/15 (Tabla 3); TMT binaria. Frecuencia 4, consistencia 0,8, PRI 0,6.
# Solución: Tabla 5 (H1, H2, H3a, H3b; el texto confirma H3a y H3b).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r64_pone0315249/x/dataset.csv")
d <- data.frame(HC = r$fhuman, IC = r$fexplor, TMT = r$tmt, SA = r$fabs, DE = r$flocal, GS = r$fgovern, DT = r$fdtnew)
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
# Control: recalibración con las anclas de la Tabla 3
anc <- list(HC = c(0.09, 0.18, 0.34), IC = c(3.74, 4.92, 6.17), SA = c(3.67, 3.91, 4.14),
            DE = c(0.10, 0.28, 0.77), GS = c(15, 16, 18), DT = c(0, 1.10, 2.56))
raw <- c(HC = "humancapital", IC = "lnexpor", SA = "absSA", DE = "localdigital", GS = "government", DT = "dtnew")
cat("Diferencia máx. |recalibrado - publicado|:\n")
print(sapply(names(anc), function(k) max(abs(round(calibrate(r[[raw[k]]], type = "fuzzy", thresholds = anc[[k]]), 2) - d[[k]]))))
cat("Necesidad (Tabla 4: IC 0,632/0,650; GS 0,687/0,616):\n")
print(round(QCA::pof("IC + GS", "DT", d, relation = "necessity")$incl.cov[1:2, 1:2], 3))
hi <- c("~HC*~IC*~TMT*~SA*GS", "IC*~TMT*~SA*DE*GS", "~HC*IC*TMT*~SA*DE*~GS", "~HC*IC*TMT*~SA*~DE*GS")
r1 <- replicar_cs(d, "DT", hi, id = "R64 Liu 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.804, 0.792, 0.861, 0.843),
                          cov_pub = c(0.156, 0.174, 0.038, 0.044)))
saveRDS(list(alta = r1), "replicas/r64_liu_2024.rds")
