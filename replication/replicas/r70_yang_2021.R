# R70 Yang, Bai, Yang (2021) Frontiers in Psychology 10.3389/fpsyg.2021.703678 — fsQCA, 64 empresas cotizadas
# (personalidad proactiva del CEO y fusiones). Resultado MEG = 1 fusión entre industrias; ~MEG = intraindustria.
# Datos: Table_1.XLSX (suplemento vía Europe PMC). CP, EDU, FIN, IND, MEG binarios; RD y OC calibrados con
# las anclas de la Tabla 4 (percentiles 75/50/25). Soluciones: Tablas 6 y 7 (círculos como imágenes en línea:
# i0001 = presente, i0002 = ausente; ●/⊗ en texto = periféricas).
# ERRATA (intra, config. 3): la tabla pone el círculo de presencia en la fila PRO_CP y deja PRO_R&D en blanco;
# CP*~OC*~EDU*~FIN*IND da 0,999/0,053 (publicado 0,947/0,0658). Búsqueda exhaustiva sobre los 3^6 términos:
# solo RD*~OC*~EDU*~FIN*IND (0,945/0,065) cuadra -> el círculo está una fila desplazado. Se usa el corregido.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r70_yang_2021/data.csv")
cat("Percentiles 25/50/75 recalculados:\n"); print(round(t(sapply(r[c("RD", "OC")], quantile, c(0.25, 0.5, 0.75))), 4))
d <- data.frame(CP = r$CP, RD = round(calibrate(r$RD, type = "fuzzy", thresholds = c(1.11, 2.595, 3.5175)), 3),
                OC = round(calibrate(r$OC, type = "fuzzy", thresholds = c(25.2225, 28.225, 33.5725)), 3),
                EDU = r$EDU, FIN = r$FIN, IND = r$IND, MEG = r$MEG)
cat("Casos en 0,500:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nMEG <- 1 - d$MEG
cat("Necesidad (Tabla 5: RD 0,6954/0,5696; OC 0,7154/0,5730; EDU 0,8462/0,4783):\n")
print(round(QCA::pof("RD + OC + EDU", "MEG", d, relation = "necessity")$incl.cov[1:3, 1:2], 4))
cd <- c("CP", "RD", "OC", "EDU", "FIN", "IND")
hi <- c("CP*RD*OC*EDU*~FIN", "CP*RD*OC*EDU*IND", "CP*RD*OC*~FIN*IND", "~CP*~RD*OC*EDU*~FIN*~IND",
        "~CP*RD*~OC*~EDU*FIN*~IND")
lo <- c("~CP*~OC*EDU*~FIN*IND", "~CP*~RD*~OC*EDU*~FIN", "CP*~RD*EDU*FIN*~IND", "RD*~OC*~EDU*~FIN*IND",
        "~RD*~OC*~EDU*FIN*IND", "CP*~RD*~OC*~EDU*FIN", "CP*OC*~EDU*FIN*IND", "~CP*RD*~EDU*~FIN*IND",
        "~CP*~RD*OC*~EDU*~FIN")
r1 <- replicar_cs(d[, c(cd, "MEG")], "MEG", hi, id = "R70 Yang 2021 (entre industrias)",
  publicados = data.frame(term = hi, incl_pub = c(0.8966, 0.8552, 0.8693, 0.9135, 1),
                          cov_pub = c(0.22, 0.2135, 0.1688, 0.0365, 0.0196)))
r2 <- replicar_cs(d[, c(cd, "nMEG")], "nMEG", lo, id = "R70 Yang 2021 (intraindustria)",
  publicados = data.frame(term = lo, incl_pub = c(0.8255, 0.8527, 1, 0.947, 0.9953, 0.9955, 0.9771, 0.9239, 1),
    cov_pub = c(0.1518, 0.1082, 0.1021, 0.0658, 0.0558, 0.0582, 0.045, 0.0447, 0.0532)))
saveRDS(list(alta = r1, baja = r2), "replicas/r70_yang_2021.rds")
