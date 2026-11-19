# R52 Huang et al. (2024) PLOS ONE 10.1371/journal.pone.0309229 — fsQCA, 30 provincias chinas (energía de ecosistemas de innovación digital).
# Datos: figshare 10.6084/m9.figshare.25952083, "Data-Weight-Table" (puntajes compuestos por entropía, 30 casos).
# Anclas declaradas: cuartiles 75/50/25 (texto). Los valores de la Tabla 2 NO están en la escala de los datos
# publicados (son ~0,62 x los cuartiles); el registro de fsQCA publicado (Operational-Result.txt) usa los cuartiles,
# salvo XT con exclusión = cruce (0,02671), una errata evidente. Aquí: anclas del registro (cuartiles del Excel), con XT corregida.
# Mapa: HX núcleo, XT colaborativos, ZL gobernanza digital, TJ reguladores, SS infraestructura, HJ entorno.
# Solución intermedia: Tabla 6 (H1-H3 para EL; N1-N4 para ~EL), idéntica al registro de fsQCA.
source("replicas/csqca_comun.R")
r <- read.csv("datos/huang_2024/puntajes.csv")
v <- c("EL", "HX", "XT", "ZL", "TJ", "SS", "HJ")
# Anclas del registro de fsQCA (= filas 0,25/0,5/0,75 del Excel; difieren de quantile() de R), XT con exclusión en su cuartil.
q <- cbind(EL = c(0.00865, 0.02258, 0.03929), HX = c(0.00149, 0.01571, 0.07441), XT = c(0.01592, 0.02671, 0.03528),
           ZL = c(0.00622, 0.00877, 0.01444), TJ = c(0.01096, 0.02216, 0.03601), SS = c(0.00443, 0.00832, 0.01246),
           HJ = c(0.02847, 0.04109, 0.07034))
cat("Cuartiles de R (tipo 7), solo como referencia:\n"); print(round(sapply(v, function(z) quantile(r[[z]], c(0.25, 0.5, 0.75))), 5))
d <- as.data.frame(sapply(v, function(z) round(calibrate(r[[z]], type = "fuzzy", thresholds = q[, z]), 2)))
cat("Necesidad para EL (publicado HX .8145 XT .7560 ZL .8474 TJ .7796 SS .8737 HJ .8622):\n")
print(round(sapply(v[-1], function(z) sum(pmin(d[[z]], d$EL)) / sum(d$EL)), 4))
d$nEL <- 1 - d$EL
cd <- v[-1]
hi <- c("HX*ZL*~TJ*SS*HJ", "HX*XT*ZL*TJ*SS", "HX*XT*TJ*SS*HJ")
lo <- c("~HX*~ZL*~TJ*~SS*~HJ", "HX*~XT*~ZL*TJ*~SS*~HJ", "~HX*~XT*~ZL*TJ*~SS*HJ", "~HX*~XT*ZL*TJ*SS*~HJ")
r1 <- replicar_cs(d[, c(cd, "EL")], "EL", hi, id = "R52 Huang 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.992366, 0.968146, 0.969893), cov_pub = c(0.174731, 0.571909, 0.606183)))
r2 <- replicar_cs(d[, c(cd, "nEL")], "nEL", lo, id = "R52 Huang 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.947732, 0.923077, 1, 1), cov_pub = c(0.635582, 0.119048, 0.132275, 0.149471)))
saveRDS(list(alta = r1, baja = r2), "replicas/r52_huang_2024.rds")
# CLASIFICACIÓN: no replicable — los términos sin XT cuadran (<=0,006) pero los que contienen XT difieren 0,03 en cobertura; la calibración de XT del registro (exclusión = cruce) no reproduce la necesidad publicada (0,756) con ninguna variante. .rds del corpus borrado.
