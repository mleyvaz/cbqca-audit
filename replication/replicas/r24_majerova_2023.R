# R24 Majerová, Jirásek et al. (2023) PLOS ONE 10.1371/journal.pone.0294638 — csQCA, 21 aerolíneas low-cost.
# Datos: Tabla 3 (matriz de datos dicotómica publicada; coincide con S1 Data s005.xlsx, que añade Onur Air, no analizada).
# Software fsQCA 3.0; soluciones intermedias: Tabla 6 (Success) y Tabla 7 (~Success), consistencia 1 en todos los términos.
source("replicas/csqca_comun.R")
d <- read.csv("datos/lowcost_2023/tabla3.csv")[, -1]
d$nSUCC <- 1 - d$SUCC
cd <- c("SIZE", "GROUP", "SERV", "ONTIME", "PROD", "LONG")
hi <- c("~GROUP*SERV*~ONTIME*~LONG", "SIZE*~GROUP*SERV*~ONTIME", "~SERV*ONTIME*PROD*~LONG",
        "SIZE*GROUP*~SERV*PROD*~LONG", "~SIZE*GROUP*SERV*~ONTIME*LONG")
lo <- c("~SIZE*GROUP*~SERV*~ONTIME*~LONG", "~SIZE*~GROUP*~SERV*~ONTIME*LONG", "GROUP*SERV*ONTIME*~PROD*~LONG",
        "~SIZE*GROUP*SERV*ONTIME*~PROD", "~SIZE*~SERV*~ONTIME*~PROD*~LONG", "~SIZE*~GROUP*~SERV*~ONTIME*~PROD")
r1 <- replicar_cs(d[, c(cd, "SUCC")], "SUCC", hi, id = "R24 Majerova 2023 (exito)",
  publicados = data.frame(term = hi, incl_pub = rep(1, 5), cov_pub = c(0.286, 0.214, 0.429, 0.143, 0.071)))
r2 <- replicar_cs(d[, c(cd, "nSUCC")], "nSUCC", lo, id = "R24 Majerova 2023 (no exito)",
  publicados = data.frame(term = lo, incl_pub = rep(1, 6), cov_pub = c(0.143, 0.143, 0.429, 0.429, 0.143, 0.143)))
saveRDS(list(alta = r1, baja = r2), "replicas/r24_majerova_2023.rds")
