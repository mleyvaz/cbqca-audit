# R38 Rodrigues et al. (2022) Scientific Reports 10.1038/s41598-022-26283-y — fsQCA sobre datos dicotómicos,
# 3748 pacientes de atención primaria (Río de Janeiro), resultado COVID positivo.
# RESULTADO: APROXIMADA (calibración 0,05/0,95 inferida; LTS*FAT cov 0,272 frente a 0,28 publicado).
# Datos: MOESM1 (CSV, sep ";"), ya en 0/1; LTS = pérdida de gusto O de olfato (unión, como los autores).
# Solución: Tabla 2 (solución PARSIMONIOSA, la que interpretan), con 2 decimales. Positivo y no positivo.
source("replicas/csqca_comun.R")
x <- read.csv("datos/rodrigues_2022/moesm1.csv", sep = ";")
d <- data.frame(RD = x$RESP_DISTRESS, LTS = pmax(x$LOSS_TASTE, x$LOSS_SMELL), FEV = x$FEVER,
                HEAD = x$HEADACHE, FAT = x$FADIGUE, COVID = x$COVID)
d <- d[complete.cases(d), ]   # casos con algún síntoma faltante quedan fuera (N = 3338 de 3748)
# Calibración NO declarada numéricamente ("degrees of membership 0-1, 0.5 crossover"): con 0/1 crudos no cuadra
# (difs. 0,14-0,48); con 0 -> 0,05 y 1 -> 0,95 (incluido el resultado) cuadra a 2 decimales salvo LTS*FAT cov.
d[] <- lapply(d, function(z) ifelse(z == 1, 0.95, 0.05))
d$nCOVID <- 1 - d$COVID
cd <- c("RD", "LTS", "FEV", "HEAD", "FAT")
pos <- c("RD", "LTS*~HEAD", "LTS*FEV", "LTS*FAT")
neg <- c("RD", "FAT", "~LTS*~FEV", "LTS*FEV*~HEAD")
r1 <- replicar_cs(d[, c(cd, "COVID")], "COVID", pos, id = "R38 Rodrigues 2022 (positivo)",
  publicados = data.frame(term = pos, incl_pub = c(0.57, 0.81, 0.70, 0.71), cov_pub = c(0.25, 0.25, 0.30, 0.28)))
r2 <- replicar_cs(d[, c(cd, "nCOVID")], "nCOVID", neg, id = "R38 Rodrigues 2022 (no positivo)",
  publicados = data.frame(term = neg, incl_pub = c(0.74, 0.67, 0.79, 0.76), cov_pub = c(0.19, 0.37, 0.61, 0.09)))
saveRDS(list(pos = r1, neg = r2), "replicas/r38_rodrigues_2022.rds")
