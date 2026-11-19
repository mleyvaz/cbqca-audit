# R92 Yu et al. (2024) PLOS ONE 10.1371/journal.pone.0300169 — fsQCA, 30 provincias (calidad del desarrollo del
# deporte rural; resultado = supereficiencia DEA). Datos: S1 Data (brutos). Anclas declaradas solo como percentiles
# 95/50/5 (sin valores); software fsQCA 3.0. Ninguna combinación (cuantiles tipo 6 o 7; calibración QCA o
# estilo fsQCA; «grassroots organizations» = estaciones culturales o instituciones de construcción rural) reproduce
# la Tabla 6 (p. ej., camino 2 da 0,61 frente a 0,957 publicado). NO REPLICABLE: no se registra en el corpus.
suppressMessages(library(QCA))
r <- read.csv("datos/r92_pone0300169/datos.csv")
v <- c("POP","ECO","INF","AGR","GRA","NFI","SCI","FLD","Y")
hi <- c("POP*ECO*INF*~GRA*NFI*SCI*FLD", "POP*INF*AGR*GRA*NFI*SCI*FLD", "~POP*ECO*~INF*~AGR*~GRA*NFI*SCI*FLD", "~POP*~ECO*~INF*~AGR*GRA*NFI*SCI*FLD")
lo <- c("~POP*~ECO*~INF*~AGR*~NFI*~SCI*~FLD", "POP*~ECO*INF*AGR*GRA*NFI*~FLD", "~POP*ECO*INF*~AGR*~GRA*~NFI*SCI*~FLD", "POP*ECO*INF*~AGR*GRA*~NFI*SCI*FLD")
for (ty in c(7, 6)) for (fs in c(TRUE, FALSE)) {
 d <- as.data.frame(lapply(v, function(k) { a <- quantile(r[[k]], c(.05,.5,.95), type = ty)
   x <- calibrate(r[[k]], type="fuzzy", thresholds=a, idm = if (fs) plogis(3) else 0.95); if (fs) round(x,2) else x }))
 names(d) <- v; d$nY <- 1 - d$Y; cat("tipo", ty, "fs", fs, " casos 0.5:", sum(d==0.5), "\n")
 print(round(cbind(pof(paste(hi,collapse="+"),"Y",data=d,relation="sufficiency")$incl.cov[1:4,c(1,3)], pof(paste(lo,collapse="+"),"nY",data=d,relation="sufficiency")$incl.cov[1:4,c(1,3)]),3)) }
