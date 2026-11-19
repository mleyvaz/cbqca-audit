# R18 (csQCA). Ziemann et al. 2016, PLOS ONE, 10.1371/journal.pone.0155535 (PMC4868285).
# Factores de éxito de sistemas europeos de vigilancia sindrómica.
# Datos: Tablas 2 y 3 (datos crudos por sistema/país). Exacto.
# Soluciones: Tabla 6 (gripe estacional) y Tabla 7 (conciencia situacional).
source("replicas/csqca_comun.R")
gripe <- read.table(header = TRUE, text = "
caso NONCLIN ACUTE SUBNAT AGE OUT
s1177 1 0 0 0 1
GETWELL 1 0 0 0 1
Genoa 0 1 1 1 1
SIDARTHa 0 1 1 0 1
NHS24 1 0 0 0 1
NHSDirect 1 0 1 1 1
QSurv 0 0 1 0 0
Oscour 0 1 0 0 0
SOSMed 0 1 1 0 1")[, -1]
gripe$nOUT <- 1 - gripe$OUT
r1 <- replicar_cs(gripe[, 1:5], "OUT", c("NONCLIN", "ACUTE*SUBNAT"), id = "R18 Ziemann 2016 (gripe, éxito)",
  publicados = data.frame(term = c("NONCLIN", "ACUTE*SUBNAT"), incl_pub = 1, cov_pub = c(0.57, 0.43)))
r2 <- replicar_cs(gripe[, c(1:4, 6)], "nOUT", c("~NONCLIN*~ACUTE", "~NONCLIN*~SUBNAT"), id = "R18 Ziemann 2016 (gripe, fracaso)",
  publicados = data.frame(term = c("~NONCLIN*~ACUTE", "~NONCLIN*~SUBNAT"), incl_pub = 1, cov_pub = 0.5))
sa <- read.table(header = TRUE, text = "
caso MULTDATA MULTSYND FREETEXT AUTOM EXIST ELEC OUT
DE 0 0 0 0 0 0 0
DK 0 0 0 1 1 1 0
EN 1 1 0 1 1 1 1
FR 1 1 0 1 1 1 1
IT 0 1 0 0 0 0 0
SC 1 1 1 1 1 1 1
SE 0 1 1 1 1 1 1
SID 1 1 0 0 1 1 0")[, -1]
sa$nOUT <- 1 - sa$OUT
r3 <- replicar_cs(sa[, 1:7], "OUT", "MULTSYND*AUTOM", id = "R18 Ziemann 2016 (situacional, éxito)",
  publicados = data.frame(term = "MULTSYND*AUTOM", incl_pub = 1, cov_pub = 1))
r4 <- replicar_cs(sa[, c(1:6, 8)], "nOUT", c("~MULTSYND", "~AUTOM"), id = "R18 Ziemann 2016 (situacional, fracaso)",
  publicados = data.frame(term = c("~MULTSYND", "~AUTOM"), incl_pub = 1, cov_pub = c(0.5, 0.75)))
saveRDS(list(r1, r2, r3, r4), "replicas/r18_ziemann_2016.rds")
