# R19 (csQCA). Blackman et al. 2013, Social Policy & Society, 10.1017/S1474746412000358 (PMC3873005).
# Reducción local del embarazo adolescente en Inglaterra (27 áreas).
# Datos: Tabla 2 (27 casos, 9 condiciones, resultado). Exacto.
# Solución parsimoniosa (texto y Tabla 3): estrechamiento = BME + DRUG*U18*BASIC;
# no estrechamiento = ~BME*~U18 + ~BME*~DRUG + ~BME*~BASIC.
# BASIC = «basic standard of commissioning» = ausencia de comisionado bueno/ejemplar.
source("replicas/csqca_comun.R")
d <- read.table(header = TRUE, text = "
DEPRIV DRUG U18 GCSE LEAD COMMUN BME MAJOR BASIC OUT
1 1 1 1 1 1 1 1 1 1
1 1 1 1 1 0 0 0 1 1
1 1 1 0 1 1 0 1 1 1
0 1 0 0 1 1 1 0 1 1
0 1 0 0 1 0 1 0 1 1
0 0 1 0 0 0 1 0 0 1
1 0 1 1 0 1 1 1 0 1
0 0 1 0 1 0 1 0 1 1
0 1 0 1 1 0 1 1 1 1
0 1 1 0 1 1 0 1 1 1
0 1 0 0 1 1 1 1 0 1
1 1 0 0 0 0 0 0 0 0
1 1 0 1 1 1 0 0 1 0
0 1 1 0 0 1 0 0 0 0
1 1 0 1 0 0 0 0 0 0
1 1 0 1 0 0 0 1 1 0
0 1 0 1 0 0 0 0 0 0
0 1 0 0 0 0 0 0 0 0
1 0 0 1 1 1 0 0 1 0
1 0 0 1 0 0 0 0 1 0
1 0 0 1 0 1 0 0 0 0
1 0 1 1 1 1 0 0 1 0
1 1 0 0 1 0 0 1 0 0
0 0 1 0 1 0 0 0 1 0
0 0 0 1 0 0 0 0 1 0
0 0 0 0 0 1 0 1 0 0
0 0 0 0 1 0 0 0 1 0")
d$nOUT <- 1 - d$OUT
pos <- c("BME", "DRUG*U18*BASIC"); neg <- c("~BME*~U18", "~BME*~DRUG", "~BME*~BASIC")
r1 <- replicar_cs(d[, 1:10], "OUT", pos, id = "R19 Blackman 2013 (estrecha)",
  publicados = data.frame(term = pos, incl_pub = 1, cov_pub = NA))
r2 <- replicar_cs(d[, c(1:9, 11)], "nOUT", neg, id = "R19 Blackman 2013 (no estrecha)",
  publicados = data.frame(term = neg, incl_pub = 1, cov_pub = NA))
saveRDS(list(r1, r2), "replicas/r19_blackman_2013.rds")
