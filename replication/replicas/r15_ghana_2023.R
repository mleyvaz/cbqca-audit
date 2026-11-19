# R15 (csQCA). Anticorrupción en Ghana y ONG internacionales, PLOS ONE 2023, 10.1371/journal.pone.0291388.
# Datos: la tabla de verdad publicada (Tabla 2) contiene todos los casos (N = 11). La fila 9 tiene
# consistencia 0,5 = 2 casos (1 con AC y 1 sin AC). Se verifica con la Tabla 1 (necesidad) y las coberturas.
# Soluciones: Tabla 3 (AC) y Tabla 4 (~AC). En el término 1, «CS» es una errata de ~ST (la cobertura
# 0,4 = 2 casos solo cuadra con las filas 3 y 4, que tienen ST = 0).
source("replicas/csqca_comun.R")
tt <- read.table(header = TRUE, text = "
RICSO FE ST LD RR FP TR AC
1 0 0 0 0 1 0 1
1 1 1 0 1 1 0 0
1 1 0 1 0 1 0 1
1 1 0 1 1 1 0 1
1 1 0 1 0 0 1 1
0 1 0 0 0 1 0 0
0 1 0 0 1 1 0 0
0 0 0 0 0 1 0 0
0 0 0 1 0 1 0 1
0 0 0 1 0 1 0 0
0 1 1 1 1 1 1 0")
tt$nAC <- 1 - tt$AC
cat("Necesidad (publicado: RICSO 0,8/0,8; FE 0,6/0,429; LD 0,8/0,667; FP 0,8/0,4; TR 0,2/0,5):\n")
print(round(QCA::pof(tt[, 1:7], outcome = tt$AC, relation = "necessity")$incl.cov[, 1:2], 3))
pos <- c("RICSO*FE*~ST*LD*FP*~TR", "RICSO*~FE*~ST*~LD*~RR*FP*~TR", "RICSO*FE*~ST*LD*~RR*~FP*TR")
neg <- c("~RICSO*~ST*~LD*~RR*FP*~TR", "~RICSO*FE*~ST*~LD*FP*~TR", "RICSO*FE*ST*~LD*RR*FP*~TR", "~RICSO*FE*ST*LD*RR*FP*TR")
r1 <- replicar_cs(tt[, c(1:7, 8)], "AC", pos, id = "R15 Ghana 2023 (AC)",
  publicados = data.frame(term = pos, incl_pub = 1, cov_pub = c(0.4, 0.2, 0.2)))
r2 <- replicar_cs(tt[, c(1:7, 9)], "nAC", neg, id = "R15 Ghana 2023 (~AC)",
  publicados = data.frame(term = neg, incl_pub = 1, cov_pub = c(1/3, 1/3, 1/6, 1/6)))
saveRDS(list(r1, r2), "replicas/r15_ghana_2023.rds")
