# R48 (csQCA). Piper et al. 2023, Implementation Science Communications 4:66, 10.1186/s43058-023-00450-2
# (PMC10251711). Implementación de PrEP en clínicas de planificación familiar del sur de EE. UU., N = 38.
# Datos: Additional file 4 «Reduced Dataset for QCA Analysis» (38 clínicas, crisp; 11 con PrEP).
# Soluciones (texto, Resultados): PrEP = LEAD*RES + LEAD*~SE + KNOW*~SE (consistencia 1; cobertura 0,82/0,73/0,64);
# sin PrEP = ~KNOW*~LEAD + ~RES*PART (consistencia 1; cobertura 0,89/0,70).
# ERRATA DOCUMENTADA: ~RES*PART cubre 18/27 = 0,667, no 0,70. Lo confirman la propia Tabla 3 del artículo
# (matriz de casos: 18 clínicas sin PrEP marcadas en el camino 2; guardada en datos/piper_2023/tab3b.jpg), el
# texto («16 clínicas con ambos caminos»: 24 + 18 - 16 = 26) y la cobertura de solución 0,96 = 26/27.
source("replicas/csqca_comun.R")
d <- read.table(header = FALSE, col.names = c("id", "PREP", "RES", "KNOW", "PART", "LEAD", "SE"), text = "
1 1 0 0 0 1 0
2 1 1 1 1 1 0
3 1 1 1 1 1 0
4 1 1 1 1 1 0
5 1 1 0 1 1 1
6 1 1 0 0 1 1
7 1 1 0 0 1 0
8 1 0 1 0 0 0
9 1 1 1 1 1 0
10 1 1 1 0 1 0
11 1 1 1 0 1 0
12 0 0 0 1 0 0
13 0 0 0 1 0 0
14 0 0 0 0 0 0
15 0 0 0 0 0 0
16 0 0 0 0 0 0
17 0 0 0 1 0 1
18 0 0 0 1 0 1
19 0 1 0 1 0 1
20 0 0 0 0 1 1
21 0 0 0 1 0 1
22 0 0 0 1 0 1
23 0 0 0 1 0 1
24 0 0 0 1 0 1
25 0 0 1 1 1 1
26 0 0 0 1 0 0
27 0 1 0 1 0 0
28 0 1 0 1 0 0
29 0 0 0 1 0 1
30 0 0 0 1 1 1
31 0 0 0 0 0 0
32 0 0 0 0 0 0
33 0 0 0 1 0 1
34 0 0 0 1 0 0
35 0 0 0 1 0 0
36 0 0 0 1 0 0
37 0 0 0 1 0 0
38 0 0 0 1 0 0
")
d$id <- NULL; d$NOPREP <- 1 - d$PREP
cs <- c("RES", "KNOW", "PART", "LEAD", "SE")
cat("N =", nrow(d), "| con PrEP:", sum(d$PREP), "\n")
t1 <- c("LEAD*RES", "LEAD*~SE", "KNOW*~SE")
r1 <- replicar_cs(d[, c(cs, "PREP")], "PREP", t1, id = "R48 Piper 2023 (PrEP)",
  publicados = data.frame(term = t1, incl_pub = 1, cov_pub = c(.82, .73, .64)))
t2 <- c("~KNOW*~LEAD", "~RES*PART")
r2 <- replicar_cs(d[, c(cs, "NOPREP")], "NOPREP", t2, id = "R48 Piper 2023 (sin PrEP)",
  publicados = data.frame(term = c(t2, "expression"), incl_pub = 1, cov_pub = c(.89, .70, .96)))
saveRDS(list(r1, r2), "replicas/r48_piper_2023.rds")
