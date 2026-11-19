# R43 (csQCA). Reanálisis de intervenciones de vacunación antigripal en personal sanitario,
# BMC Health Services Research 2022, 22:653, 10.1186/s12913-022-08001-6 (PMC9108687).
# Análisis 2 (mandatos blandos/otros, N = 20, crisp): tabla de verdad publicada (Tabla 4) con los casos
# nombrados por fila y resultado por fila (consistencias 0/1) -> todos los casos reconstruidos.
# Solución intermedia mejorada (Tabla 5): LEADSUP*DGC*EDUC + LEADSUP*DGC*TWOWAY*~LETTER + ~LEADSUP*EDUC*LETTER.
# Análisis crisp de mandatos duros (Tabla 6, N = 11): condiciones de la Tabla 2 (ya dicotómicas),
# resultado = 1 en los 4 casos con RR < 0,2 nombrados en la Tabla 6 (Stuart, Babcock, Rakita, Smith).
source("replicas/csqca_comun.R")
tt <- read.table(header = TRUE, text = "
n LEADSUP DGC TWOWAY EDUC LETTER SUCC
4 1 1 1 1 0 1
1 0 0 0 1 1 1
1 0 1 1 1 1 1
1 1 1 0 1 0 1
1 1 1 0 1 1 1
1 1 1 1 0 0 1
1 1 1 1 1 1 1
4 0 1 0 1 0 0
2 0 0 1 1 0 0
1 0 0 0 1 0 0
1 0 1 0 0 0 0
1 0 1 1 1 0 0
1 1 0 0 1 0 0")
d <- expandir_frecuencia(tt, "n")
cat("N =", nrow(d), "| exitosos:", sum(d$SUCC), "\n")
t1 <- c("LEADSUP*DGC*EDUC", "LEADSUP*DGC*TWOWAY*~LETTER", "~LEADSUP*EDUC*LETTER")
r1 <- replicar_cs(d, "SUCC", t1, id = "R43 Sutcliffe 2022 (mandatos blandos)",
  publicados = data.frame(term = t1, incl_pub = c(1, 1, 1), cov_pub = c(.7, .5, .2)))
th <- read.table(header = TRUE, text = "
n TWOWAY LEADSUP EDUC DGC SUCC
3 1 1 1 1 1
1 1 1 0 1 1
2 0 1 1 1 0
4 0 0 0 0 0
1 0 1 0 1 0")
dh <- expandir_frecuencia(th, "n")
r2 <- replicar_cs(dh, "SUCC", "TWOWAY*LEADSUP*DGC", id = "R43 Sutcliffe 2022 (mandatos duros crisp)",
  publicados = data.frame(term = "TWOWAY*LEADSUP*DGC", incl_pub = 1, cov_pub = 1))
saveRDS(list(r1, r2), "replicas/r43_sutcliffe_2022.rds")
