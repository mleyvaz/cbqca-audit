# R46 (csQCA). Wang et al. 2024, Frontiers in Public Health 12:1396013, 10.3389/fpubh.2024.1396013
# (PMC11345240). Confinamiento estricto en 26 países europeos (primera ola 2020).
# Datos: Tabla 7 (codificación crisp de los 26 países). Modelo de 4 condiciones (ES, SF, PS, FC),
# corte de consistencia 0,8, n = 1 (Tabla 9). Solución (Tabla 10; compleja = intermedia = parsimoniosa):
# ~ES*~SF*~PS + SF*~PS*~FC + ES*PS*FC, consistencias 1, coberturas 0,142857 / 0,285714 / 0,428571.
# El pie de la Tabla 10 dice «solution consistency 0,857143; coverage 1»: está invertido (es 1 y 0,857).
source("replicas/csqca_comun.R")
d <- read.table(header = TRUE, row.names = 1, text = "
pais EB SP ES SF PS FC PI LOCK
AUT 0 0 1 0 1 1 0 1 
BEL 0 1 1 1 0 0 0 1 
BGR 1 0 0 1 1 1 0 0 
HRV 1 0 0 1 1 0 0 0 
CYP 0 0 0 1 0 0 0 1 
CZE 1 0 0 1 0 0 0 1 
DNK 0 0 1 0 0 0 1 0 
EST 1 0 0 0 1 1 0 0 
FIN 0 0 1 0 0 1 1 0 
FRA 0 1 1 1 1 1 0 1 
DEU 0 0 1 1 0 1 0 0 
GRC 0 0 1 1 1 1 0 1 
HUN 1 0 0 0 1 1 0 0 
IRL 0 1 0 0 0 0 0 1 
ITA 0 1 1 1 1 1 1 1 
LVA 1 0 1 0 0 1 0 0 
LTU 1 0 0 1 0 1 1 0 
NLD 0 1 1 0 0 1 0 0 
POL 1 0 0 0 1 1 0 1 
PRT 0 0 1 0 1 0 1 0 
ROU 1 0 0 1 1 0 0 1 
SVK 1 0 1 1 1 1 0 1 
SVN 1 0 0 0 0 1 0 1 
ESP 0 1 1 1 0 0 1 1 
SWE 0 1 1 0 0 0 1 0 
GBR 0 1 1 1 1 1 0 1 
")
cat("N =", nrow(d), "| confinamiento:", sum(d$LOCK), "\n")
d4 <- d[, c("ES", "SF", "PS", "FC", "LOCK")]
t1 <- c("~ES*~SF*~PS", "SF*~PS*~FC", "ES*PS*FC")
r1 <- replicar_cs(d4, "LOCK", t1, id = "R46 Wang 2024 (confinamiento)",
  publicados = data.frame(term = t1, incl_pub = 1, cov_pub = c(.142857, .285714, .428571)))
print(round(QCA::pof(d[, c("EB", "SP", "ES", "SF", "PS", "FC", "PI")], "LOCK", data = d, relation = "necessity")$incl.cov[, 1:3], 2))
saveRDS(list(r1), "replicas/r46_wang_2024.rds")
