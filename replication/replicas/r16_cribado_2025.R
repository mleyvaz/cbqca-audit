# R16 (csQCA). Mujeres mayores y cribado de cáncer de mama, PLOS ONE 2025, 10.1371/journal.pone.0319141.
# Datos: Tabla 4 (perfiles de los 59 casos con sus conteos de continuar/abandonar).
# Condiciones: OA edad >= 75, SI cribado por iniciativa propia, CS historia personal de cáncer,
# DR recomendación del médico, RS cribado reciente. Resultado CONT (intención de continuar).
# Reglas publicadas (Tabla 4): continuar RS*CS*DR, RS*CS*~DR, RS*~CS*DR; abandonar ~SI*~CS*~DR,
# ~SI*CS*~DR, ~SI*~CS*DR. No reportan consistencia: la réplica verifica los conteos (32/27).
source("replicas/csqca_comun.R")
p <- read.table(header = TRUE, text = "
OA SI CS DR RS nc nd
1 0 1 1 1 6 1
0 0 1 1 1 7 0
0 1 1 1 1 1 0
1 1 1 0 1 5 0
0 1 1 0 1 2 0
1 0 1 0 1 1 1
0 1 0 1 1 1 0
1 1 0 1 1 0 1
0 0 0 1 1 6 0
1 0 0 1 1 0 2
1 0 0 0 0 0 6
1 0 0 0 1 0 3
0 0 0 0 0 0 2
0 0 0 0 1 0 3
1 0 1 0 0 0 4
0 0 1 0 0 1 2
1 0 0 1 0 1 0
0 0 0 1 0 0 1
0 1 1 0 0 1 1")
d <- p[c(rep(seq_len(nrow(p)), p$nc), rep(seq_len(nrow(p)), p$nd)), 1:5]
d$CONT <- c(rep(1, sum(p$nc)), rep(0, sum(p$nd))); d$nCONT <- 1 - d$CONT
cat("Casos: continuar", sum(d$CONT), "(publicado 32) | abandonar", sum(d$nCONT), "(publicado 27)\n")
cat("Comprobación con la Tabla 3: cribado reciente en continuar 29/32 y en abandonar 11/27 ->",
    sum(d$RS[d$CONT == 1]), sum(d$RS[d$CONT == 0]), "\n")
cont <- c("RS*CS*DR", "RS*CS*~DR", "RS*~CS*DR")
# Las reglas se aplican en orden (lista de decisión): las de abandonar solo clasifican los casos que
# no cubren las de continuar. Por eso las filas de las reglas 2 y 3 tienen todas RS = 0 (Tabla 4) y el
# término efectivo es ~SI*CS*~DR*~RS y ~SI*~CS*DR*~RS. Leerlas sin ~RS daría 0,78 y 0,30 de consistencia.
# Nota para el texto: es una clasificación configuracional de estilo csQCA, no una minimización booleana.
disc <- c("~SI*~CS*~DR", "~SI*CS*~DR*~RS", "~SI*~CS*DR*~RS")
r1 <- replicar_cs(d[, c("OA", "SI", "CS", "DR", "RS", "CONT")], "CONT", cont, id = "R16 Cribado 2025 (continuar)")
r2 <- replicar_cs(d[, c("OA", "SI", "CS", "DR", "RS", "nCONT")], "nCONT", disc, id = "R16 Cribado 2025 (abandonar)")
saveRDS(list(r1, r2), "replicas/r16_cribado_2025.rds")
