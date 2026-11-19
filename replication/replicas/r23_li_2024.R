# R23 (csQCA). Li et al. 2024, BMC Primary Care, 10.1186/s12875-024-02278-8 (PMC10823627).
# Efectividad de equipos de atención primaria (23 casos).
# Datos: tabla de verdad (Tabla 4) con número de casos por fila, sin columna de resultado. Las coberturas
# publicadas (Tabla 6: 10, 7, 2, 2, 2 y 1 de 22 casos) y la consistencia 1 de todos los caminos solo son
# compatibles con que el único caso sin el resultado sea la fila 6 (PA=1, CG=1, resto 0), la única que
# ningún camino cubre. Reconstrucción exacta: las seis coberturas coinciden.
source("replicas/csqca_comun.R")
tt <- read.table(header = TRUE, text = "
PA TS TC CG CC CMR CIS n
0 1 0 1 1 0 0 3
0 1 0 1 0 0 0 2
1 1 0 1 1 0 0 2
1 1 1 1 1 0 0 2
0 0 0 1 0 0 0 1
1 0 0 1 0 0 0 1
1 1 0 1 0 0 0 1
1 1 1 1 0 0 0 1
0 0 0 1 1 0 0 1
1 1 0 1 0 1 0 1
1 0 0 1 0 0 1 1
1 1 0 1 0 0 1 1
1 1 1 1 0 0 1 1
1 1 0 1 1 0 1 1
1 1 1 1 1 0 1 1
0 0 0 0 1 1 1 1
1 0 0 1 1 1 1 1
0 1 1 1 1 1 1 1")
tt$PTE <- 1; tt$PTE[6] <- 0
d <- tt[rep(seq_len(nrow(tt)), tt$n), c(1:7, 9)]; rownames(d) <- NULL
terms <- c("PA*TS*~CMR", "~PA*~TC*~CMR*~CIS", "PA*TS*~TC*~CC*~CIS", "PA*~TC*~CC*~CMR*CIS",
           "~TS*~TC*CC*CMR*CIS", "~PA*TS*TC*CC*CMR*CIS")
r1 <- replicar_cs(d, "PTE", terms, id = "R23 Li 2024 (efectividad del equipo)",
  publicados = data.frame(term = terms, incl_pub = 1, cov_pub = c(0.4545, 0.3182, 0.0909, 0.0909, 0.0909, 0.0455)))
saveRDS(list(r1), "replicas/r23_li_2024.rds")
