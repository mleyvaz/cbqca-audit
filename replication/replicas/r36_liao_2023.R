# R36 Liao y Yang (2023) PLOS ONE 10.1371/journal.pone.0292253 — fsQCA, 61 marcas chinas internacionalizadas.
# RESULTADO: NO REPLICABLE. Términos 1, 2, 3 y 5 cuadran (incl <= 0,002; cov +0,005/+0,007), pero la configuración 4
# (~TI*IP*ME*~BE*~RE*~CD) da incl 0,921 frente a 0,978 publicada; ningún cambio de signo ni omisión de una condición la reproduce.
# Datos: S1 Data (s001.xlsx): TI, IP, ME, BE, RE, CD (condiciones) y BI (resultado), datos brutos.
# Anclas: Tabla 4 (percentiles 95/50/5 ajustados por los autores; BE cruce 77,9 y BI cruce 10).
# Solución: Tabla 6 (5 configuraciones, todas las condiciones especificadas; solo resultado alto).
source("replicas/csqca_comun.R")
x <- as.data.frame(readxl::read_excel("datos/liao_2023/s001.xlsx", n_max = 61))
v <- c("TI", "IP", "ME", "BE", "RE", "CD", "BI")
raw <- as.data.frame(lapply(x[, 3:9], as.numeric)); names(raw) <- v
anc <- list(TI = c(2, 600, 35000), IP = c(4.3, 5, 5.7), ME = c(10, 124, 200), BE = c(68, 77.9, 81.4),
            RE = c(45, 40000, 730000), CD = c(1.7, 2.7, 3.6), BI = c(1.3, 10, 50))
d <- as.data.frame(lapply(v, function(k) round(calibrate(raw[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- v
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
hi <- c("~TI*~IP*ME*~BE*RE*CD", "TI*IP*ME*~BE*RE*CD", "~TI*IP*~ME*BE*RE*CD",
        "~TI*IP*ME*~BE*~RE*~CD", "TI*~IP*~ME*~BE*RE*~CD")
r1 <- replicar_cs(d, "BI", hi, id = "R36 Liao 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.9422, 0.9893, 0.9078, 0.9776, 0.9647),
                          cov_pub = c(0.1886, 0.1568, 0.2197, 0.2095, 0.1542)))
saveRDS(list(alta = r1), "replicas/r36_liao_2023.rds")
