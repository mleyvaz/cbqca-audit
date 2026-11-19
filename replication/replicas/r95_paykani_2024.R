# R95 Paykani et al. (2024) BMC Public Health 10.1186/s12889-024-19594-4 (PMC11373141) — fsQCA, 38 países de la
# OCDE (tasa de letalidad de COVID-19, CFR). Datos: Additional file 3 (pertenencias calibradas, 2 decimales;
# descargado del servidor de suplementos de Springer). Conjuntos: D = democracia alta (6 valores), LC = capacidad
# estatal baja, LT = confianza baja, A = edad mediana alta, LH = gasto sanitario bajo; anclas en la Tabla 2.
# Soluciones intermedias: Tabla 5 y Additional file 4 (CFR alta: 2 términos; CFR baja: 3 términos, donde
# «high C» = ~LC, «high T» = ~LT, «high H» = ~LH y «~high H» = LH).
# RESULTADO: NO REPLICABLE. Las pertenencias publicadas coinciden con la recalibración de los datos brutos
# (Additional file 2) con las anclas de la Tabla 2, pero ningún término de 1-5 condiciones (búsqueda exhaustiva
# 3^5) alcanza las coberturas publicadas 0,781 o 0,474; dif. hasta 0,13. Se borró el .rds del corpus.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r95_PMC11373141/fuzzy.csv", check.names = FALSE)
d <- data.frame(D = r[[5]], LC = r[[6]], LT = r[[7]], A = r[[8]], LH = r[[9]], CFR = r[[10]])
d <- as.data.frame(lapply(d, as.numeric))
cat("N =", nrow(d), "; casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nCFR <- 1 - d$CFR
cd <- c("D", "LC", "LT", "A", "LH")
hi <- c("LC*LT*LH", "D*LC*LT*A")
lo <- c("D*~LC*A", "D*~LT*A*LH", "D*~LT*~LC*~LH")
r1 <- replicar_cs(d[, c(cd, "CFR")], "CFR", hi, id = "R95 Paykani 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.890, 0.872), cov_pub = c(0.781, 0.474)))
r2 <- replicar_cs(d[, c(cd, "nCFR")], "nCFR", lo, id = "R95 Paykani 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.929, 0.928, 0.970), cov_pub = c(0.532, 0.165, 0.598)))
saveRDS(list(alta = r1, baja = r2), "replicas/r95_paykani_2024.rds")
