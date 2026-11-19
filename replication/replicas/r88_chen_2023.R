# R88 Chen et al. (2023) PLOS ONE 10.1371/journal.pone.0282617 — fsQCA, 80 países (mortalidad temprana por COVID-19,
# tasa de años de vida perdidos, YLL). Datos: S2 Table (pertenencias calibradas publicadas; los autores sumaron
# 0,001 a densidad, ingreso y YLL). Anclas: Tabla 2 (percentiles 95/50/5); EPI binaria.
# Soluciones: Tablas 3 (YLL alta, 1-4) y 4 (YLL baja, 5-8), publicadas con 2 decimales.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r88_pone0282617/calibrados.csv")
d <- r[, c("DEL", "EPI", "ELD", "DEN", "INC", "YLL")]
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nYLL <- 1 - d$YLL
cd <- c("DEL", "EPI", "ELD", "DEN", "INC")
hi <- c("DEN*INC", "~EPI*ELD*DEN", "EPI*~ELD*INC", "DEL*EPI*INC")
lo <- c("~EPI*~DEN", "~ELD*DEN*~INC", "~DEL*~EPI*INC", "~DEL*EPI*ELD*~INC")
r1 <- replicar_cs(d[, c(cd, "YLL")], "YLL", hi, id = "R88 Chen 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.79, 0.76, 0.84, 0.90), cov_pub = c(0.49, 0.23, 0.21, 0.30)))
r2 <- replicar_cs(d[, c(cd, "nYLL")], "nYLL", lo, id = "R88 Chen 2023 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.81, 0.86, 0.74, 0.85), cov_pub = c(0.33, 0.40, 0.14, 0.13)))
saveRDS(list(alta = r1, baja = r2), "replicas/r88_chen_2023.rds")
