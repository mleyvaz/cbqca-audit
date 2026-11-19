# R40 (csQCA). Fürrutter 2023, The Review of International Organizations 18:161-185,
# 10.1007/s11558-022-09458-0 (PMC9800347). Diseño de las medidas restrictivas de la UE (19 episodios).
# Datos: datos crudos (ESM3, xlsx) y script R de los autores (ESM1). Calibración crisp declarada:
# ECON = FinancialRestrictions + SectorCommodities >= 1; SAL: Salience >= 100; USE: USeco >= 1;
# FAT: Deaths >= 1; CAP: vector fijado a mano en el script de los autores (Ukraine3 = 1).
# Solución conservadora (texto y Tabla 4 del apéndice): CAP*USE*FAT + CAP*FAT*SAL + ~CAP*USE*~FAT*~SAL.
# Negación (apéndice): ~USE*~SAL -> ~ECON (consistencia y cobertura 1).
source("replicas/csqca_comun.R")
raw <- read.table(header = TRUE, text = "
caso Salience USeco Deaths Fin Com
Belarus 31.75 0 0 0 0
Bosnia 10 0 0 0 0
Burundi 0 0 92 0 0
Egypt 43 0 0 0 0
Guinea 2.5 0 0 0 0
Iran 256.8 1 2 0 1
Moldova 0 0 0 0 0
Myanmar1 3 0 19 0 0
Myanmar2 122 0 11 0 1
Nicaragua 31 0 0 0 0
Russia 0 1 2 1 1
Syria 344.78 1 47758.15 1 1
Tunisia 10 0 0 0 0
Ukraine1 0 0 887 0 0
Ukraine2 0 0 887 0 0
Ukraine3 0 1 887 1 1
Venezuela 42 1 0 0 1
Zimbabwe1 56 0 0 0 0
Zimbabwe2 28 0 0 0 0")
d <- data.frame(
  CAP = c(0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0),  # fijado por los autores
  FAT = as.integer(raw$Deaths >= 1),
  SAL = as.integer(raw$Salience >= 100),
  USE = as.integer(raw$USeco >= 1),
  ECON = as.integer(raw$Fin + raw$Com >= 1))
rownames(d) <- raw$caso
d$nECON <- 1 - d$ECON
print(d)
conds <- c("CAP", "FAT", "SAL", "USE")
t1 <- c("CAP*USE*FAT", "CAP*FAT*SAL", "~CAP*USE*~FAT*~SAL")
r1 <- replicar_cs(d[, c(conds, "ECON")], "ECON", t1, id = "R40 Furrutter 2023 (economicas)",
  publicados = data.frame(term = t1, incl_pub = c(1, 1, 1), cov_pub = c(.667, .500, .167)))
r2 <- replicar_cs(d[, c(conds, "nECON")], "nECON", "~USE*~SAL", id = "R40 Furrutter 2023 (no economicas)",
  publicados = data.frame(term = "~USE*~SAL", incl_pub = 1, cov_pub = 1))
saveRDS(list(r1, r2), "replicas/r40_furrutter_2023.rds")
