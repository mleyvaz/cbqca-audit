# R45 (csQCA). Rajkumar et al. 2023, Heliyon 9:e13778, 10.1016/j.heliyon.2023.e13778 (PMC9931422).
# Disposición a vacunarse contra la COVID-19 (India), N = 30 entrevistados (15 dispuestos, 15 no).
# Datos: Tabla 3 (los 30 casos, 9 condiciones crisp codificadas de entrevistas, resultado).
# Solución (Tabla 4, 5 caminos, términos escritos en la tabla), consistencia 1 en todos.
source("replicas/csqca_comun.R")
d <- read.table(header = TRUE, text = "
KAV PSUS PSEV FAM MED SOC TRS FEA EFF W
1 1 1 1 1 1 1 0 1 1
1 1 1 1 1 1 0 0 0 1
1 1 1 1 1 0 0 0 1 1
1 1 1 1 0 1 1 0 1 1
1 1 1 1 1 1 1 1 1 1
1 0 1 1 1 1 1 1 1 1
1 1 1 1 1 1 1 0 1 1
1 1 1 1 1 1 1 1 1 1
1 0 1 1 1 1 1 0 0 1
1 1 1 1 1 1 1 0 1 1
1 1 1 1 0 1 1 0 1 1
1 0 1 1 1 1 1 0 1 1
1 1 1 1 1 0 1 0 1 1
1 1 1 1 1 1 1 0 1 1
1 1 1 1 1 1 1 0 1 1
0 0 0 0 1 0 0 1 0 0
0 1 0 0 1 0 1 1 0 0
0 0 0 0 1 0 0 1 0 0
0 1 0 1 1 0 0 1 0 0
1 1 0 1 0 0 0 0 1 0
0 1 1 0 1 0 0 0 0 0
0 0 1 0 0 0 0 1 0 0
1 0 0 1 1 0 1 1 1 0
0 0 0 1 1 0 0 1 0 0
0 1 0 0 1 0 0 1 0 0
0 0 1 1 0 0 1 1 0 0
1 0 0 1 0 0 1 1 1 0
0 1 0 0 1 0 0 1 0 0
1 0 0 0 1 0 1 1 1 0
0 0 0 0 0 0 0 1 0 0
")
cat("N =", nrow(d), "| dispuestos:", sum(d$W), "\n")
t1 <- c("KAV*PSEV*FAM*MED*SOC*TRS*EFF", "KAV*~PSUS*PSEV*FAM*MED*SOC*TRS*~FEA",
        "KAV*PSUS*PSEV*FAM*MED*~SOC*~FEA*EFF", "KAV*PSUS*PSEV*FAM*SOC*TRS*~FEA*EFF",
        "KAV*PSUS*PSEV*FAM*MED*SOC*~TRS*~FEA*~EFF")
r1 <- replicar_cs(d, "W", t1, id = "R45 Rajkumar 2023 (vacunacion)",
  publicados = data.frame(term = t1, incl_pub = 1, cov_pub = c(.6, .133333, .133333, .466667, .0666667)))
saveRDS(list(r1), "replicas/r45_rajkumar_2023.rds")
