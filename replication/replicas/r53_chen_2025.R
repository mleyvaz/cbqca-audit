# R53 Chen et al. (2025) BMC Nursing 10.1186/s12912-025-04170-x — fsQCA, 269 artículos de divulgación de enfermería en WeChat.
# Datos: Supplementary Material 1 (12912_2025_4170_MOESM1_ESM.xlsx, servidor static-content.springer.com), datos crudos.
# Anclas: Tabla 3 (percentiles 5/50/95 para CC, IE y efecto; OS 0/1/3; PT 3/4/7; dicotómicas sin calibrar).
# El texto menciona además una asignación graduada de OS (0,05/0,35/0,65/0,95); esa variante NO cuadra (registrada abajo);
# la Tabla 3 sí. Frecuencia 3, consistencia 0,80, PRI 0,70. Solución: Tabla 5 (H1-H8).
source("replicas/csqca_comun.R")
r <- read.csv("datos/chen_2025_nursing/datos.csv")
d <- data.frame(TO = r$TO, PM = r$PM, TS = r$TS, TT = r$TT, PP = r$PP)
d$CC <- round(calibrate(r$CC, type = "fuzzy", thresholds = c(2.17, 5.28, 8.33)), 3)
d$IE <- round(calibrate(r$IE, type = "fuzzy", thresholds = c(0.20, 0.27, 0.39)), 3)
d$OS <- round(calibrate(r$OS, type = "fuzzy", thresholds = c(0, 1, 3)), 3)
d$PT <- round(calibrate(r$PT, type = "fuzzy", thresholds = c(3, 4, 7)), 3)
d$CE <- round(calibrate(r$CE, type = "fuzzy", thresholds = c(350.46, 503.50, 749.14)), 3)
cd <- c("TO", "CC", "IE", "OS", "PM", "PT", "TS", "TT", "PP")
hi <- c("~CC*IE*~OS*PM*PT*~TS*~TT*PP", "~TO*~CC*IE*~OS*PM*PT*~TT*PP", "TO*CC*~IE*OS*~PM*PT*~TT*PP",
        "TO*CC*~IE*OS*~PM*PT*~TS*PP", "TO*~CC*IE*OS*~PM*~PT*TS*~TT*~PP", "~TO*CC*~IE*~OS*~PM*PT*~TS*TT*PP",
        "~TO*CC*IE*~OS*~PM*PT*TS*~TT*PP", "~TO*CC*IE*OS*~PM*PT*~TS*TT*PP")
r1 <- replicar_cs(d[, c(cd, "CE")], "CE", hi, id = "R53 Chen 2025 (divulgación de enfermería)",
  publicados = data.frame(term = hi, incl_pub = c(1, 0.989, 1, 0.995, 0.881, 1, 0.815, 0.976),
                          cov_pub = c(0.038, 0.029, 0.038, 0.050, 0.043, 0.032, 0.011, 0.020)))
saveRDS(list(alta = r1), "replicas/r53_chen_2025.rds")
# Variante con OS graduado (0,05/0,35/0,65/0,95, descrita en el texto): H3, H4, H8 difieren hasta 0,024 -> descartada.
# CLASIFICACIÓN: exacta (dif. máx. 0,002 consistencia, 0,001 cobertura).
