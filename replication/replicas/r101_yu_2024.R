# R101 Yu, Zhao, Wang et al. (2024) PLOS ONE 10.1371/journal.pone.0302700 — fsQCA, lógicas causation/effectuation y desempeño de pymes
# de software, 344 empresas (166 start-ups <= 8 años; 178 establecidas). Datos: S2 File (minimal data set, ítems).
# Constructo = media de ítems. Anclas: Tabla 2 (percentiles 95/50/5); 0,5 -> 0,501 como los autores.
# Software no declarado -> calibración por defecto del paquete QCA. Frecuencia 3. Solución: Tabla 4.
source("replicas/csqca_comun.R")
it <- read.csv("datos/r101_pone0302700/minimal.csv")
it <- it[!is.na(it$number), ]
mk <- function(p) rowMeans(it[, grep(paste0("^", p, "[0-9]"), names(it))])
m <- data.frame(CAU = mk("cau"), EX = mk("ex"), FL = mk("fl"), AL = mk("al"), PR = mk("pra"), FP = mk("firmper"))
cat("N =", nrow(m), "; start-ups (firmage <= 8):", sum(it$firmage <= 8), "\n")
anc <- list(CAU = c(3.02, 4, 4.86), EX = c(2, 3.33, 4.33), FL = c(3.25, 4, 4.75), AL = c(2.38, 4, 4.95),
            PR = c(2.83, 4, 4.67), FP = c(3.67, 5.33, 6.33))
cat("Percentiles 5/50/95 (tipo 7):\n"); print(round(t(sapply(m, quantile, c(0.05, 0.5, 0.95))), 2))
d <- as.data.frame(lapply(names(anc), function(k) calibrate(m[[k]], type = "fuzzy", thresholds = anc[[k]])))
names(d) <- names(anc); d[d == 0.5] <- 0.501
su <- it$firmage <= 8
cat("Necesidad (muestra total; publicado CAU 0,79, EX 0,71, FL 0,78, AL 0,66, PR 0,73):\n")
print(round(sapply(names(anc)[1:5], function(k) sum(pmin(d[[k]], d$FP)) / sum(d$FP)), 3))
r1 <- replicar_cs(d[su, ], "FP", "CAU*EX*FL", id = "R101 Yu 2024 (start-ups)",
  publicados = data.frame(term = "CAU*EX*FL", incl_pub = 0.860, cov_pub = 0.564))
est <- c("CAU*EX*FL", "CAU*FL*~AL*~PR", "CAU*~EX*AL*PR", "CAU*EX*AL*~PR")
r2 <- replicar_cs(d[!su, ], "FP", est, id = "R101 Yu 2024 (establecidas)",
  publicados = data.frame(term = est, incl_pub = c(0.840, 0.893, 0.894, 0.882), cov_pub = c(0.590, 0.397, 0.364, 0.381)))
saveRDS(list(startups = r1, establecidas = r2), "replicas/r101_yu_2024.rds")
# CLASIFICACIÓN: aproximada — start-ups y E1a/E1b cuadran (<= 0,004), pero E2 y E3 tienen los valores intercambiados en la Tabla 4 (dif. 0,014 / 0,019 tal como se publican; con el intercambio cuadrarían <= 0,005). No es errata de signo.
