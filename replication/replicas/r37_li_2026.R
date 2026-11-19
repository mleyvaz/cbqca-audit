# R37 Li et al. (2026) PLOS ONE 10.1371/journal.pone.0340629 — fsQCA, 35 empresas de semiconductores.
# Datos: Zenodo 10.5281/zenodo.15812073 (research data.xlsx; datos brutos: el archivo NO trae puntajes calibrados
# pese a lo que dice Data Availability). HCL = % personal de I+D; ES y GS en log10 (así cuadran las anclas 50/75).
# Anclas: Tabla 3 (exclusión, cruce, inclusión). El texto dice percentiles 25/50/75; cruce e inclusión coinciden
# con los percentiles 50/75, pero las anclas de exclusión publicadas NO son el percentil 25. Con las anclas de la
# Tabla 3 no cuadra (difs. de hasta 0,12); con los percentiles 25/50/75 DECLARADOS en el texto sí (<= 0,005).
# Se usan los percentiles (tipo 7). Escala log10 de ES y GS inferida de las anclas de la Tabla 3.
# Solución: Tabla 5 (S1a, S1b, S2a, S2b, S3), resultado TFP alto.
source("replicas/csqca_comun.R")
x <- as.data.frame(readxl::read_excel("datos/li_2026_semiconductores/research_data.xlsx"))
x <- x[!is.na(x[[1]]), ]
raw <- data.frame(TIC = as.numeric(x[[4]]), DT = as.numeric(x[[5]]), OR = as.numeric(x[[6]]),
                  HCL = as.numeric(sub("%", "", x[[7]])) / 100, ES = log10(as.numeric(x[[8]])),
                  GS = log10(as.numeric(x[[9]])), TFP = as.numeric(x[[10]]))
cat("Percentiles 25/50/75 recalculados:\n"); print(round(sapply(raw, quantile, c(.25, .5, .75)), 3))
anc_tabla3 <- list(TIC = c(4, 18, 39), DT = c(1.609, 3.135, 3.526), OR = c(0.843, 0.87, 0.882),
            HCL = c(0.183, 0.622, 0.737), ES = c(9.191, 9.675, 9.977), GS = c(7.077, 7.457, 7.649),
            TFP = c(7.724, 8.694, 8.974))  # no se usan (exclusión inconsistente con el texto)
anc <- lapply(raw, function(z) unname(quantile(z, c(.25, .5, .75))))
v <- names(raw)
d <- as.data.frame(lapply(v, function(k) round(calibrate(raw[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- v
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
hi <- c("TIC*OR*ES*GS", "TIC*~DT*~HCL*ES*GS", "TIC*~OR*HCL*ES*~GS", "TIC*DT*~OR*HCL*~GS", "DT*OR*HCL*ES*GS")
r1 <- replicar_cs(d, "TFP", hi, id = "R37 Li 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.978793, 0.974747, 0.920732, 0.869074, 0.990446),
                          cov_pub = c(0.331309, 0.213142, 0.166759, 0.21259, 0.171728)))
saveRDS(list(alta = r1), "replicas/r37_li_2026.rds")
