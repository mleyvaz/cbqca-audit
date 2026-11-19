# R33 Hu et al. (2025) PLOS ONE 10.1371/journal.pone.0329826 — fsQCA, 33 empresas de artículos deportivos (servitización).
# Datos: S1 (s001.xlsx): SV resultado; DIG, INN, MAN, STA, OS, SUB, MAR condiciones (datos brutos).
# Anclas: Tabla 3 (percentiles 95/50/5). Solución: Tabla 5 (A1, A2, B, C; solo resultado alto).
source("replicas/csqca_comun.R")
x <- as.data.frame(readxl::read_excel("datos/r33_pone0329826/s001.xlsx", 1))
v <- c("SV", "DIG", "INN", "MAN", "STA", "OS", "SUB", "MAR")
raw <- x[, v]
anc <- list(SV = c(0.018, 0.091, 0.761), DIG = c(0.693, 2.398, 4.454), INN = c(1.076, 3.634, 6.471),
            MAN = c(0.194, 0.775, 2.182), STA = c(0.021, 0.130, 0.573), OS = c(0.041, 0.169, 0.557),
            SUB = c(0.103, 2.834, 28.713), MAR = c(11.163, 14.846, 18.617))
cat("Percentiles tipo 7 recalculados (5/50/95):\n"); print(round(sapply(raw, quantile, c(.05, .5, .95)), 3))
d <- as.data.frame(lapply(v, function(k) round(calibrate(raw[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- v
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
hi <- c("~DIG*INN*~MAN*STA*OS*~SUB*MAR", "~DIG*INN*MAN*STA*OS*~SUB*~MAR",
        "DIG*~INN*~MAN*STA*OS*~SUB*~MAR", "DIG*~INN*MAN*STA*~OS*SUB*~MAR")
r1 <- replicar_cs(d[, c(v[-1], "SV")], "SV", hi, id = "R33 Hu 2025 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.909, 0.947, 0.952, 0.900),
                          cov_pub = c(0.287, 0.256, 0.263, 0.236)))
saveRDS(list(alta = r1), "replicas/r33_hu_2025.rds")
