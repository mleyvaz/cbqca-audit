# R08 (fsQCA). Xiao et al. (2026), PLOS ONE, doi 10.1371/journal.pone.0340674. N = 273 usuarios.
# Datos calibrados publicados (S1). Soluciones leídas de las imágenes de las tablas t006 y t007.
source("replicas/csqca_comun.R")
suppressPackageStartupMessages(library(readxl))
X <- as.data.frame(read_excel("datos/xiao_2026/s001.xlsx", sheet = 1))
X <- X[complete.cases(X), ]
cat("N =", nrow(X), "| casos con HIA = 0,5 exacto:", sum(X$HIA == 0.5), "\n")
nec <- function(x, y) round(c(incl = sum(pmin(x, y)) / sum(y), cov = sum(pmin(x, y)) / sum(x)), 4)
cat("Control necesidad (publ. SE 0,7690/0,7165; ES 0,8234/0,7794):\n"); print(rbind(SE = nec(X$SE, X$HIA), ES = nec(X$ES, X$HIA)))
H <- c("SE*HIL*IV*~IA*TS*ES*CE*GRS", "SE*HIL*IV*~IO*TS*ES*CE*GRS", "SE*IV*IO*IA*TS*ES*CE*~GRS",
       "SE*HIL*IV*IO*IA*~TS*ES*CE*GRS", "SE*HIL*IV*IO*IA*TS*ES*~CE*GRS", "~SE*~HIL*~IV*IO*IA*TS*ES*CE*~GRS")
pubH <- data.frame(term = H, incl_pub = c(0.9465, 0.9528, 0.9674, 0.9706, 0.9764, 0.9738),
                   cov_pub = c(0.3749, 0.3677, 0.3120, 0.3117, 0.2932, 0.2728))
rH <- replicar_cs(X, "HIA", H, "Xiao 2026 | ansiedad alta", pubH)
X$NHIA <- 1 - X$HIA
L <- c("SE*HIL*~IO*~IA*~TS*~ES*~CE*GRS", "~SE*~HIL*~IV*IO*IA*~TS*~ES*~CE*~GRS")
pubL <- data.frame(term = L, incl_pub = c(0.9245, 0.9445), cov_pub = c(0.3850, 0.3176))
rL <- replicar_cs(X[, setdiff(names(X), "HIA")], "NHIA", L, "Xiao 2026 | sin ansiedad", pubL)
saveRDS(list(rH, rL), "replicas/r08_xiao_2026.rds")
