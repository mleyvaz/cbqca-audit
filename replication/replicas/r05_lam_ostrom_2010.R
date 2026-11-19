# R05 (csQCA). Lam & Ostrom (2010), Policy Sciences 43: 1-25. 15 sistemas de riego (Nepal).
# Solución publicada, Eq. 1 (p. 19): W = ACR + alCf + AlRF + LCRF; nota 10: consistencia y cobertura de la solución = 1.
source("replicas/csqca_comun.R")
suppressPackageStartupMessages(library(cna))
data(d.irrigate, package = "cna"); d <- as.data.frame(d.irrigate)
print(d)
# Tabla 8 del artículo: A R F L C, n(W), n(w)
t8 <- rbind(c(0,1,0,1,1,1,1),c(0,1,1,1,1,2,0),c(1,1,0,1,1,2,0),c(1,1,1,1,1,2,0),c(0,0,0,0,1,1,0),
  c(0,1,0,0,1,1,0),c(0,0,1,1,1,0,1),c(1,0,0,0,0,0,1),c(1,1,1,0,0,1,0),c(1,1,0,0,1,1,0),c(1,1,1,0,1,1,0))
exp8 <- do.call(rbind, lapply(1:nrow(t8), function(i) rbind(
  matrix(rep(c(t8[i,1:5],1), t8[i,6]), ncol=6, byrow=TRUE), matrix(rep(c(t8[i,1:5],0), t8[i,7]), ncol=6, byrow=TRUE))))
key <- function(m) unname(sort(apply(m, 1, paste, collapse = "")))
cat("Tabla 8 coincide con d.irrigate:", identical(key(exp8), key(as.matrix(d[, c("A","R","F","L","C","W")]))), "\n")
r <- replicar_cs(d[, c("A","R","F","L","C","W")], "W", c("A*C*R", "~A*~L*C*~F", "A*~L*R*F", "L*C*R*F"),
                 "Lam & Ostrom 2010 | W | Eq. 1")
saveRDS(r, "replicas/r05_lam_ostrom_2010.rds")
