# R04 (csQCA). Krook (2010), Political Studies 58(5): 886-908. "The West", 22 países.
# Soluciones publicadas (pp. 895-897), notación del artículo: PR=ES, QU, WS, WM, L=LP.
source("replicas/csqca_comun.R")
data(Krook)
d <- Krook
# Verificación contra la Tabla 2 del artículo (orden de filas = Suecia ... Irlanda)
tab2 <- rbind(c(1,1,1,0,0,1),c(1,0,1,0,0,1),c(1,1,1,1,1,1),c(1,0,1,1,1,1),c(1,1,0,1,1,1),
  c(1,1,0,1,0,1),c(1,1,0,1,1,1),c(1,1,0,0,1,1),c(0,0,0,1,1,1),c(1,1,1,1,1,1),c(0,1,0,1,1,1),
  c(1,1,0,0,0,0),c(0,1,0,1,0,0),c(1,0,0,0,1,0),c(1,1,0,0,0,0),c(0,0,0,1,0,0),c(0,1,0,1,0,0),
  c(0,1,0,1,0,0),c(0,1,0,0,0,0),c(0,0,0,1,0,0),c(1,1,0,0,0,0),c(0,1,0,1,0,0))
key <- function(m) unname(sort(apply(m, 1, paste, collapse = "")))
cat("Tabla 2 coincide con el conjunto de datos (multiconjunto de filas):",
    identical(key(tab2), key(as.matrix(d[, c("ES","QU","WS","WM","LP","WNP")]))), "\n")
d$NWNP <- 1 - d$WNP
# 1) Solución compleja (solo casos observados), resultado alto
compleja <- c("ES*WS*~WM*~LP", "ES*WS*WM*LP", "ES*QU*~WS*WM", "ES*QU*~WS*LP", "~ES*~WS*WM*LP")
r1 <- replicar_cs(d[, c("ES","QU","WS","WM","LP","WNP")], "WNP", compleja, "Krook 2010 | WNP | compleja")
# 2) Soluciones con remanentes (parsimoniosas), resultado alto
pars <- c("WS", "ES*WM", "QU*LP", "~ES*LP", "WM*LP")
r2 <- replicar_cs(d[, c("ES","QU","WS","WM","LP","WNP")], "WNP", pars, "Krook 2010 | WNP | con remanentes (dos modelos)")
# 3) Solución compleja, resultado bajo (~WNP)
neg <- c("QU*~WS*~WM*~LP", "~ES*~WS*WM*~LP", "ES*~QU*~WS*~WM*LP")
r3 <- replicar_cs(d[, c("ES","QU","WS","WM","LP","NWNP")], "NWNP", neg, "Krook 2010 | ~WNP | compleja")
saveRDS(list(r1, r2, r3), "replicas/r04_krook_2010.rds")
