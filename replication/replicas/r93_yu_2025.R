# R93 Yu et al. (2025) PLOS ONE 10.1371/journal.pone.0316706 — fsQCA, 274 espectadores de la CBA (experiencia
# del espectador). Datos: S2 Appendix (ítems Likert). Anclas declaradas: media ± 1 DE (cruce = media); la
# calibración es invariante a usar suma o media de ítems. Ni la necesidad (Sense 0,718 frente a 0,751 publicado)
# ni la Tabla 11 se reproducen (p. ej., camino 1a: cobertura 0,427 frente a 0,498), con calibración QCA o estilo
# fsQCA. Probablemente usaron puntuaciones de constructo no publicadas. NO REPLICABLE: no se registra en el corpus.
suppressMessages(library(QCA))
it <- read.csv("datos/r93_pone0316706/items.csv")
pre <- c(Sense = "GG", Feel = "QG", Think = "SK", Act = "XD", Relate = "GL", Serv = "QMFW", SE = "CXGS")
m <- as.data.frame(lapply(pre, function(p) rowMeans(it[, grep(paste0("^", p, "[0-9]"), names(it))])))
cat("N =", nrow(m), "\n")
hi <- c("Sense*Feel*Act*Serv", "Feel*Act*Relate*Serv", "Sense*Think*Act*Serv", "Sense*Feel*Think*~Relate*Serv", "Think*Act*Relate*Serv", "~Sense*Feel*Think*Relate*Serv")
for (fs in c(FALSE, TRUE)) {
 d <- as.data.frame(lapply(m, function(x) { a <- c(mean(x) - sd(x), mean(x), mean(x) + sd(x))
   y <- calibrate(x, type = "fuzzy", thresholds = a, idm = if (fs) plogis(3) else 0.95); if (fs) round(y, 2) else y }))
 cat("fs", fs, "casos 0,5:", sum(d == 0.5), "\n")
 print(round(pof(paste(hi, collapse = "+"), "SE", data = d, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
 print(round(pof("Sense+Feel+Think+Act+Relate+Serv", "SE", data = d, relation = "necessity")$incl.cov[1:6, c(1, 3)], 3)) }
