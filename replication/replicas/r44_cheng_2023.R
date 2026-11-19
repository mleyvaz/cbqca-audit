# R44 (csQCA). Cheng et al. 2023, Heliyon 9:e19305, 10.1016/j.heliyon.2023.e19305 (PMC10470225).
# «Model students» entre los medios oficiales chinos, N = 60. Datos crisp: Tabla 3 (60 medios).
# Solución intermedia (fsQCA 3.0), Tabla 5: ~AL*GP*~EO*~ET + ~AL*GP*~MD*EO + ~AL*GP*EO*ET + AL*GP*MD*ET
# + ~AL*~GP*MD*EO*~ET; solución 0,962963 / 0,742857.
# RESULTADO: NO REPLICABLE. La Tabla 3 tiene 37 casos con TS = 1 (21 con ~AL), pero la necesidad y las
# coberturas publicadas implican 35 (19 con ~AL): 1/35 = 0,0286. Coberturas de 3 términos difieren 0,05-0,06.
# No se corrige la tabla (habría que adivinar qué filas están mal). .rds borrado del corpus.
source("replicas/csqca_comun.R")
d <- read.table(header = TRUE, text = "
AL GP MD EO ET TS
1 1 1 1 1 1
1 1 1 1 1 1
1 1 1 1 1 1
1 1 0 1 0 0
1 1 1 1 1 1
1 1 1 0 1 1
1 1 1 0 0 0
1 1 1 1 1 1
1 1 1 0 0 0
1 1 1 1 0 0
1 1 0 1 1 0
1 1 1 1 1 1
1 1 0 0 1 0
1 1 1 0 0 0
1 1 1 1 1 1
1 1 0 0 1 0
1 1 1 1 1 0
1 1 1 1 0 0
1 1 1 1 1 1
1 1 1 1 1 1
1 1 1 1 0 1
1 1 1 1 1 1
1 1 1 1 1 1
1 1 1 0 1 1
1 1 0 1 0 0
1 1 1 1 0 1
1 1 1 1 1 1
1 0 1 0 1 0
1 0 0 0 0 0
1 0 1 0 1 0
0 1 1 1 1 1
0 1 1 1 1 1
0 1 1 0 0 1
0 1 0 0 0 1
0 1 1 1 1 1
0 1 0 0 0 1
0 1 0 0 0 1
0 1 1 0 0 1
0 1 0 0 1 1
0 1 1 1 1 1
0 1 0 0 1 1
0 1 0 1 0 1
0 1 0 1 0 1
0 1 0 0 1 0
0 0 1 1 1 1
0 0 1 1 1 1
0 0 0 1 0 0
0 0 1 1 1 1
0 0 0 1 0 0
0 0 1 1 0 1
0 0 1 1 1 1
0 0 1 1 1 1
0 0 1 1 1 1
0 0 1 1 1 0
0 0 1 1 1 0
0 0 0 1 1 0
0 0 0 1 1 0
0 0 1 1 1 0
0 0 0 1 1 1
0 0 0 1 1 0
")
cat("N =", nrow(d), "| TS:", sum(d$TS), "\n")
t1 <- c("~AL*GP*~EO*~ET", "~AL*GP*~MD*EO", "~AL*GP*EO*ET", "AL*GP*MD*ET", "~AL*~GP*MD*EO*~ET")
r1 <- replicar_cs(d, "TS", t1, id = "R44 Cheng 2023 (model students)",
  publicados = data.frame(term = c(t1, "expression"), incl_pub = c(1, 1, 1, .933333, 1, .962963),
                          cov_pub = c(.0857143, .114286, .171429, .4, .0285714, .742857)))
# Necesidad (Tabla 4) como control de los datos
print(round(QCA::pof(d[, c("AL", "GP", "MD", "EO", "ET")], "TS", data = d, relation = "necessity")$incl.cov[, 1:2], 6))
cat("Casos TS por AL:", tapply(d$TS, d$AL, sum), "
")
saveRDS(list(r1), "replicas/r44_cheng_2023.rds")
