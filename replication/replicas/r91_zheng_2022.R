# R91 Zheng et al. (2022) PLOS ONE 10.1371/journal.pone.0274307 — fsQCA, 63 maker-spaces (desempeño innovador, QIP).
# Datos: S1 Data (brutos + pertenencias fs*; FL, AS, TE binarias). La necesidad (Tabla 5) se reproduce EXACTA
# (NSF 0,93189 / 0,53976, etc.), pero la Tabla 6 es incoherente: sus símbolos (〇 ◎ ●) no están definidos y
# las cifras no corresponden a ninguna lectura: C2 = NSF*RGC*FL*AS*TE da incl 0,9376 (= publicado) pero
# cobertura 0,5524 (publicada para C1); NSF*RGC*~HRI*FL*AS da 0,8431 / 0,2324 (consistencia y cobertura única
# publicadas para C1). Cobertura y consistencia de la solución aparecen intercambiadas. NO REPLICABLE.
suppressMessages(library(QCA))
r <- read.csv("datos/r91_pone0274307/datos.csv")
d <- data.frame(NSF = r$fsNSF, RGC = r$fsRGC, HRI = r$fsHRI, FL = r$FL, AS = r$AS, TE = r$TE, QIP = r$fsQIP)
print(round(pof("NSF+RGC+HRI+FL+AS+TE", "QIP", data = d, relation = "necessity")$incl.cov[1:6, c(1, 3)], 5))
print(round(pof("NSF*RGC*HRI*FL*AS + NSF*RGC*FL*AS*TE + NSF*RGC*~HRI*FL*AS", "QIP", data = d, relation = "sufficiency")$incl.cov[, c(1, 3)], 4))
