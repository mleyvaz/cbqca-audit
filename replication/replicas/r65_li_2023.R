# R65 Li, Xinlan, Yixuan (2023) PLOS ONE 10.1371/journal.pone.0287819 — fsQCA, 30 provincias (CCD economía
# digital / eficiencia de carbono). Solución publicada (Tabla 7, imagen): H1a IS*ED*UL*OP;
# H1b ~TI*ER*IS*~ED*~UL*OP; H2 TI*ED*UL*OP; H3a TI*~ER*IS*~ED*~UL*~OP; H3b TI*ER*IS*ED*~UL*~OP.
# NO REPLICABLE: el S1 Dataset (panel 2011-2019) no trae la población residente, necesaria para UL
# (proporción de población urbana), y las definiciones del texto (ED = ln PIB per cápita, OP = comercio/PIB)
# no coinciden con la Tabla 5 (ED 16413-164220 y OP 37,58-71763 en niveles brutos). Tampoco se publican
# las medias provinciales ni los datos calibrados. Este script solo documenta lo disponible.
r <- read.csv("datos/r65_pone0287819/s002.csv", check.names = FALSE)
cat("Panel:", nrow(r), "filas x", ncol(r), "columnas\n"); print(names(r))
cat("Rango PIB per cápita (Tabla 5 ED: 16413-164220):\n"); print(range(r[["Per capita GDP"]]))
cat("Rango comercio exterior (Tabla 5 OP: 37,58-71763,36):\n"); print(range(r[["Total import and export volume"]]))
cat("Rango mercado tecnológico (Tabla 5 TI: 0,57-5695,28):\n"); print(range(r[["The transaction volume of the technology market"]]))
