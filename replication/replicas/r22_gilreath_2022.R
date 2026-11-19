# R22 (csQCA). Gilreath et al. 2022, Frontiers in Psychology, 10.3389/fpsyg.2022.948474 (PMC9806339).
# Afrontamiento desadaptativo en adolescentes de familias militares (24 participantes).
# Datos: tabla de verdad (Tabla 2) con N por fila. El texto dice: 4 con el resultado, 17 sin él y 3
# intermedios (0,5 o menos) asignados al conjunto de exclusión, de modo que el resultado crisp es 1 solo
# en las filas 5, 6, 8 y 10. La «Incl.» publicada usa membresías intermedias: APROXIMADA en consistencia.
# Solución (texto, Resultados): YESBBULL*NEGMCY*ACAPROB + YESBBULL*~MFMPTSD*~NEGMCY*~ACAPROB
#                               + ~YESBBULL*MFMPTSD*~NEGMCY*ACAPROB.
source("replicas/csqca_comun.R")
tt <- read.table(header = TRUE, text = "
YESBBULL MFMPTSD NEGMCY ACAPROB OUT n
0 0 0 0 0 10
0 0 0 1 0 2
0 0 1 0 0 2
0 1 0 0 0 1
0 1 0 1 1 1
1 0 0 0 1 1
1 0 0 1 0 3
1 0 1 1 1 1
1 1 0 0 0 2
1 1 1 1 1 1")
d <- tt[rep(seq_len(nrow(tt)), tt$n), 1:5]; rownames(d) <- NULL; names(d)[5] <- "MALCOPE"
terms <- c("YESBBULL*NEGMCY*ACAPROB", "YESBBULL*~MFMPTSD*~NEGMCY*~ACAPROB", "~YESBBULL*MFMPTSD*~NEGMCY*ACAPROB")
r1 <- replicar_cs(d, "MALCOPE", terms, id = "R22 Gilreath 2022 (afrontamiento desadaptativo)",
  publicados = data.frame(term = terms, incl_pub = NA, cov_pub = NA))
saveRDS(list(r1), "replicas/r22_gilreath_2022.rds")
