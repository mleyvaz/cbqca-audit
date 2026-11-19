# R42 (csQCA). Kemp et al. 2021, SSM - Mental Health 1:100009, 10.1016/j.ssmmh.2021.100009 (PMC8443051).
# Barreras y facilitadores de la identificación de depresión por enfermeras de APS (KwaZulu-Natal), N = 68.
# Datos: Apéndice A (mmc1.xlsx, Elsevier), tabla de verdad completa (32 filas) con nº de casos y
# consistencia por fila; casos con resultado = round(n x consistencia) (suma 24 = Tabla 3). Calibración
# declarada en la Tabla 1. Solución parsimoniosa excluyendo los supuestos contradictorios (filas 9 y 11),
# corte 0,75 (Tabla 4). La Tabla 4 publica SOLO el ajuste de la solución (0,92/0,46 y 0,94/0,77), no
# por término; se compara la expresión completa (fila «expression»).
source("replicas/csqca_comun.R")
tt <- read.table(header=TRUE, text="
row TR CO AIM ORIC NRO n cons
1 0 0 0 0 0 5 0
2 0 0 0 0 1 5 0
3 0 0 0 1 0 1 0
4 0 0 0 1 1 5 0.2
5 0 0 1 0 0 1 0
8 0 0 1 1 1 2 0
12 0 1 0 1 1 3 0.3333
13 0 1 1 0 0 1 1
14 0 1 1 0 1 1 0
15 0 1 1 1 0 3 1
16 0 1 1 1 1 3 0.6667
17 1 0 0 0 0 2 0
18 1 0 0 0 1 2 0
19 1 0 0 1 0 3 0.6667
20 1 0 0 1 1 2 0
21 1 0 1 0 0 2 0
24 1 0 1 1 1 5 0.2
25 1 1 0 0 0 2 0
27 1 1 0 1 0 1 0
28 1 1 0 1 1 1 1
29 1 1 1 0 0 3 0.6667
30 1 1 1 0 1 1 1
31 1 1 1 1 0 8 0.5
32 1 1 1 1 1 6 0.8333")
d <-do.call(rbind, lapply(seq_len(nrow(tt)), function(i) { k <- round(tt$n[i] * tt$cons[i])
  cbind(tt[rep(i, tt$n[i]), 2:6], REF = c(rep(1, k), rep(0, tt$n[i] - k)))})); rownames(d) <- NULL
cat("N =", nrow(d), "| 100 % de derivación:", sum(d$REF), "\n")
d$NREF <- 1 - d$REF
cs <- c("TR", "CO", "AIM", "ORIC", "NRO")
t1 <- c("TR*CO*NRO", "~TR*CO*AIM*~NRO")
r1 <- replicar_cs(d[, c(cs, "REF")], "REF", t1, id = "R42 Kemp 2021 (100 % derivacion)",
  publicados = data.frame(term = "expression", incl_pub = .92, cov_pub = .46))
t2 <- c("~TR*~CO", "~CO*~ORIC", "~CO*NRO", "~TR*~ORIC*NRO", "TR*CO*~AIM*~NRO")
r2 <- replicar_cs(d[, c(cs, "NREF")], "NREF", t2, id = "R42 Kemp 2021 (menos de 100 %)",
  publicados = data.frame(term = "expression", incl_pub = .94, cov_pub = .77))
# Comprobación: la minimización de los autores (excluyendo filas 9 y 11) da exactamente estos términos.
tA <- truthTable(d[, c(cs, "REF")], "REF", incl.cut = .75, complete = TRUE)
print(minimize(tA, include = "?", exclude = c(9, 11)))
tB <- truthTable(d[, c(cs, "NREF")], "NREF", incl.cut = .75, complete = TRUE)
print(minimize(tB, include = "?", exclude = c(9, 11)))
saveRDS(list(r1, r2), "replicas/r42_kemp_2021.rds")
