# R47 (csQCA). Cheng et al. 2024, Heliyon 10:e35393, 10.1016/j.heliyon.2024.e35393 (PMC11336628).
# Sistemas multipartidistas en 44 Estados europeos. Datos crisp: Tabla 3 (44 casos).
# Solución intermedia (fsQCA 3.0), Tabla 5: 8 términos; solución 0,954545 / 0,7.
source("replicas/csqca_comun.R")
d <- read.table(header = TRUE, text = "
TP DE CC EDI SP MP
1 1 0 1 0 1
1 1 0 1 1 1
1 1 0 1 1 1
0 1 0 1 1 1
1 1 0 1 0 1
0 0 1 0 1 0
1 1 1 0 1 1
0 0 1 0 1 0
1 1 0 1 1 1
1 0 1 1 0 0
0 0 1 1 1 0
0 1 1 0 1 1
0 1 0 0 1 1
1 0 1 1 0 1
1 0 1 1 0 1
0 0 0 1 0 0
1 0 1 1 0 0
0 0 0 1 1 0
0 0 1 1 0 0
1 1 0 1 1 1
1 1 1 1 1 1
1 1 1 1 1 1
1 1 1 1 1 1
1 1 1 1 1 1
0 1 0 1 1 1
0 1 1 1 0 1
1 1 0 1 0 1
1 1 0 1 1 1
1 1 1 1 0 1
1 1 1 1 0 0
1 1 1 1 1 0
1 1 1 1 0 1
1 1 1 1 1 1
0 0 1 0 0 0
1 1 1 0 1 1
0 0 0 0 0 1
1 1 1 0 0 1
1 1 1 0 1 0
0 1 0 1 1 1
1 0 1 1 0 1
0 0 0 0 0 1
1 0 1 0 1 1
1 0 1 1 0 0
0 0 0 0 0 0
")
cat("N =", nrow(d), "| MP:", sum(d$MP), "\n")
t1 <- c("~TP*DE*~EDI*SP", "TP*DE*~CC*EDI", "TP*DE*EDI*SP", "TP*DE*CC*~EDI*~SP", "~TP*DE*CC*EDI*~SP",
        "TP*~DE*CC*~EDI*SP", "~TP*DE*~CC*SP", "DE*~CC*EDI*SP")
r1 <- replicar_cs(d, "MP", t1, id = "R47 Cheng 2024 (multipartidismo)",
  publicados = data.frame(term = c(t1, "expression"), incl_pub = c(1, 1, .909091, 1, 1, 1, 1, 1, .954545),
    cov_pub = c(.0666667, .266667, .333333, .0333333, .0333333, .0333333, .133333, .266667, .7)))
print(round(QCA::pof(d[, c("TP", "DE", "CC", "EDI", "SP")], "MP", data = d, relation = "necessity")$incl.cov[, 1:2], 6))
saveRDS(list(r1), "replicas/r47_cheng_2024.rds")
