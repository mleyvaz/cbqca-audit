# R06 (fsQCA). Chai & Yang (2026), PLOS ONE, doi 10.1371/journal.pone.0348315. N = 203 emprendedores artísticos.
# Datos calibrados publicados (S3 Data). Solución publicada: tabla «High-level and low-level EI configurations».
source("replicas/csqca_comun.R")
d <- read.csv("datos/chai_yang_2026/s003.csv")
cat("N =", nrow(d), "| columnas:", names(d), "\n")
# ¿nMG es MG o ~MG? Necesidad publicada para EI: MG incl 0,700 cov 0,769; ~MG incl 0,570 cov 0,537
nec <- function(x, y) c(incl = sum(pmin(x, y)) / sum(y), cov = sum(pmin(x, y)) / sum(x))
cat("nMG tal cual:\n"); print(round(nec(d$nMG, d$EI), 3))
cat("1 - nMG:\n"); print(round(nec(1 - d$nMG, d$EI), 3))
cat("Control MSC (publ. 0,743 / 0,744):\n"); print(round(nec(d$MSC, d$EI), 3))
names(d)[names(d) == "nMG"] <- "MG"
d$NEI <- 1 - d$EI
X <- d[, c("MSC","TC","SF","TT","IP","MG","EI","NEI")]
pubH <- data.frame(term = c("MSC*TC*SF*MG", "TC*SF*TT*~IP*~MG", "~TC*SF*TT*~IP*MG",
                            "~MSC*TC*~SF*TT*~IP*MG", "MSC*TC*~SF*TT*IP*~MG", "MSC*~TC*SF*TT*IP*~MG"),
                   incl_pub = c(0.946, 0.958, 0.968, 0.957, 0.946, 0.963),
                   cov_pub = c(0.441, 0.283, 0.274, 0.221, 0.255, 0.224))
rH <- replicar_cs(X[, setdiff(names(X), "NEI")], "EI", pubH$term, "Chai & Yang 2026 | EI alta", pubH)
pubL <- data.frame(term = "~MSC*~TC*~SF*~TT*~MG", incl_pub = 0.929, cov_pub = 0.391)
rL <- replicar_cs(X[, setdiff(names(X), "EI")], "NEI", pubL$term, "Chai & Yang 2026 | EI baja", pubL)
saveRDS(list(rH, rL), "replicas/r06_chai_yang_2026.rds")
