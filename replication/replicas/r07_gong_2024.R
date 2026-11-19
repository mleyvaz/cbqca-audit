# R07 (fsQCA). Gong et al. (2024), PLOS ONE, doi 10.1371/journal.pone.0295403. 132 países (GII 2021).
# Puntajes de S1 Data (bloque «Score»); calibración directa con las anclas publicadas (95 %, 50 %, 5 %).
source("replicas/csqca_comun.R")
s <- read.csv("datos/gong_2024/gong_scores.csv"); s <- s[s$country != "Value", ]
v <- c("InnPer","ITT","HCR","IFT","MS","BS"); s[v] <- lapply(s[v], as.numeric)
cat("N =", nrow(s), "\n"); print(round(sapply(s[v], function(x) c(min=min(x), max=max(x), mean=mean(x), sd=sd(x))), 1))
cat("Publicado: Min 5.6 27.6 7.0 17.6 23.7 8.7 | Max 62 95.1 67.4 64.8 84.7 68.1 | Mean 25.2 64.9 32.7 41.5 47.6 29.8\n")
anc <- list(InnPer = c(9.61, 21.55, 52.06), ITT = c(44.3, 63.1, 88.8), HCR = c(10.7, 31.6, 59.8),
            IFT = c(22.0, 42.1, 59.9), MS = c(28.5, 46.3, 67.3), BS = c(14.6, 25.4, 59.3))
X <- as.data.frame(lapply(v, function(k) calibrate(s[[k]], type = "fuzzy", thresholds = anc[[k]])))
names(X) <- c("Y", v[-1]); X$NY <- 1 - X$Y
# Control: necesidad publicada (alta innovación) ITT 0,866/0,822; HCR 0,881/0,869; BS 0,864/0,884
nec <- function(x, y) round(c(incl = sum(pmin(x, y)) / sum(y), cov = sum(pmin(x, y)) / sum(x)), 3)
print(rbind(ITT = nec(X$ITT, X$Y), HCR = nec(X$HCR, X$Y), IFT = nec(X$IFT, X$Y), MS = nec(X$MS, X$Y), BS = nec(X$BS, X$Y)))
pubH <- data.frame(term = c("ITT*HCR*IFT*BS", "HCR*IFT*MS*BS"), incl_pub = c(0.97, 0.98), cov_pub = c(0.74, 0.71))
rH <- replicar_cs(X[, c("ITT","HCR","IFT","MS","BS","Y")], "Y", pubH$term, "Gong 2024 | innovación alta", pubH)
pubL <- data.frame(term = c("~HCR*~MS*~BS", "~HCR*~IFT*~MS", "~ITT*~IFT*~BS", "~HCR*~IFT*~BS", "~ITT*~HCR*~IFT", "~ITT*~HCR*~BS"),
                   incl_pub = c(0.96, 0.96, 0.96, 0.96, 0.94, 0.96), cov_pub = c(0.71, 0.68, 0.71, 0.74, 0.71, 0.72))
rL <- replicar_cs(X[, c("ITT","HCR","IFT","MS","BS","NY")], "NY", pubL$term, "Gong 2024 | innovación baja", pubL)
saveRDS(list(rH, rL), "replicas/r07_gong_2024.rds")
