# R11 Rahman et al. (2025) PLOS ONE 10.1371/journal.pone.0320723 — fsQCA, 318 docentes (Bangladés).
# Datos: S1 Data (ítems Likert 1-5). Constructo = media de los ítems retenidos (Tabla 2).
# Anclas 2/3/4 (Tabla 8). Solución: Tabla 10 (X núcleo, 0 periférica; todas presentes).
source("replicas/csqca_comun.R")
raw <- read.csv("datos/rahman_2025/s003.csv", fileEncoding = "UTF-8-BOM")
cons <- c("VG", "MU", "IR", "CS", "PE", "QEU", "LE")
m <- as.data.frame(sapply(cons, function(k) rowMeans(raw[, grep(paste0("^", k, "[0-9]"), names(raw))])))
cat("Medias (publicado VG 3.52, MU 3.64, IR 3.69, CS 3.58, PE 3.48, QEU 3.56, LE 3.47):\n")
print(round(colMeans(m), 2))
d <- as.data.frame(lapply(m, function(z) round(calibrate(z, type = "fuzzy", thresholds = c(2, 3, 4)), 3)))
cat("N =", nrow(d), "| casos en 0,5 exacto:\n"); print(sapply(d, function(z) sum(z == 0.5)))
print(round(QCA::pof(d[, 1:6], outcome = d$LE, relation = "necessity")$incl.cov[, 1:2], 3))
terms <- c("VG*MU*IR", "VG*IR*CS*PE", "VG*IR*CS*QEU", "MU*IR*CS*QEU", "MU*IR*PE*QEU", "VG*MU*CS*PE*QEU")
pub <- data.frame(term = terms, incl_pub = c(0.910, 0.949, 0.947, 0.937, 0.951, 0.964),
                  cov_pub = c(0.822, 0.774, 0.779, 0.795, 0.790, 0.738))
r <- replicar_cs(d, "LE", terms, id = "R11 Rahman 2025 (alta LE)", publicados = pub)
saveRDS(r, "replicas/r11_rahman_2025.rds")
