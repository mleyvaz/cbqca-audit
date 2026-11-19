# R14 Wang et al. (2024) PLOS ONE 10.1371/journal.pone.0296791 — fsQCA, inversiones chinas en el exterior.
# Datos: S1 (hoja «data», 480 proyectos, sin resultado) alineada en orden con la hoja «1»
# (código + año; 479 de 480 alineadas), de donde sale el resultado: participación >= 90 % = 1
# (modo de propiedad total). Los #N/A se toman como 0, lo que reproduce la Tabla 2.
# Anclas: Tabla 5. Experiencia y resultado son nítidos. Solución: Tabla 7.
source("replicas/csqca_comun.R")
raw <- read.csv("datos/wang_2024/wang.csv")
anc <- list(MI = c(0, 0.089, 0.577), TI = c(0, 0.046, 0.829), FD = c(0.130, 3.269, 5.984),
            IFD = c(0, 2.688, 7.640))
d <- as.data.frame(lapply(names(anc), function(v)
  round(calibrate(raw[[v]], type = "fuzzy", thresholds = anc[[v]]), 3)))
names(d) <- names(anc); d$EX <- raw$EX; d$WO <- raw$WO
cat("N =", nrow(d), "| casos en 0,5 exacto:\n"); print(sapply(d, function(z) sum(z == 0.5)))
print(round(QCA::pof(d[, c("EX", "MI", "TI", "FD", "IFD")], outcome = d$WO, relation = "necessity")$incl.cov[, 1:2], 3))
terms <- c("EX*~MI", "EX*IFD", "EX*TI*~FD")
pub <- data.frame(term = terms, incl_pub = c(0.846, 0.881, 0.831), cov_pub = c(0.152, 0.127, 0.045))
r <- replicar_cs(d, "WO", terms, id = "R14 Wang 2024 (propiedad total)", publicados = pub)
saveRDS(r, "replicas/r14_wang_2024.rds")
