# R81 Ling et al. (2026) PLOS ONE 10.1371/journal.pone.0354175 — fsQCA, 25 partidos de Champions League
# (atención colectiva a publicaciones institucionales). Datos: S1 File (zip), qca_input_calibrated.csv con
# conjuntos ya calibrados (anclas News_Index 90/60/30 y Raw_Delay 360/60/15, Tabla 1).
# Solución intermedia: Tabla 3 (dos términos), resultado High_News.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r81_pone0354175/x/Supporting Information/qca_input_calibrated.csv")
d <- data.frame(Delay = r$High_Delay, Shock = r$High_Shock, Stakes = r$High_Stakes,
                TD = r$High_TemplateDeviation, News = r$High_News)
cat("Control de calibración de News (lineal 30/60/90 por tramos):\n")
print(summary(r$High_News))
hi <- c("~Delay*~Shock*Stakes*TD", "~Delay*Shock*Stakes*TD")
r1 <- replicar_cs(d, "News", hi, id = "R81 Ling 2026 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.912, 1.000), cov_pub = c(0.342, 0.045)))
saveRDS(list(alta = r1), "replicas/r81_ling_2026.rds")
