# R85 Islam et al. (2024) PLOS ONE 10.1371/journal.pone.0305916 — fsQCA, 71 participantes (evaluación posterior
# a la capacitación APA). Datos: S1 Data (CSV) con constructos y pertenencias calibradas (*_C, 2 decimales).
# Anclas: Tabla 5 (percentiles 5/50/95). Soluciones: Tabla 8 (ETAPA y ~ETAPA).
source("replicas/csqca_comun.R")
r <- read.csv("datos/r85_pone0305916/s003_journal.pone.0305916.s003.csv")
d <- data.frame(RA = r$Reaction_C, LS = r$Skill_C, BH = r$Behaviour_C, RE = r$Result_C, ETAPA = r$Design_C)
anc <- list(Design = c(1.92, 4.2, 5), Reaction = c(1.7, 4.25, 5), Skill = c(1.9, 4, 5), Behaviour = c(1.65, 4, 5),
            Result = c(1.55, 4, 5))
cat("Diferencia máx. |recalibrado (fsQCA, ±3) - publicado|:\n")
print(sapply(names(anc), function(k) max(abs(round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]], idm = plogis(3)), 2) - r[[paste0(k, "_C")]]))))
cat("Casos en 0,5:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d$nETAPA <- 1 - d$ETAPA
cd <- c("RA", "LS", "BH", "RE")
hi <- c("LS*BH", "~RA*LS*~RE", "~RA*~LS*~BH*RE")
lo <- c("~LS*~BH*~RE", "~RA*~BH*~RE", "~RA*~LS*~BH")
r1 <- replicar_cs(d[, c(cd, "ETAPA")], "ETAPA", hi, id = "R85 Islam 2024 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.875525, 0.908912, 0.888039), cov_pub = c(0.872318, 0.482993, 0.334118)))
r2 <- replicar_cs(d[, c(cd, "nETAPA")], "nETAPA", lo, id = "R85 Islam 2024 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.950113, 0.958254, 0.935928), cov_pub = c(0.644906, 0.616230, 0.601587)))
saveRDS(list(alta = r1, baja = r2), "replicas/r85_islam_2024.rds")
