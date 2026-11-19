# R17 (csQCA). Ziemann et al. 2022, Implementation Science Communications, 10.1186/s43058-022-00364-5
# (PMC9617223). Difusión del programa TCAM en 15 AHSN de Inglaterra.
# Datos: Tabla 1 «QCA raw data table» (15 casos, 11 condiciones, resultado). Exacto.
# Soluciones parsimoniosas (Tabla 3): éxito = TIMELY*PHARMA; fracaso = ~TIMELY + ~PHARMA.
source("replicas/csqca_comun.R")
raw <- rbind(
  TIMELY    = c(0,0,0,1,1,1,1,1,1,1,1,0,1,1,1),
  ADOPT     = c(1,0,1,0,0,1,0,1,1,1,0,0,1,1,0),
  EXP       = c(0,0,1,0,0,1,0,1,1,0,0,0,1,1,0),
  PILOT     = c(0,1,1,1,0,1,0,1,1,1,1,0,1,1,1),
  PHARMA    = c(0,1,1,1,0,1,0,1,1,0,0,1,1,1,1),
  CHAMP     = c(1,0,0,1,0,1,1,1,1,0,0,1,1,1,1),
  SENIOR    = c(0,1,1,1,0,1,1,1,1,0,0,1,1,1,1),
  CCG       = c(1,1,1,1,0,1,1,1,0,0,0,0,1,1,0),
  INTERSECT = c(1,1,1,0,1,1,1,0,0,0,1,0,1,1,1),
  NETWORK   = c(1,1,1,1,0,1,1,1,1,0,1,0,1,1,1),
  CONTEXT   = c(0,0,1,1,1,1,1,1,1,1,1,0,1,1,1),
  OUT       = c(0,0,0,1,0,1,0,1,1,0,0,0,1,1,1))
d <- as.data.frame(t(raw)); d$nOUT <- 1 - d$OUT
conds <- setdiff(names(d), c("OUT", "nOUT"))
r1 <- replicar_cs(d[, c(conds, "OUT")], "OUT", "TIMELY*PHARMA", id = "R17 Ziemann 2022 (éxito)",
  publicados = data.frame(term = "TIMELY*PHARMA", incl_pub = 1, cov_pub = 1))
r2 <- replicar_cs(d[, c(conds, "nOUT")], "nOUT", c("~TIMELY", "~PHARMA"), id = "R17 Ziemann 2022 (fracaso)",
  publicados = data.frame(term = c("~TIMELY", "~PHARMA"), incl_pub = 1, cov_pub = c(0.5, 0.625)))
saveRDS(list(r1, r2), "replicas/r17_ziemann_2022.rds")
