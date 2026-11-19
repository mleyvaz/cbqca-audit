# R41 (csQCA). Fransen et al. 2026, Nature Communications, 10.1038/s41467-026-76467-7 (PMC13562626).
# Objetivos estratégicos de las NDC (16 Partes). Datos: Zenodo 10.5281/zenodo.19264169 (qca_data.csv);
# código: Zenodo 10.5281/zenodo.19264297 (csQCA.R). Calibración declarada en el código:
# credibilidad = main_cred >= 1,5; AUTOCRACY = polity < 0; HIGH_EMITTER = cuota de emisiones > 6 %.
# Única solución publicada (Tabla Supl. 7, objetivo «mantener credibilidad»): AUTOCRACY + HIGH_EMITTER,
# inclS 1,000/1,000, covS 0,429/0,571. Los otros objetivos no tienen filas que pasen el corte 0,8.
source("replicas/csqca_comun.R")
raw <- read.csv(text = "iso,main_cred,polity,ems
arg,0,9,0.8
chn,2,-7,24.23
col,0,7,0.54
cri,0,10,0.02
eth,0,1,0.37
euu,2,9.653846153846152,6.33
gbr,2,8,0.86
idn,0,9,3.94
ind,2,9,6.76
jpn,0,10,2.28
mex,0,8,1.35
phl,1,8,0.48
sgp,2,-2,0.14
tur,2,-4,1
zaf,1.33,9,1.13
usa,2,8,11.6")
d <- data.frame(AUTOCRACY = as.integer(raw$polity < 0), HIGH_EMITTER = as.integer(raw$ems > 6),
                CRED = as.integer(raw$main_cred >= 1.5))
rownames(d) <- raw$iso
print(d)
t1 <- c("AUTOCRACY", "HIGH_EMITTER")
r1 <- replicar_cs(d, "CRED", t1, id = "R41 Fransen 2026 (credibilidad)",
  publicados = data.frame(term = t1, incl_pub = c(1, 1), cov_pub = c(.429, .571)))
saveRDS(list(r1), "replicas/r41_fransen_2026.rds")
