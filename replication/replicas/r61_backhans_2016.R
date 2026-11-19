# R61 Backhans, Mosedale, Bruce et al. (2016) BMC Public Health 10.1186/s12889-016-3482-2 — fsQCA, 21 países europeos (flexiseguridad y
# retorno al empleo de personas con mala salud). Datos: Additional file 2 (CSV con valores originales y CALIBRADOS por los autores).
# Soluciones con consistencia y cobertura por término: solo las COMPLEJAS (Additional file 3, 2 decimales).
# Las intermedias del texto (RTW: Emp*OldServ*ALMP + Emp*EPT*~PLMP_Sick + OldServ*~EPT*~PLMP_Sick, sol. 0,78/0,91;
# RR: Uneave*PLMP_Sick + Uneave*EPL) no traen parámetros por término; se verifican aparte a nivel de solución.
source("replicas/csqca_comun.R")
r <- read.csv("datos/backhans_2016/add2.csv", sep = ";", dec = ",")
d <- data.frame(EMP = r$empave_cal, UNE = r$uneave_cal, EPT = r$ept_cal, EPL = r$epl_cal, ALMP = r$almp_cal,
                PLS = r$plmp_sick, OLD = r$old_cal, SOC = r$socexp_cal, RTW = r$rtw_cal, RR = r$rr_cal)
rtw <- c("EMP*ALMP*PLS*OLD", "EMP*EPT*~ALMP*~PLS", "~EMP*~EPT*~ALMP*~PLS*OLD")
rr <- c("UNE*ALMP*PLS*SOC", "UNE*EPL*~ALMP*~PLS*~SOC", "UNE*~EPL*~ALMP*PLS*~SOC")
r1 <- replicar_cs(d[, c("EMP", "EPT", "ALMP", "PLS", "OLD", "RTW")], "RTW", rtw, id = "R61 Backhans 2016 (RTW)",
  publicados = data.frame(term = rtw, incl_pub = c(0.80, 0.74, 0.81), cov_pub = c(0.57, 0.35, 0.12)))
r2 <- replicar_cs(d[, c("UNE", "EPL", "ALMP", "PLS", "SOC", "RR")], "RR", rr, id = "R61 Backhans 2016 (RR)",
  publicados = data.frame(term = rr, incl_pub = c(0.98, 0.94, 0.92), cov_pub = c(0.45, 0.24, 0.22)))
cat("\nSoluciones intermedias del texto (solo nivel de solución publicado: RTW 0,78/0,91):\n")
print(round(QCA::pof("EMP*OLD*ALMP + EMP*EPT*~PLS + OLD*~EPT*~PLS", "RTW", d, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
print(round(QCA::pof("UNE*PLS + UNE*EPL", "RR", d, relation = "sufficiency")$incl.cov[, c(1, 3)], 3))
saveRDS(list(rtw = r1, rr = r2), "replicas/r61_backhans_2016.rds")
# CLASIFICACIÓN: aproximada (complejas; dif. máx. 0,005 en consistencia y 0,009 en cobertura, UNE*~EPL*~ALMP*PLS*~SOC 0,229 vs 0,22).
