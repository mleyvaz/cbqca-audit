# R54 Wang et al. (2025) BMC Public Health 10.1186/s12889-025-25344-x — fsQCA, 995 agricultores (CGSS 2021), actividad física.
# Datos: Supplementary Material 1 (CSV, static-content.springer.com), datos crudos Likert.
# Anclas: Tabla 2. Frecuencia 10, consistencia 0,80, PRI 0,75. Solución: Tabla 4 (configuraciones 1-7).
# La leyenda de la Tabla 4 pierde el símbolo de «ausencia núcleo» (puede haber celdas en blanco que son ⊗).
source("replicas/csqca_comun.R")
r <- read.csv("datos/wang_2025_farmers/s1.csv")
names(r) <- c("PROV", "PE", "HL", "ICC", "SCC", "SI", "SS", "IU", "RA", "EA")
an <- list(PE = c(1, 2, 5), HL = c(2, 4, 5), ICC = c(1, 2, 4), SCC = c(1.38, 2.25, 3.13), SI = c(1, 2, 4.2),
           SS = c(1, 2.33, 4), IU = c(1, 4, 5), RA = c(1, 3, 5), EA = c(2, 4, 5))
d <- as.data.frame(sapply(names(an), function(z) calibrate(r[[z]], type = "fuzzy", thresholds = an[[z]])))
cd <- names(an)[-1]
hi <- c("ICC*SCC*SI*IU*EA", "ICC*SCC*SI*SS*IU*~RA", "~HL*SCC*SI*SS*IU*~RA*EA", "HL*ICC*SCC*SI*SS*IU",
        "HL*ICC*SI*SS*IU*EA", "ICC*SCC*SI*IU*RA*EA", "~HL*SCC*SI*IU*RA*EA")
r1 <- replicar_cs(d[, c(cd, "PE")], "PE", hi, id = "R54 Wang 2025 (actividad física)",
  publicados = data.frame(term = hi, incl_pub = c(0.8635, 0.8433, 0.8755, 0.8604, 0.8675, 0.8434, 0.8455),
                          cov_pub = c(0.3748, 0.2658, 0.2203, 0.3072, 0.2907, 0.2391, 0.2176)))
saveRDS(list(alta = r1), "replicas/r54_wang_2025.rds")
# CLASIFICACIÓN: no replicable. Configuraciones 1-5 cuadran (<= 0,003), pero la 6 (cobertura 0,308 vs 0,239,
# consistencia 0,870 vs 0,843) no cuadra con ninguna negación de sus celdas en blanco, y la 7 solo con ~SS no
# declarado (0,844/0,221). .rds del corpus borrado.
