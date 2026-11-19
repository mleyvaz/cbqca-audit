# R35 Li et al. (2024) PLOS ONE 10.1371/journal.pone.0308717 — fsQCA, crowdfunding (Indiegogo).
# RESULTADO: NO REPLICABLE. El fsQCA usa 64 proyectos (top 1 % por monto recaudado, > 30 000 USD),
# pero S1 Dataset (s004.xls) trae los 6284 proyectos SIN monto recaudado ni meta, así que la submuestra
# no es identificable. Anclas publicadas (Tabla 9) no coinciden con percentiles de la muestra completa.
x <- as.data.frame(readxl::read_excel("datos/li_2024_crowdfunding/s004.xls"))
x[] <- lapply(x, as.numeric)
cat("N del archivo:", nrow(x), "| columnas:", paste(names(x), collapse = ", "), "\n")
cat("Percentiles 5/50/95 de la muestra completa (Tabla 9 publica 0,27/1/5,16 para funding ratio):\n")
print(round(sapply(x, quantile, c(.05, .5, .95)), 3))
cat("Suma de casos en la tabla de verdad S2:", sum(c(3,1,4,2,5,1,3,2,1,1,1,2,2,3,2,4,3,3,1,8,1,2,1,3)), "\n")
