# R69 Zhang, Hou, Guo (2023) PLOS ONE 10.1371/journal.pone.0291652 — fsQCA, 35 empresas (transformación digital
# y desempeño en desarrollo de nuevos productos, NPD). Datos: S1 Data (s001.xlsx, brutos; NPD = columna
# «增加zl», incremento de patentes). Anclas: Tabla 1 (declaradas); 0,5 -> 0,501 (declarado).
# fsQCA 3.0, consistencia 0,8, PRI 0,7, frecuencia 1. Soluciones: Fig 2 (alta, 5) y Fig 3 (no alta, 5), imágenes.
source("replicas/csqca_comun.R")
r <- read.csv("datos/r69_zhang_2023/s001.csv", colClasses = c(code = "character"))
cons <- c("DI", "DS", "RDP", "RDC", "SIZE", "AGE", "NPD")
anc <- list(DI = c(8957310.295, 27734417.11, 68357283.87), DS = c(10, 46, 80), RDP = c(5.46, 14.5, 18.605),
            RDC = c(3.675, 6.43, 9.545), SIZE = c(1932, 2773, 5544), AGE = c(20, 22, 23.5), NPD = c(13, 34, 95))
cat("Cuartiles recalculados (tipo 7):\n"); print(round(t(sapply(r[cons], quantile, c(0.25, 0.5, 0.75))), 3))
d <- as.data.frame(lapply(cons, function(k) round(calibrate(r[[k]], type = "fuzzy", thresholds = anc[[k]]), 3)))
names(d) <- cons
cat("Casos en 0,500:\n"); print(sapply(d, function(z) sum(z == 0.5)))
d[d == 0.5] <- 0.501
d$nNPD <- 1 - d$NPD
cat("Necesidad (Tabla 2: DI 0,735479/0,687765; AGE 0,551081/0,536977):\n")
print(round(QCA::pof("DI + AGE", "NPD", d, relation = "necessity")$incl.cov[1:2, 1:2], 6))
cd <- cons[1:6]
hi <- c("DI*DS*RDP*SIZE", "DI*RDP*RDC*SIZE", "DI*DS*RDC*AGE", "~DI*DS*RDP*~RDC*~SIZE*AGE", "~DI*~DS*RDP*~RDC*SIZE*AGE")
lo <- c("~DI*~DS*~RDP*~SIZE*~AGE", "~DI*~DS*~RDP*~RDC*~AGE", "~DI*~DS*~RDC*~SIZE*AGE",
        "~DI*DS*RDP*RDC*~SIZE*~AGE", "~DI*DS*~RDP*RDC*SIZE*AGE")
r1 <- replicar_cs(d[, c(cd, "NPD")], "NPD", hi, id = "R69 Zhang 2023 (alta)",
  publicados = data.frame(term = hi, incl_pub = c(0.908257, 0.986553, 0.981958, 0.91207, 0.908257),
                          cov_pub = c(0.346697, 0.265789, 0.262891, 0.0688926, 0.0597754)))
r2 <- replicar_cs(d[, c(cd, "nNPD")], "nNPD", lo, id = "R69 Zhang 2023 (baja)",
  publicados = data.frame(term = lo, incl_pub = c(0.922771, 0.976182, 0.964365, 0.973262, 0.869919),
                          cov_pub = c(0.239126, 0.155603, 0.234841, 0.0987092, 0.0580323)))
saveRDS(list(alta = r1, baja = r2), "replicas/r69_zhang_2023.rds")
