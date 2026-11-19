ESTUDIO <- "R01 Li An 2025"
## Réplica + CB-QCA: Li & An (2025), PLOS ONE, doi 10.1371/journal.pone.0337406.
## Datos: suplemento S1 (s001.xlsx). Calibración y solución tomadas del artículo (Tablas de calibración y de configuraciones).
suppressPackageStartupMessages({ library(QCA); library(readxl); library(cbqca) })
d <- as.data.frame(read_excel("../datos/li_an_2025/s001.xlsx", sheet = 1))
z <- function(v) ifelse(v < 1e-6, 1e-6, v)
d$UE <- 0.2 * log(z(d$Likes) + 1) + 0.24 * log(z(d$Comments) + 1) + 0.27 * log(z(d$Collects) + 1) + 0.29 * log(z(d$Shares) + 1)
cat("UE: max", round(max(d$UE), 3), "min", round(min(d$UE), 3), "mean", round(mean(d$UE), 3), "sd", round(sd(d$UE), 3),
    " (artículo: 11.077, 0, 3.291, 2.043)\n")
cal <- function(x, th) calibrate(x, type = "fuzzy", thresholds = th)   # th = c(exclusión, cruce, inclusión)
X <- data.frame(
  VT = c("Travel guides" = 1, "Travel recommendations" = .75, "Travel records" = .5, "Popular science information" = .25, "Scenic montages" = 0)[d$`Video type`],
  SQ = cal(d$`Scene quantity`, c(2, 14, 51)),
  SR = c("Natural landscapes" = 1, "Travel shopping" = .75, "Human activities" = .5, "Architectural facilities" = .25, "Animals and plants" = 0)[d$`Scene representation`],
  TS = c("Exclamatory sentences" = 1, "Imperative sentences" = .75, "Interrogative sentences" = .5, "Declarative sentences" = .25, "No title" = 0)[d$`Title sentence`],
  BT = c(UGC = 1, PGC = .5, OGC = 0)[d$`Blogger type`],
  BI = cal(d$`Blogger influence`, c(9964, 38036, 622750)),
  BA = cal(d$`Blogger activity`, c(3700, 577, 64)),
  NW = c("Live narration" = 1, "Subtitles" = .75, "Voiceover+Subtitles" = .5, "No narration" = .25, "Voiceover" = 0)[d$`Narrative way`],
  BM = c(Song = 1, "No music" = .5, "Absolute music" = 0)[d$`Background music`],
  AD = c(Yes = 1, No = 0)[d$`Advertising information`],
  Y  = cal(d$UE, c(0.678554735, 2.973981802, 6.832218542)))
rownames(X) <- NULL
cat("Faltantes por columna:\n"); print(colSums(is.na(X)))
## Seis caminos publicados (Tabla de configuraciones; celdas en blanco = condición irrelevante)
terms <- c(P1 = "VT*SQ*TS*BT*BI*BA*NW*~AD",
           P2 = "VT*SQ*SR*TS*BT*BI*BA*NW",
           P3 = "VT*SQ*~SR*TS*BT*BI*NW*~BM*~AD",
           P4 = "VT*SQ*~SR*TS*BT*BA*NW*~BM*~AD",
           P5 = "VT*SQ*~SR*BT*BI*BA*NW*~BM*~AD",
           P6 = "SQ*~SR*TS*BT*BI*BA*NW*~BM*~AD")
pub <- data.frame(term = names(terms), incl_pub = c(0.945394, 0.917914, 0.932508, 0.894176, 0.936127, 0.943618),
                  cov_pub = c(0.128105, 0.252746, 0.0665085, 0.0731585, 0.0714878, 0.077534))
rep <- t(sapply(terms, function(tm) { p <- pof(tm, "Y", data = X, relation = "sufficiency"); unlist(p$incl.cov[1, c("inclS", "covS")]) }))
cmp <- cbind(pub, incl_rep = round(rep[, 1], 4), cov_rep = round(rep[, 2], 4))
cat("\nRéplica de consistencia y cobertura por camino:\n"); print(cmp, row.names = FALSE)
r <- cbqca(X, "Y", unname(terms), tau = 0.5, boot = 1000)
r$table$term <- names(terms)
print(r)

# Complemento 17-nov: cota exacta y sensibilidad a tau (desviación declarada del protocolo)
source("csqca_comun.R")
extra_cs(X, "Y", unname(terms), names(terms))
