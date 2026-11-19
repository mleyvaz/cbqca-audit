# Análisis transversal del corpus CB-QCA (v2, 18-nov-2026, tras la revisión de Codex).
# Entrada: replicas/corpus/*.rds (lo escriben replicar_cs y extra_cs al correr cada réplica).
# Salida: analisis/elegibilidad.csv, analisis/tabla_terminos.csv, analisis/sensibilidad_corte.csv,
#         analisis/corpus.txt (consola).
# Uso (desde auditoria/): Rscript analisis/corpus.R > analisis/corpus.txt
#
# Cambios v2:
#  - Análisis PRINCIPAL solo con réplicas EXACTAS (protocolo, criterio 3); SENSIBILIDAD con todas.
#  - Etiquetas: «PS_lo < 0,5» = los datos y la exogeneidad NO garantizan PS >= 0,5 (no «no causal»);
#    «cota exacta = 0» = límite de confianza no informativo (no «indistinguible del azar»).
#    p_y_fuera = P(Y | ~X), frecuencia del resultado fuera del camino (no la tasa base P(Y)).
#  - Inferencia: logit de Firth (logistf) contra separación; errores CR2 con gl de Satterthwaite
#    (clubSandwich) por pocos clusters; dejar-un-estudio-fuera; AUC por estrato con IC bootstrap por estudio.
#  - El 95 % de la cota exacta es por término, no simultáneo.
suppressPackageStartupMessages({ library(cbqca); library(sandwich); library(lmtest)
  library(clubSandwich); library(logistf) })
source("replicas/csqca_comun.R")

# Elegibilidad según el criterio 3 del protocolo (réplica exacta), decidida por estudio.
eleg <- read.table(header = TRUE, sep = "|", strip.white = TRUE, text = "
codigo | estudio                 | calidad      | motivo
R01    | Li y An 2025            | exacta       | consistencias dif <= 0,003
R02    | Bing y Wan 2025         | exacta       | dif <= 0,002 (N 3.594 frente a 3.838 declarados)
R03    | Zheng 2026              | aproximada   | medias de constructo difieren 0,04-0,09
R04    | Krook 2010              | exacta       | datos idénticos a la Tabla 2
R05    | Lam y Ostrom 2010       | exacta       | datos idénticos a la Tabla 8
R06    | Chai y Yang 2026        | exacta       | dif 0,000
R07    | Gong 2024               | exacta       | anclas publicadas, dif <= 0,001
R08    | Xiao 2026               | exacta       | dif 0,000
R09    | Shi 2025                | aproximada   | dif <= 0,006
R10    | Zhang 2024              | aproximada   | NH4 no replica: no cumple «todos los términos» (los otros 7 dif <= 0,004)
R11    | Rahman 2025             | aproximada   | medias de ítems, dif <= 0,006
R12    | Jiang y Song 2026       | exacta       | dif 0,000
R13    | Zhao 2026               | reconstruida | anclas 1/3/5 distintas de las declaradas; signo de EN corregido
R14    | Wang 2024               | reconstruida | resultado alineado por orden entre hojas; #N/A = 0
R15    | Ghana 2023              | exacta       | tabla de verdad con todos los casos, dif 0
R16    | Cribado 2025            | excluida     | lista de decisión, sin minimización booleana
R17    | Ziemann 2022            | exacta       | datos crudos, dif 0
R18    | Ziemann 2016            | exacta       | datos crudos, dif <= 0,001
R19    | Blackman 2013           | exacta       | datos crudos; consistencias = 1 publicadas; coberturas no publicadas (regla: solo consistencia)
R20    | Cooper 2022             | exacta       | tabla de verdad con n y consistencia, dif <= 0,004
R21    | Xin 2025                | aproximada   | 42 de 47 casos; camino 2 no reproduce lo publicado
R22    | Gilreath 2022           | reconstruida | resultado crisp; la Incl. publicada usa pertenencias intermedias
R23    | Li 2024                 | reconstruida | resultado negativo inferido de las coberturas
R24    | Majerova 2023           | exacta       | csQCA, datos de la Tabla 3, dif 0
R25    | Lu 2025                 | exacta       | csQCA, dif <= 0,004 (redondeo a 2 decimales)
R26    | Zhang 2025              | exacta       | datos calibrados publicados, dif 0; signos de la Tabla 7
R27    | Li 2024b                | exacta       | datos codificados s001, dif 0
R28    | Yang 2024               | no_replicable | S1 no coincide con la Tabla 2
R29    | Xie 2024                | no_replicable | H4 y ~TE no cuadran
R30    | Chen 2024               | aproximada   | dif <= 0,017
R31    | Wenchang 2026           | duplicado    | mismo artículo que R08 (doi 10.1371/journal.pone.0340674); se conserva R08
R32    | Hou 2025                | aproximada   | cobertura +0,008; L4 duplicado omitido
R33    | Hu 2025                 | exacta       | dif <= 0,004
R34    | Song 2024               | no_replicable | dif hasta 0,40
R35    | Li 2024c                | no_replicable | submuestra no identificable
R36    | Liao 2023               | no_replicable | configuración 4 no cuadra
R37    | Li 2026                 | reconstruida | anclas por percentiles declarados, no las de la tabla
R38    | Rodrigues 2022          | aproximada   | calibración no declarada en números; dif <= 0,008
R39    | Qu 2022                 | exacta       | dif <= 0,004
R40    | Furrutter 2023          | exacta       | csQCA, datos y script de los autores, dif 0
R41    | Fransen 2026            | exacta       | csQCA, datos y código en Zenodo, dif 0
R42    | Kemp 2021               | reconstruida | solo se publica el ajuste de la solución completa
R43    | Sutcliffe 2022          | exacta       | csQCA, casos nombrados por fila, dif 0
R44    | Cheng 2023              | no_replicable | tabla de datos incompatible con coberturas
R45    | Rajkumar 2023           | exacta       | csQCA, dif 0
R46    | Wang 2024b              | exacta       | csQCA, dif 0 (pie de la Tabla 10 invierte los valores)
R47    | Cheng 2024              | exacta       | csQCA, dif 0
R48    | Piper 2023              | reconstruida | csQCA; errata de cobertura (0,70 por 0,667): no es de signo
R50    | Li 2026b                | exacta       | csQCA, Zenodo, dif 0
R51    | Bai 2026                | exacta       | csQCA, Tabla 3, dif 0
R52    | Huang 2024              | no_replicable | anclas fuera de escala; XT no cuadra
R53    | Chen 2025               | exacta       | anclas de la Tabla 3, dif <= 0,002
R54    | Wang 2025b              | no_replicable | configuraciones 6 y 7 no cuadran
R55    | Wang 2025c              | reconstruida | anclas de CYN no declaradas (copiadas de SCB)
R56    | Chen 2025b              | aproximada   | agrupación de ítems inferida; dif <= 0,007
R57    | Chen 2023               | reconstruida | columnas alineadas por posición
R58    | Wang 2026               | aproximada   | cobertura dif 0,006
R59    | Wu 2025                 | no_replicable | sin datos por caso
R60    | Xu 2026                 | no_replicable | microdatos no publicados
R61    | Backhans 2016           | aproximada   | publicado a 2 decimales; cobertura dif 0,009
R62    | Li 2024d                | exacta       | dif <= 0,003
R63    | Xia 2024                | aproximada   | un término dif 0,008
R64    | Liu 2024                | exacta       | calibrados publicados, dif 0
R65    | Li 2023                 | no_replicable | faltan variables
R66    | Zhang 2025b             | exacta       | anclas de la Tabla 6, dif <= 0,0045
R67    | Guo 2023                | no_replicable | resultado DEA no publicado
R68    | Yan 2021                | no_replicable | faltan 3 condiciones
R69    | Zhang 2023              | aproximada   | L4 dif 0,009
R70    | Yang 2021               | reconstruida | un término solo cuadra con otra condición (errata no de signo)
R71    | Yin 2023                | no_replicable | no es QCA
R72    | Hu 2023                 | no_replicable | sin datos de condiciones
R80    | Wu 2025b                | exacta       | calibrados publicados, dif 0
R81    | Ling 2026               | exacta       | calibrados publicados, dif 0
R82    | Qu 2026                 | exacta       | calibración de fsQCA 3.1 (software declarado), dif <= 0,002
R83    | Wu 2025c                | reconstruida | errata de cobertura de H5 (0,17 por 0,517), no de signo; la salida S2 da 0,517
R84    | Wang 2025d              | reconstruida | solo cuadra con calibración estilo fsQCA, software no declarado
R85    | Islam 2024              | exacta       | pertenencias publicadas, dif 0
R86    | Mahdy 2026              | no_replicable | necesidad no cuadra con las anclas declaradas
R87    | Zhao 2025               | exacta       | calibración de fsQCA 3.0 (declarado), dif <= 0,002
R88    | Chen 2023b              | exacta       | pertenencias publicadas, dif <= 0,004 (2 decimales)
R89    | Xu 2021                 | exacta       | calibrados publicados, dif 0
R90    | Ma 2023                 | exacta       | pertenencias publicadas, dif 0
R91    | Zheng 2022              | no_replicable | Tabla 6 internamente inconsistente
R92    | Yu 2024                 | no_replicable | anclas no declaradas en valores
R93    | Yu 2025                 | no_replicable | puntajes de constructo no publicados
R94    | Mo 2026                 | exacta       | calibración de fsQCA 3.0 (declarado), dif <= 0,001
R95    | Paykani 2024            | no_replicable | coberturas no alcanzables con ningún término
R96    | Li 2026c                | exacta       | calibrados publicados, dif 0
R100   | Chen 2023c              | exacta       | calibración de fsQCA 3 (declarado), dif <= 0,003
R101   | Yu 2024b                | aproximada   | E2 y E3 con valores intercambiados en la tabla; dif <= 0,019
R102   | Valero-Moreno 2020      | no_replicable | autoestima de S1 no reproduce las tablas
R103   | Liang 2026              | reconstruida | solo cuadra con calibración fsQCA 3, no declarada
R104   | Yao 2023                | exacta       | calibrados publicados, dif <= 0,001")
write.csv(eleg, "analisis/elegibilidad.csv", row.names = FALSE)

medir <- function(d, y, tm, corte, nivel = "termino") {
  yv <- as.numeric(as.character(d[[y]])); xv <- cbqca:::.term_membership(d, tm)
  ok <- !is.na(xv) & !is.na(yv); x <- xv[ok] > corte; yy <- yv[ok] > corte
  if (sum(x) == 0 || sum(!x) == 0) return(NULL)
  B <- if (corte == 0.5) 1000 else 0       # protocolo: ancho bootstrap (1000 réplicas) en el corte principal
  r <- suppressWarnings(cbqca(d, y, tm, threshold = corte, tau = 0.5, boot = B))$table
  r3 <- suppressWarnings(cbqca(d, y, tm, threshold = corte, tau = 0.3, boot = 0))$table
  r7 <- suppressWarnings(cbqca(d, y, tm, threshold = corte, tau = 0.7, boot = 0))$table
  data.frame(nivel = nivel, corte = corte, N = sum(ok), n_in = sum(x),
             k = if (nivel == "termino") length(strsplit(tm, "*", fixed = TRUE)[[1]]) else NA_integer_,
             tasa_base = mean(yy), p_y_fuera = mean(yy[!x]), incl_crisp = mean(yy[x]),
             incl_fuzzy = sum(pmin(xv[ok], yv[ok])) / sum(xv[ok]),
             PS_lo = r$PS_lo, PN_lo = r$PN_lo, PNS_lo = r$PNS_lo,
             bd_PS = r$breakdown_PS, bd_PNS = r$breakdown_PNS,
             bd_PS_t03 = r3$breakdown_PS, bd_PS_t07 = r7$breakdown_PS,
             ancho_id = r$PS_up - r$PS_lo,
             ancho_boot = if (B > 0) r$PS_lo_ci_high - r$PS_lo_ci_low else NA_real_,
             PS_exact95 = ps_lo_exacta(d, y, tm, umbral = corte))
}

fs <- list.files("replicas/corpus", full.names = TRUE)
filas <- list()
CACHE <- "analisis/cache"; dir.create(CACHE, showWarnings = FALSE)
for (f in fs) {
  cf <- file.path(CACHE, basename(f))               # caché por estudio: la corrida es reanudable
  if (file.exists(cf) && file.mtime(cf) > file.mtime(f)) { filas <- c(filas, readRDS(cf)); next }
  n0 <- length(filas)
  e <- readRDS(f); d <- e$data
  cod <- regmatches(basename(f), regexpr("^R[0-9]+", basename(f)))
  if (length(cod) == 0) cod <- c(Krook = "R04", Lam = "R05", Chai = "R06", Gong = "R07", Xiao = "R08")[
    sapply(c("Krook", "Lam", "Chai", "Gong", "Xiao"), function(k) startsWith(basename(f), k))]
  cs <- all(unlist(lapply(d, function(z) suppressWarnings(as.numeric(as.character(z))))) %in% c(0, 1, NA))
  for (i in seq_along(e$terms)) for (corte in if (cs) 0.5 else c(0.4, 0.5, 0.6)) {
    m <- medir(d, e$outcome, e$terms[i], corte)
    if (!is.null(m)) filas[[length(filas) + 1]] <- cbind(estudio = unname(cod), estrato = if (cs) "cs" else "fs",
                                                          outcome = e$outcome, term = e$terms[i], m)
  }
  sol <- paste(unique(e$terms), collapse = " + ")          # solución completa (disyunción de los términos)
  m <- medir(d, e$outcome, sol, 0.5, nivel = "solucion")
  if (!is.null(m)) filas[[length(filas) + 1]] <- cbind(estudio = unname(cod), estrato = if (cs) "cs" else "fs",
                                                        outcome = e$outcome, term = sol, m)
  saveRDS(if (length(filas) > n0) filas[(n0 + 1):length(filas)] else list(), cf)
  cat("hecho:", basename(f), "
", file = stderr())
}
tt <- do.call(rbind, filas)
dup <- duplicated(tt[, c("estudio", "outcome", "term", "corte", "nivel")])
cat("Registros duplicados descartados (misma réplica registrada dos veces):", sum(dup), "\n")
tt <- tt[!dup, ]
tt <- merge(tt, eleg[, c("codigo", "calidad")], by.x = "estudio", by.y = "codigo", all.x = TRUE)
if (any(is.na(tt$calidad))) stop("estudio sin clasificar: ", paste(unique(tt$estudio[is.na(tt$calidad)]), collapse = ", "))
tt$ps_lo_bajo <- as.integer(tt$PS_lo < 0.5)       # primaria 1: no se garantiza PS >= 0,5
tt$cota_no_inf <- as.integer(tt$PS_exact95 == 0)  # límite de confianza al 95 % (por término) no informativo
write.csv(tt, "analisis/tabla_terminos.csv", row.names = FALSE)

auc <- function(score, y) { r <- rank(score); n1 <- sum(y); n0 <- sum(!y)
  if (n1 == 0 || n0 == 0) return(NA_real_); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
auc_ic <- function(z, y, B = 1000) {           # bootstrap por estudio (clusters), sin copiar data frames
  set.seed(1); g <- split(seq_len(nrow(z)), z$estudio); sc <- z$incl_fuzzy; yy <- z[[y]]
  b <- replicate(B, { i <- unlist(g[sample(length(g), replace = TRUE)], use.names = FALSE); auc(sc[i], yy[i]) })
  c(auc = auc(z$incl_fuzzy, z[[y]]), quantile(b, c(0.025, 0.975), na.rm = TRUE))
}
cr2 <- function(m, d) {
  ct <- coef_test(m, vcov = vcovCR(m, cluster = d$estudio, type = "CR2"), test = "Satterthwaite")
  data.frame(coef = names(coef(m)), est = round(ct$beta, 3), se = round(ct$SE, 3),
             gl = round(ct$df_Satt, 1), p = signif(ct$p_Satt, 3))
}

analizar <- function(t5, titulo) {
  cat("\n\n############################################################\n")
  cat("##", titulo, "\n")
  cat("Términos:", nrow(t5), "| estudios:", length(unique(t5$estudio)),
      "| cs:", sum(t5$estrato == "cs"), "(", length(unique(t5$estudio[t5$estrato == "cs"])), "estudios )",
      "| fs:", sum(t5$estrato == "fs"), "(", length(unique(t5$estudio[t5$estrato == "fs"])), "estudios )\n")

  cat("\n=== 1. Medidas primarias (corte 0,5) ===\n")
  cat("PS_lo<0,5 = la cota inferior de PS bajo exogeneidad no alcanza 0,5 (no se garantiza PS >= 0,5).\n")
  cat("Cota exacta = 0 = el límite de confianza al 95 % (por término) de esa cota es 0: no informativo.\n")
  for (s in c("cs", "fs", "todos")) {
    z <- if (s == "todos") t5 else t5[t5$estrato == s, ]
    cat(sprintf("%-5s n=%3d | PS_lo<0,5: %2d (%.0f %%) | cota exacta = 0: %2d (%.0f %%) | mediana quiebre PNS %.3f | mediana quiebre PS %.3f\n",
                s, nrow(z), sum(z$ps_lo_bajo), 100 * mean(z$ps_lo_bajo), sum(z$cota_no_inf), 100 * mean(z$cota_no_inf),
                median(z$bd_PNS), median(z$bd_PS)))
  }
  cat("\nPor estudio (términos, PS_lo<0,5, cota exacta = 0):\n")
  print(aggregate(cbind(n = 1, ps_lo_bajo, cota_no_inf) ~ estudio + estrato, data = t5, FUN = sum))

  cat("\n=== 2. Consistencia difusa recalculada frente a las medidas causales (AUC aparente, IC 95 % bootstrap por estudio) ===\n")
  for (s in c("cs", "fs", "todos")) {
    z <- if (s == "todos") t5 else t5[t5$estrato == s, ]
    z$ps_ok <- 1 - z$ps_lo_bajo; z$cota_inf <- 1 - z$cota_no_inf
    a1 <- auc_ic(z, "ps_ok"); a2 <- auc_ic(z, "cota_inf")
    cat(sprintf("%-5s PS_lo >= 0,5: AUC %.3f [%.3f, %.3f] | cota exacta > 0: AUC %.3f [%.3f, %.3f]\n",
                s, a1[1], a1[2], a1[3], a2[1], a2[2], a2[3]))
  }
  cat("(En cs la consistencia vale 1 en casi todos los términos: la AUC no es informativa allí.)\n")
  cat("Rango de la consistencia difusa en términos con PS_lo < 0,5:", round(range(t5$incl_fuzzy[t5$ps_lo_bajo == 1]), 3), "\n")

  cat("\n=== 3. Complejidad, casos y P(Y|~X) (descriptivo; parte de estas relaciones se sigue de las fórmulas) ===\n")
  t5$log_n_in <- log(t5$n_in); t5$frac_in <- t5$n_in / t5$N
  m1 <- lm(log(frac_in) ~ k, data = t5)
  cat("\n3a. log(n_in/N) ~ k (MCO; CR2, gl de Satterthwaite)\n"); print(cr2(m1, t5), row.names = FALSE)
  cat(sprintf("Spearman k ~ n_in/N: %.3f\n", cor(t5$k, t5$frac_in, method = "spearman")))
  f2 <- cota_no_inf ~ log_n_in + p_y_fuera + incl_fuzzy
  m2 <- glm(f2, family = binomial, data = t5)
  cat("\n3b. Cota exacta = 0 ~ log(n_in) + P(Y|~X) + consistencia (logit; CR2)\n"); print(cr2(m2, t5), row.names = FALSE)
  fi <- logistf(f2, data = t5)
  cat("Firth (penalizado, robusto a separación): ", paste(sprintf("%s %.2f (p %.3g)", names(coef(fi)), coef(fi), fi$prob), collapse = "; "), "\n")
  ds <- glm(f2, family = binomial, data = t5, method = detectseparation::detect_separation)
  cat("Separación (detectseparation):", if (isTRUE(ds$outcome)) "SÍ, estimaciones ML infinitas: usar Firth" else "no detectada", "
")
  if (sum(t5$ps_lo_bajo) >= 8) {
    m3 <- glm(ps_lo_bajo ~ log_n_in + p_y_fuera + incl_fuzzy, family = binomial, data = t5)
    cat("\n3c. PS_lo < 0,5 ~ log(n_in) + P(Y|~X) + consistencia (logit; CR2)\n"); print(cr2(m3, t5), row.names = FALSE)
    fi3 <- logistf(ps_lo_bajo ~ log_n_in + p_y_fuera + incl_fuzzy, data = t5)
    cat("Firth: ", paste(sprintf("%s %.2f (p %.3g)", names(coef(fi3)), coef(fi3), fi3$prob), collapse = "; "), "\n")
  } else cat("\n3c. omitido: menos de 8 términos con PS_lo < 0,5\n")
  m4 <- lm(bd_PNS ~ log_n_in + p_y_fuera + k + incl_fuzzy, data = t5)
  cat("\n3d. Quiebre de PNS ~ log(n_in) + P(Y|~X) + k + consistencia (MCO; CR2)\n"); print(cr2(m4, t5), row.names = FALSE)
  cat("\nDejar un estudio fuera (rango de coeficientes del logit Firth 3b):\n")
  loo <- t(sapply(unique(t5$estudio), function(e) coef(logistf(f2, data = t5[t5$estudio != e, ]))))
  print(round(apply(loo, 2, range), 2))
  cat("\nTramos de n_in:\n")
  t5$tramo_n <- cut(t5$n_in, c(0, 5, 15, 50, Inf), labels = c("1-5", "6-15", "16-50", ">50"))
  print(cbind(aggregate(cbind(ps_lo_bajo, cota_no_inf, bd_PNS) ~ tramo_n, data = t5, FUN = function(z) round(mean(z), 2)),
              n = as.vector(table(droplevels(t5$tramo_n)))))
  cat("\nTramos de P(Y|~X):\n")
  t5$tramo_base <- cut(t5$p_y_fuera, c(-0.01, 0.25, 0.5, 0.75, 1), labels = c("<=0,25", "0,25-0,5", "0,5-0,75", ">0,75"))
  print(cbind(aggregate(cbind(ps_lo_bajo, cota_no_inf, bd_PNS) ~ tramo_base, data = t5, FUN = function(z) round(mean(z), 2)),
              n = as.vector(table(droplevels(t5$tramo_base)))))
}

t5 <- tt[tt$corte == 0.5 & tt$nivel == "termino", ]
cat("Elegibilidad (estudios):\n"); print(table(eleg$calidad))
analizar(t5[t5$calidad == "exacta", ], "ANÁLISIS PRINCIPAL: solo réplicas exactas (protocolo, criterio 3)")
analizar(t5[t5$calidad %in% c("exacta", "aproximada", "reconstruida"), ], "SENSIBILIDAD: exactas + aproximadas + reconstruidas")

cat("\n\n=== 4. Sensibilidad al corte (solo fsQCA, réplicas exactas): 0,4 / 0,5 / 0,6 ===\n")
f3 <- tt[tt$estrato == "fs" & tt$calidad == "exacta" & tt$nivel == "termino", ]
print(aggregate(cbind(ps_lo_bajo, cota_no_inf, PS_lo, bd_PS, bd_PNS, n_in) ~ corte, data = f3,
                FUN = function(z) round(c(media = mean(z), mediana = median(z)), 3)))
cat("Términos evaluables por corte:\n"); print(table(f3$corte))
w <- reshape(f3[, c("estudio", "outcome", "term", "corte", "ps_lo_bajo", "cota_no_inf")],
             idvar = c("estudio", "outcome", "term"), timevar = "corte", direction = "wide")
w <- w[complete.cases(w), ]
cat(sprintf("Términos evaluables en los tres cortes: %d\n", nrow(w)))
cat(sprintf("Cambian de clase PS_lo<0,5 entre 0,4 y 0,5: %d | entre 0,5 y 0,6: %d\n",
            sum(w$ps_lo_bajo.0.4 != w$ps_lo_bajo.0.5), sum(w$ps_lo_bajo.0.6 != w$ps_lo_bajo.0.5)))
cat(sprintf("Cambian de clase cota exacta=0 entre 0,4 y 0,5: %d | entre 0,5 y 0,6: %d\n",
            sum(w$cota_no_inf.0.4 != w$cota_no_inf.0.5), sum(w$cota_no_inf.0.6 != w$cota_no_inf.0.5)))
cat(sprintf("PS_lo<0,5 en algún corte: %d | en los tres: %d\n",
            sum(w$ps_lo_bajo.0.4 | w$ps_lo_bajo.0.5 | w$ps_lo_bajo.0.6), sum(w$ps_lo_bajo.0.4 & w$ps_lo_bajo.0.5 & w$ps_lo_bajo.0.6)))
write.csv(w, "analisis/sensibilidad_corte.csv", row.names = FALSE)


cat("\n\n=== 5. Medidas secundarias del protocolo (términos, réplicas exactas, corte 0,5) ===\n")
z <- t5[t5$calidad == "exacta", ]
q <- function(v) sprintf("mediana %.3f [P25 %.3f; P75 %.3f]", median(v, na.rm = TRUE),
                         quantile(v, .25, na.rm = TRUE), quantile(v, .75, na.rm = TRUE))
for (s in c("cs", "fs", "todos")) {
  w <- if (s == "todos") z else z[z$estrato == s, ]
  cat(sprintf("\n[%s] n = %d\n", s, nrow(w)))
  cat("  PN_lo:", q(w$PN_lo), "\n")
  cat("  Ancho de identificación PS_up - PS_lo:", q(w$ancho_id), "\n")
  cat("  Ancho bootstrap (1000) de PS_lo:", q(w$ancho_boot),
      sprintf("| degenerado (= 0) en %d de %d (sin contraejemplos)\n", sum(w$ancho_boot == 0, na.rm = TRUE), sum(!is.na(w$ancho_boot))))
  cat(sprintf("  Sensibilidad a tau: PS_lo < 0,3: %.0f %% | < 0,5: %.0f %% | < 0,7: %.0f %%\n",
              100 * mean(w$PS_lo < 0.3), 100 * mean(w$PS_lo < 0.5), 100 * mean(w$PS_lo < 0.7)))
  cat("  Quiebre de PS con tau 0,3 / 0,5 / 0,7 (medianas):",
      sprintf("%.3f / %.3f / %.3f\n", median(w$bd_PS_t03), median(w$bd_PS), median(w$bd_PS_t07)))
}
cat("\nIncertidumbre dominante por término (exactas; la mayor entre ancho bootstrap, ancho de identificación\n",
    "y 1 - quiebre de PNS; descriptivo):\n", sep = "")
dom <- with(z, c("muestreo", "identificación", "confusión")[max.col(cbind(ancho_boot, ancho_id, 1 - bd_PNS), ties.method = "first")])
print(table(estrato = z$estrato, dominante = dom))

cat("\n\n=== 6. Soluciones completas (disyunción de los términos publicados; réplicas exactas, corte 0,5) ===\n")
so <- tt[tt$nivel == "solucion" & tt$corte == 0.5 & tt$calidad == "exacta", ]
so$ps_lo_bajo <- as.integer(so$PS_lo < 0.5); so$cota_no_inf <- as.integer(so$PS_exact95 == 0)
for (s in c("cs", "fs", "todos")) {
  w <- if (s == "todos") so else so[so$estrato == s, ]
  cat(sprintf("%-5s soluciones=%3d | PS_lo<0,5: %2d (%.0f %%) | cota exacta = 0: %2d (%.0f %%) | mediana n_in %.0f | mediana quiebre PNS %.3f | mediana PNS_lo %.3f\n",
              s, nrow(w), sum(w$ps_lo_bajo), 100 * mean(w$ps_lo_bajo), sum(w$cota_no_inf), 100 * mean(w$cota_no_inf),
              median(w$n_in), median(w$bd_PNS), median(w$PNS_lo)))
}
