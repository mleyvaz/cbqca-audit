# Análisis adicionales pedidos por la revisión de método (19-nov-2026). Liviano: fórmulas cerradas, sin LP.
# Uso (desde auditoria/): Rscript analisis/extra.R > analisis/extra.txt
#  (a) Prueba exacta de Fisher unilateral por término (contraste con la cota Bonferroni–Clopper–Pearson).
#  (b) PS de cada camino frente a los casos FUERA DE TODA LA SOLUCIÓN (~S), no solo fuera del camino (~X):
#      responde a la objeción de equifinalidad (los casos de caminos hermanos inflan P(Y|~X)).
#  (c) Resúmenes ponderados por estudio (cada estudio pesa 1), con IC bootstrap por estudio.
#  (d) Identidad del modelo δ: quiebre de PNS = PNS_lo / 2.
suppressPackageStartupMessages(library(cbqca))
source("replicas/csqca_comun.R")
el <- read.csv("analisis/elegibilidad.csv")
tt <- read.csv("analisis/tabla_terminos.csv")

ps_lo_cf <- function(p1, p0) if (p0 >= 1) 0 else max(0, (p1 - p0) / (1 - p0))
exact_cf <- function(k1, n1, k0, n0) {           # misma regla que ps_lo_exacta, con grupos dados
  p1L <- if (k1 == 0) 0 else qbeta(0.025, k1, n1 - k1 + 1)
  p0U <- if (k0 == n0) 1 else qbeta(0.975, k0 + 1, n0 - k0)
  if (p0U >= 1) 0 else max(0, (p1L - p0U) / (1 - p0U))
}
cod_de <- function(f) {
  c1 <- regmatches(basename(f), regexpr("^R[0-9]+", basename(f)))
  if (length(c1)) return(c1)
  m <- c(Krook = "R04", Lam = "R05", Chai = "R06", Gong = "R07", Xiao = "R08")
  unname(m[sapply(names(m), function(k) startsWith(basename(f), k))])
}
filas <- list()
for (f in list.files("replicas/corpus", full.names = TRUE)) {
  e <- readRDS(f); d <- e$data; cod <- cod_de(f)
  cal <- el$calidad[el$codigo == cod]; if (!length(cal) || cal != "exacta") next
  yv <- suppressWarnings(as.numeric(as.character(d[[e$outcome]])))
  sv <- cbqca:::.term_membership(d, paste(unique(e$terms), collapse = " + "))
  cs <- all(unlist(lapply(d, function(z) suppressWarnings(as.numeric(as.character(z))))) %in% c(0, 1, NA))
  for (tm in unique(e$terms)) {
    xv <- cbqca:::.term_membership(d, tm)
    ok <- !is.na(xv) & !is.na(yv) & !is.na(sv)
    x <- xv[ok] > 0.5; y <- yv[ok] > 0.5; s <- sv[ok] > 0.5
    if (sum(x) == 0 || sum(!x) == 0) next
    n1 <- sum(x); k1 <- sum(x & y); n0 <- sum(!x); k0 <- sum(!x & y)
    fp <- fisher.test(matrix(c(k1, n1 - k1, k0, n0 - k0), 2, byrow = TRUE), alternative = "greater")$p.value
    nS <- sum(!s); kS <- sum(!s & y)
    filas[[length(filas) + 1]] <- data.frame(
      estudio = cod, estrato = if (cs) "cs" else "fs", outcome = e$outcome, term = tm, n_in = n1,
      p1 = k1 / n1, p0 = k0 / n0, fisher_p = fp,
      PS_lo = ps_lo_cf(k1 / n1, k0 / n0), exact = exact_cf(k1, n1, k0, n0),
      n_outS = nS, p0S = if (nS > 0) kS / nS else NA,
      PS_lo_S = if (nS > 0) ps_lo_cf(k1 / n1, kS / nS) else NA,
      exact_S = if (nS > 0) exact_cf(k1, n1, kS, nS) else NA)
  }
}
z <- do.call(rbind, filas)
z <- z[!duplicated(z[, c("estudio", "outcome", "term")]), ]
write.csv(z, "analisis/extra_terminos.csv", row.names = FALSE)
cat("Términos (exactas, corte 0,5):", nrow(z), "| estudios:", length(unique(z$estudio)),
    "| cs:", sum(z$estrato == "cs"), "| fs:", sum(z$estrato == "fs"), "\n")
mm <- merge(z, tt[tt$corte == 0.5 & tt$nivel == "termino", c("estudio", "outcome", "term", "PS_lo")],
            by = c("estudio", "outcome", "term"))
cat("Control: PS_lo de forma cerrada frente al LP de tabla_terminos: filas", nrow(mm), "| máx. dif.",
    signif(max(abs(mm$PS_lo.x - mm$PS_lo.y)), 3), "\n")

cat("\n=== (a) Fisher unilateral frente a la cota exacta ===\n")
for (s in c("cs", "fs", "todos")) {
  w <- if (s == "todos") z else z[z$estrato == s, ]
  cat(sprintf("%-5s n=%3d | Fisher p < 0,05: %3d (%.0f %%) | cota exacta > 0: %3d (%.0f %%) | Fisher p<0,05 con cota = 0: %3d\n",
              s, nrow(w), sum(w$fisher_p < 0.05), 100 * mean(w$fisher_p < 0.05), sum(w$exact > 0), 100 * mean(w$exact > 0),
              sum(w$fisher_p < 0.05 & w$exact == 0)))
}
cat("Por tramos de n_in (todos): proporción con Fisher p < 0,05 | con cota exacta > 0\n")
z$tramo <- cut(z$n_in, c(0, 5, 15, 50, Inf), labels = c("1-5", "6-15", "16-50", ">50"))
print(cbind(aggregate(cbind(fisher05 = fisher_p < 0.05, cota_pos = exact > 0) ~ tramo, data = z, FUN = function(v) round(mean(v), 2)),
            n = as.vector(table(z$tramo))))
cat("Nota: Fisher contrasta P(Y|X) > P(Y|~X) (PNS_lo > 0); la cota exacta exige PS_lo > 0 con 95 % conjunto. Ambas por término.\n")

cat("\n=== (b) Camino frente a casos fuera de TODA la solución (~S) ===\n")
w0 <- z[!is.na(z$p0S), ]
for (s in c("cs", "fs", "todos")) {
  w <- if (s == "todos") w0 else w0[w0$estrato == s, ]
  cat(sprintf("%-5s n=%3d | mediana P(Y|~X) %.3f -> P(Y|~S) %.3f | PS_lo<0,5: %2d -> %2d | cota exacta = 0: %3d (%.0f %%) -> %3d (%.0f %%)\n",
              s, nrow(w), median(w$p0), median(w$p0S), sum(w$PS_lo < 0.5), sum(w$PS_lo_S < 0.5),
              sum(w$exact == 0), 100 * mean(w$exact == 0), sum(w$exact_S == 0), 100 * mean(w$exact_S == 0)))
}
cat("Términos sin casos fuera de la solución (excluidos de (b)):", sum(is.na(z$p0S)), "\n")
cat("Por tramos de n_in: cota exacta = 0 frente a ~X | frente a ~S\n")
print(cbind(aggregate(cbind(vs_X = exact == 0, vs_S = exact_S == 0) ~ tramo, data = w0, FUN = function(v) round(mean(v), 2)),
            n = as.vector(table(droplevels(w0$tramo)))))

cat("\n=== (c) Ponderado por estudio (cada estudio pesa 1; IC 95 % bootstrap por estudio, 2000) ===\n")
set.seed(1)
por <- aggregate(cbind(cota0 = exact == 0, pslo = PS_lo < 0.5, cota0_S = exact_S == 0) ~ estudio + estrato,
                 data = transform(z, exact_S = ifelse(is.na(exact_S), exact, exact_S)), FUN = mean)
for (s in c("cs", "fs", "todos")) {
  w <- if (s == "todos") por else por[por$estrato == s, ]
  bs <- replicate(2000, colMeans(w[sample(nrow(w), replace = TRUE), c("cota0", "pslo", "cota0_S")]))
  ci <- apply(bs, 1, quantile, c(0.025, 0.975))
  cat(sprintf("%-5s estudios=%2d | cota exacta = 0: %.2f [%.2f, %.2f] | PS_lo<0,5: %.2f [%.2f, %.2f] | cota frente a ~S = 0: %.2f [%.2f, %.2f]\n",
              s, nrow(w), mean(w$cota0), ci[1, 1], ci[2, 1], mean(w$pslo), ci[1, 2], ci[2, 2], mean(w$cota0_S), ci[1, 3], ci[2, 3]))
}

cat("\n=== (d) Identidad del modelo δ ===\n")
x5 <- tt
d5 <- abs(x5$bd_PNS - x5$PNS_lo / 2)
cat(sprintf("Filas de tabla_terminos (todos los niveles y cortes): %d | máx |quiebre PNS − PNS_lo/2| = %.1e (tolerancia de bisección 1e-4)\n",
            nrow(x5), max(d5, na.rm = TRUE)))
cat("En este modelo el quiebre de PNS no aporta información más allá de PNS_lo (= P(Y|X) − P(Y|~X)).\n")
cat(sprintf("Quiebre de PS: valores de contorno en términos exactos al corte 0,5: = 0 en %d (PS_lo <= tau), = 1 en %d.\n",
            sum(tt$corte == 0.5 & tt$nivel == "termino" & tt$calidad == "exacta" & tt$bd_PS == 0),
            sum(tt$corte == 0.5 & tt$nivel == "termino" & tt$calidad == "exacta" & tt$bd_PS >= 1)))
