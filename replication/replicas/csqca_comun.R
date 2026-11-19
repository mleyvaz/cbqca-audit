# Funciones comunes para réplicas csQCA (estrato A, análisis principal).
# Uso: source("replicas/csqca_comun.R"); replicar_cs(datos, "Y", c("A*~B","C"), id = "...")
suppressPackageStartupMessages({ library(QCA); library(cbqca) })

# Cota exacta de muestreo para la PS mínima bajo exogeneidad (Clopper-Pearson, Bonferroni
# 2 x 2,5 %): P(Y|X) en su límite inferior y P(Y|~X) en su límite superior.
ps_lo_exacta <- function(datos, outcome, termino, umbral = 0.5) {
  yv <- suppressWarnings(as.numeric(as.character(datos[[outcome]])))
  xv <- cbqca:::.term_membership(datos, termino)
  ok <- !is.na(xv) & !is.na(yv)
  x <- as.integer(xv[ok] > umbral); y <- as.integer(yv[ok] > umbral)
  n1 <- sum(x == 1); k1 <- sum(x == 1 & y == 1)
  n0 <- sum(x == 0); k0 <- sum(x == 0 & y == 1)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  p1L <- if (k1 == 0) 0 else stats::qbeta(0.025, k1, n1 - k1 + 1)
  p0U <- if (k0 == n0) 1 else stats::qbeta(0.975, k0 + 1, n0 - k0)
  if (p0U >= 1) return(0)
  max(0, (p1L - p0U) / (1 - p0U))
}


# Registro del corpus: guarda datos, resultado y términos de cada réplica para los análisis
# transversales (prueba de la tesis de complejidad y sensibilidad al corte).
CORPUS_DIR <- if (dir.exists("replicas")) "replicas/corpus" else "corpus"  # relativo a replication/ o a replication/replicas/
registrar <- function(datos, outcome, terminos, etiquetas = terminos, id = "sin_id", publicados = NULL) {
  dir.create(CORPUS_DIR, showWarnings = FALSE)
  f <- file.path(CORPUS_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", paste(id, outcome)), ".rds"))
  saveRDS(list(id = id, outcome = outcome, terms = unname(terminos), labels = unname(etiquetas),
               data = as.data.frame(datos), publicados = publicados), f)
}

# Complemento para réplicas ya hechas: cota exacta y quiebre de PS con tau 0,3/0,5/0,7.
extra_cs <- function(datos, outcome, terminos, etiquetas = terminos,
                     id = get0("ESTUDIO", envir = globalenv(), ifnotfound = "sin_id")) {
  registrar(datos, outcome, terminos, etiquetas, id)
  out <- data.frame(term = etiquetas,
                    PS_lo_exact95 = sapply(terminos, function(t) ps_lo_exacta(datos, outcome, t)))
  for (t in c(0.3, 0.5, 0.7)) {
    r <- suppressWarnings(cbqca(datos, outcome, unname(terminos), tau = t, boot = 0))
    out[[paste0("bdPS_tau", t)]] <- r$table$breakdown_PS
  }
  rownames(out) <- NULL
  num <- vapply(out, is.numeric, logical(1)); out[num] <- lapply(out[num], round, 3)
  cat("\nComplemento (cota exacta al 95 % y sensibilidad a tau):\n"); print(out, row.names = FALSE)
  invisible(out)
}

# Expande filas por una columna de frecuencias (p. ej., Greckhamer et al. 2008).
expandir_frecuencia <- function(d, col = "frequency") {
  d <- as.data.frame(d)
  out <- d[rep(seq_len(nrow(d)), d[[col]]), setdiff(names(d), col), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Réplica: consistencia y cobertura de cada término publicado (QCA::pof) y CB-QCA.
# publicados: data.frame opcional con columnas term, incl_pub, cov_pub para comparar.
replicar_cs <- function(datos, outcome, terminos, id, publicados = NULL,
                        boot = 1000, taus = c(0.3, 0.5, 0.7)) {
  datos <- as.data.frame(datos)
  registrar(datos, outcome, terminos, id = id, publicados = publicados)
  cat("\n=====", id, "| N =", nrow(datos), "| outcome", outcome, "\n")
  cat("Términos publicados:", paste(terminos, collapse = " + "), "\n\n")
  # 1) réplica de los parámetros de ajuste
  conds <- setdiff(names(datos), outcome)
  p <- QCA::pof(paste(terminos, collapse = " + "), outcome = outcome,
                data = datos, relation = "sufficiency")
  ic <- p$incl.cov
  print(round(ic[, c("inclS", "covS", "covU")], 3))
  if (!is.null(publicados)) {
    m <- merge(data.frame(term = rownames(ic), incl_rep = ic$inclS, cov_rep = ic$covS),
               publicados, by = "term", all.y = TRUE)
    m$dif_incl <- round(m$incl_rep - m$incl_pub, 3)
    m$dif_cov <- round(m$cov_rep - m$cov_pub, 3)
    cat("\nComparación con lo publicado:\n"); print(m, row.names = FALSE)
  }
  # 2) CB-QCA (tau = 0,5 principal; 0,3 y 0,7 sensibilidad del protocolo)
  res <- lapply(taus, function(t) {
    r <- cbqca(datos, outcome, terminos, tau = t, boot = if (t == 0.5) boot else 0)
    r$table$tau <- t; r$table
  })
  principal <- res[[which(taus == 0.5)]]
  # Cota exacta (Clopper-Pearson, Bonferroni 2 x 2,5 %): P(Y|X) en su límite inferior y
  # P(Y|~X) en su límite superior. El bootstrap es degenerado cuando no hay contraejemplos.
  principal$PS_lo_exact95 <- sapply(principal$term, function(tm) ps_lo_exacta(datos, outcome, tm))
  cat("\nCB-QCA (tau = 0,5):\n")
  num <- vapply(principal, is.numeric, logical(1))
  pr <- principal; pr[num] <- lapply(pr[num], round, 3)
  print(pr[, intersect(c("term", "n_in", "n_out", "incl_crisp", "PS_lo", "PN_lo", "PNS_lo",
                         "breakdown_PS", "breakdown_PNS", "PS_lo_ci_low", "PS_lo_ci_high",
                         "PS_lo_exact95"),
                       names(pr))], row.names = FALSE)
  sens <- do.call(rbind, lapply(res, function(x) x[, c("term", "tau", "breakdown_PS")]))
  cat("\nSensibilidad del quiebre de PS a tau:\n")
  print(reshape(sens, idvar = "term", timevar = "tau", direction = "wide"), row.names = FALSE)
  invisible(list(fit = ic, cb = principal, sens = sens, id = id))
}
