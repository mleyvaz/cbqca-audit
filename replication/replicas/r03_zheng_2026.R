ESTUDIO <- "R03 Zheng 2026"
## Réplica + CB-QCA: Zheng et al. (2026), Scientific Reports 16:23972, doi 10.1038/s41598-026-55404-0.
## Datos: Mendeley Data x22x9w7n5s ("Continuance Usage.xlsx"), N = 704. Constructos = media de ítems
## (1.x SO, 2.x EA, 3.x SA, 4.x SL, 5.x SC, 6.x WT; asignación comprobada con las medias de la Tabla 8).
## Anclas = media ± DE (Tabla 8). Caminos de la Tabla 9.
suppressPackageStartupMessages({ library(QCA); library(readxl); library(cbqca) })
d <- as.data.frame(read_excel("../datos/zheng_2026/data.xlsx", sheet = 1, .name_repair = "minimal"))
it <- function(p) rowMeans(d[, substr(names(d), 1, 2) == paste0(p, "."), drop = FALSE])
raw <- data.frame(SO = it("1"), EA = it("2"), SA = it("3"), SL = it("4"), SC = it("5"), WT = it("6"))
cat("Medias (Tabla 8: SO 3.35 EA 3.82 SA 3.75 SL 3.90 SC 3.53 WT 3.77):\n"); print(round(colMeans(raw), 2))
anc <- list(SO = c(2.105, 3.35, 4.588), EA = c(2.772, 3.82, 4.867), SA = c(2.721, 3.75, 4.779),
            SL = c(2.949, 3.90, 4.860), SC = c(2.519, 3.53, 4.538), WT = c(2.816, 3.77, 4.724))
X <- as.data.frame(lapply(names(anc), function(v) calibrate(raw[[v]], type = "fuzzy", thresholds = anc[[v]])))
names(X) <- names(anc)
terms <- c(C1 = "EA*SL", C2 = "SA*SL", C3 = "SL*SC", C4 = "~SO*EA*SA", C5 = "~SO*EA*SC", C6 = "SO*EA*~SA*~SC")
pub <- c(0.911, 0.914, 0.927, 0.874, 0.876, 0.801); pcov <- c(0.833, 0.826, 0.736, 0.266, 0.223, 0.156)
rep <- t(sapply(terms, function(t) unlist(pof(t, "WT", data = X, relation = "sufficiency")$incl.cov[1, c("inclS", "covS")])))
print(data.frame(term = names(terms), incl_pub = pub, incl_rep = round(rep[, 1], 3), cov_pub = pcov, cov_rep = round(rep[, 2], 3)), row.names = FALSE)
r <- cbqca(X, "WT", unname(terms), tau = 0.5, boot = 1000); r$table$term <- names(terms)
print(r)

# Complemento 17-nov: cota exacta y sensibilidad a tau (desviación declarada del protocolo)
source("csqca_comun.R")
extra_cs(X, "WT", unname(terms), names(terms))
