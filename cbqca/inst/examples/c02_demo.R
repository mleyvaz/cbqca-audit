## CB-QCA on a published fsQCA study:
## Kurz & Ettensperger (2024), European Political Science 23: 349-397,
## doi 10.1057/s41304-023-00460-7; data: Harvard Dataverse doi 10.7910/DVN/CDZMPM.
## Calibration copied from the authors' scripts (as reconstructed in the piqca
## replication code); the published parsimonious solution for high youth
## representation (ARI35) is read term by term as causal claims.
## Usage: Rscript c02_demo.R <path to c02_Youth_QCA_Data.csv>
suppressPackageStartupMessages({ library(QCA); library(cbqca) })
args <- commandArgs(trailingOnly = TRUE)
f <- if (length(args)) args[1] else "c02_Youth_QCA_Data.csv"
d <- read.csv(f, stringsAsFactors = FALSE)
d <- d[d$nr.MPs >= 10, ]
cal <- function(x, th, dig) round(calibrate(x, type = "fuzzy", method = "direct",
                                            thresholds = th, logistic = TRUE), dig)
X <- data.frame(
  YOUTH = d$Youth.index,
  GDP   = cal(d$Gdp.per.cap, c(25000, 36000, 55000), 2),
  ELEC  = ifelse(d$Elec.system %in% c(9, 11), 1, ifelse(d$Elec.system %in% c(1, 2, 3, 10, 12), 0, NA)),
  DEC   = cal(d$Decentralization, c(0.1, 0.48, 0.9), 2),
  PROG  = cal(d$Manifesto.progcons, c(10, -8, -20), 3),
  YOUNG = cal(d$party.age, c(50, 19.9, 3), 3),
  Y     = cal(d$ARI35, c(0.05, 0.6, 0.95), 3))
X <- X[!is.na(X$DEC), ]                     # the 105 parties analysed in the article
terms <- c("~YOUTH*ELEC*PROG", "GDP*ELEC*~DEC*YOUNG", "GDP*ELEC*PROG*~YOUNG",
           "~YOUTH*ELEC*PROG + GDP*ELEC*~DEC*YOUNG + GDP*ELEC*PROG*~YOUNG")
r <- cbqca(X, "Y", terms, tau = 0.5, boot = 1000)
print(r)
