# How causal are published QCA solutions? Replication materials

Replication materials for:

> Leyva-Vázquez, Maikel. 2026. "How Causal Are Published QCA Solutions? Bounding the Probabilities of
> Causation behind Configurational Findings." SocArXiv preprint. https://doi.org/10.31235/osf.io/9gqpx_v1

The study replicates published Qualitative Comparative Analysis (QCA) solutions from open data and bounds
Pearl's probabilities of necessity (PN), sufficiency (PS) and necessity-and-sufficiency (PNS) for every
published path and every complete solution (procedure: CB-QCA).

## Contents

| Folder | What it holds |
|---|---|
| `cbqca/` | R package `cbqca` 0.1.0: exact bounds on PN, PS and PNS by linear programming over response types, breakdown points under a bounded-confounding model, and the exact (Bonferroni–Clopper–Pearson) sampling bound `ps_exact_bound()`. MIT licence. |
| `replication/replicas/` | One R script per attempted study (`rNN_author_year.R`) with its console output (`.txt`). `csqca_comun.R` holds the shared functions; `corpus/` holds the derived data of every registered study. |
| `replication/analisis/` | `corpus.R` (main and sensitivity analyses), `extra.R` (Fisher contrast, comparison with cases outside the whole solution, study-weighted summaries, check of the PNS-breakdown identity), their outputs and the per-term tables. `elegibilidad.csv` records the replication category of every study. |
| `replication/datos/` | Data files obtained from the original publications' supplementary materials or repositories (one folder per study). |
| `replication/PROTOCOLO_AUDITORIA.md` | Written protocol, version 1, dated 16 November 2026 (Spanish). |
| `replication/INSTRUCCIONES_REPLICA.md` | Replication rules applied to every study (Spanish). |

Script comments and console labels are in Spanish; the article and its Supplementary Appendix give the
English definitions. Key terms: *exacta* = exact replication, *aproximada* = approximate, *reconstruida* =
reconstructed, *cota exacta* = exact sampling bound, *quiebre* = breakdown point, *término* = path,
*solución* = complete solution, *corte* = crossover.

## How to reproduce

Requirements: R ≥ 4.3 with packages `QCA`, `lpSolve`, `readxl`, `sandwich`, `lmtest`, `clubSandwich`,
`logistf`, `detectseparation`, `testthat`.

```r
# 1. install the package
install.packages("cbqca", repos = NULL, type = "source")   # from the repository root
# 2. from the folder replication/, run any replication, e.g.
#    Rscript replicas/r17_ziemann_2022.R
#    (R01–R03 are run from replication/replicas/)
# 3. from replication/, run the cross-study analyses
#    Rscript analisis/corpus.R > analisis/corpus.txt   # resumable; first run takes a while (bootstrap)
#    Rscript analisis/extra.R  > analisis/extra.txt
```

Each replication script registers its study in `replicas/corpus/`; `analisis/corpus.R` reads that folder.
Studies classified as not replicable have no file there.

## Data and licences

The code in this repository is released under the MIT licence. The data in `replication/datos/` were
published by the authors of the replicated studies as supplementary material or in public repositories;
each remains under the licence of its original source (most are open-access articles under CC BY). The
DOI of every source article is listed in the header of its replication script and in Table A1 of the
Supplementary Appendix. Full texts of articles are not redistributed.

## Use of AI tools

AI assistants (Claude, Codex and Gemini) were used to search for candidate studies, write replication
code, draft text and run adversarial reviews. The author checked every number against the scripts and
takes full responsibility for the content.

## Contact

Maikel Leyva-Vázquez, Universidad Bernardo O'Higgins (Chile), Universidad Bolivariana del Ecuador and
Universidad de Guayaquil (Ecuador). ORCID 0000-0001-7911-5879.
