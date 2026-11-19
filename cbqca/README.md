# cbqca: Causal-Bounds QCA

`cbqca` reads the solution terms of a qualitative comparative analysis (QCA) as
causal claims. For each term it reports:

- the classic sufficiency consistency (fuzzy and crisp);
- exact bounds on Pearl's probabilities of necessity (PN), sufficiency (PS) and
  necessity-and-sufficiency (PNS), computed by linear programming over response
  types (Tian and Pearl, 2000), under exogeneity and without assumptions;
- the **configurational breakdown point**: the smallest level of confounding at
  which the causal reading is no longer guaranteed;
- optional bootstrap intervals, so that sampling uncertainty is reported apart
  from identification uncertainty.

The package is self-contained. It does not require the `piqca` replication code.

## Installation (from source)

```r
# in the folder that contains cbqca/
install.packages("cbqca", repos = NULL, type = "source")
```

Dependencies: `lpSolve`. Tests: `testthat`.

## Example

```r
library(cbqca)
r <- cbqca(data, outcome = "Y",
           terms = c("A*~B", "C"),   # QCA notation: * AND, + OR, ~ NOT
           tau = 0.5, boot = 1000)
print(r)
```

`inst/examples/c02_demo.R` applies the method to a published fsQCA study
(Kurz and Ettensperger 2024, *European Political Science*; data on Harvard
Dataverse, doi 10.7910/DVN/CDZMPM). Output in `inst/examples/c02_demo_output.txt`.

## Confounding model

Response types (Y0, Y1) are never, causal, preventive and always. The parameter
`delta` bounds how much the share of each type may differ between cases inside
and outside the configuration: `delta = 0` is exogeneity, `delta = 1` imposes
nothing beyond the data. The breakdown point is the smallest `delta` at which the
lower bound of the chosen probability falls to `tau`.

## Validation

`tests/testthat/test-bounds.R` checks that the linear programme at `delta = 0`
reproduces the closed-form exogeneity bounds of Tian and Pearl on 300 random
tables, that bounds widen monotonically with `delta`, and that 2,000 randomly
drawn response-type tables consistent with the data never fall outside the
bounds.

## Scope and limits

- Probabilities of causation are defined for crisp sets. Fuzzy memberships are
  dichotomised at the crossover (default 0.5) for the causal part; the fuzzy
  consistency is reported alongside. A fuzzy extension is future work.
- The bounds treat each term as a binary exposure against all other cases.
- Exogeneity is an assumption about the design, not something the data can test.

## Reference

Tian, J., and Pearl, J. (2000). Probabilities of causation: Bounds and
identification. *Annals of Mathematics and Artificial Intelligence*, 28,
287-313. doi:10.1023/A:1018912507879

License: MIT.
