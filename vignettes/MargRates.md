## Introduction

**MargRates** provides inference for marginal binary event rates when data are stratified. It is designed for two-arm comparisons where counts are reported by stratum (e.g., by baseline category or study site). The package estimates marginal event rates in each arm by weighting stratum-specific rates, then compares arms using the risk difference, risk ratio, and odds ratio, with optional bootstrap confidence intervals and permutation tests.

### Mathematical framework

Suppose we have $J$ strata. In stratum $j$, arm 0 has $y_{j0}$ events out of $n_{j0}$ subjects and arm 1 has $y_{j1}$ events out of $n_{j1}$ subjects. Let $r_{j0} = y_{j0}/n_{j0}$ and $r_{j1} = y_{j1}/n_{j1}$ denote the stratum-specific event rates.

**Weights.** A set of stratum weights $w_1, \ldots, w_J$ with $\sum_j w_j = 1$ is used to form marginal rates. By default, MargRates uses the stratum proportions of the total sample size:
$$
w_j = \frac{n_{j0} + n_{j1}}{N}, \qquad N = \sum_{j=1}^J (n_{j0} + n_{j1}).
$$

**Marginal event rates.** The marginal event rate in each arm is the weighted average of the stratum-specific rates:
$$
p_0 = \sum_{j=1}^J w_j \, r_{j0}, \qquad
p_1 = \sum_{j=1}^J w_j \, r_{j1}.
$$

**Contrasts.** The package reports three marginal contrasts:

- **Risk difference (RD):** $\theta_{\text{RD}} = p_1 - p_0$.
- **Risk ratio (RR):** $\theta_{\text{RR}} = p_1 / p_0$ (with inference on $\log \theta_{\text{RR}}$).
- **Odds ratio (OR):** $\theta_{\text{OR}} = \frac{p_1/(1-p_1)}{p_0/(1-p_0)}$ (with inference on $\log \theta_{\text{OR}}$).

**Asymptotic inference.** Standard errors for RD are obtained from the usual binomial variance of the marginal rates. For RR and OR, the delta method is applied on the log scale; confidence intervals are then exponentiated back to the ratio scale. Two-sided $p$-values use the normal approximation.

**Optional resampling.** Bootstrap confidence intervals resample event counts within each cell (fixing $n_{j0}$, $n_{j1}$) and recompute the marginal statistics. A permutation test of the null hypothesis of no treatment effect conditions on the marginal totals in each stratum and permutes the split of events between arms within strata (via the hypergeometric distribution).

---

## Example: COVID-19 trial data

We illustrate with 28-day mortality from a COVID-19 trial, stratified by respiratory support at randomization. The data are stored as stratum-level event counts and sample sizes for the control arm (0) and the treatment arm (1).


``` r
library(MargRates)

# Event counts (y) and sample sizes (n) by stratum; arm 0 = control, arm 1 = treatment.
y0 <- c(283, 682, 145)
n0 <- c(683, 2604, 1034)
y1 <- c(95, 298, 89)
n1 <- c(324, 1279, 501)
```

**Asymptotic analysis only:**


``` r
out <- CompMargRates(
  y0 = y0,
  n0 = n0,
  y1 = y1,
  n1 = n1,
  alpha = 0.05,
  boot = FALSE,
  perm = FALSE
)
show(out)
#> Marginal Rates:
#>   Arm    N     Rates
#> 1   0 4321 0.2567286
#> 2   1 2104 0.2292085
#> 
#> 
#> Risk Difference:
#>       Method     Stat        Est         SE       Lower        Upper          P
#> 1 Asymptotic RiskDiff -0.0275201 0.01121997 -0.04951084 -0.005529367 0.01417575
#> 
#> 
#> Risk Ratio:
#>       Method      Stat       Est         SE    Lower     Upper          P
#> 2 Asymptotic RiskRatio 0.8928047 0.04218031 0.813845 0.9794251 0.01639499
#> 
#> 
#> Odds Ratio:
#>       Method      Stat       Est         SE     Lower     Upper          P
#> 3 Asymptotic OddsRatio 0.8609283 0.05334131 0.7624797 0.9720881 0.01565469
```

**With bootstrap and permutation** (for reproducibility, use a larger `reps` in practice):


``` r
set.seed(2013)
out_full <- CompMargRates(
  y0 = y0,
  n0 = n0,
  y1 = y1,
  n1 = n1,
  alpha = 0.05,
  boot = TRUE,
  perm = TRUE,
  reps = 500
)
show(out_full)
#> Marginal Rates:
#>   Arm    N     Rates
#> 1   0 4321 0.2567286
#> 2   1 2104 0.2292085
#> 
#> 
#> Risk Difference:
#>       Method     Stat        Est         SE       Lower        Upper          P
#> 1 Asymptotic RiskDiff -0.0275201 0.01121997 -0.04951084 -0.005529367 0.01417575
#> 4  Bootstrap RiskDiff -0.0275201 0.01148952 -0.05250397 -0.005205896 0.00800000
#> 
#> 
#> Risk Ratio:
#>       Method      Stat       Est         SE     Lower     Upper          P
#> 2 Asymptotic RiskRatio 0.8928047 0.04218031 0.8138450 0.9794251 0.01639499
#> 5  Bootstrap RiskRatio 0.8928047 0.04314530 0.8021671 0.9787042 0.00800000
#> 
#> 
#> Odds Ratio:
#>       Method      Stat       Est         SE     Lower     Upper          P
#> 3 Asymptotic OddsRatio 0.8609283 0.05334131 0.7624797 0.9720881 0.01565469
#> 6  Bootstrap OddsRatio 0.8609283 0.05453990 0.7487346 0.9720065 0.00800000
#> 
#> 
#> Permutation test:
#>        Stat        Est          P
#> 1  RiskDiff -0.0275201 0.01197605
#> 2 RiskRatio  0.8928047 0.00998004
#> 3 OddsRatio  0.8609283 0.01197605
```

The output lists marginal rates in each arm and, for each contrast (risk difference, risk ratio, odds ratio), the estimate (Est), standard error (SE), confidence limits (Lower, Upper), and $p$-value (P). When `boot = TRUE`, bootstrap SEs and intervals are appended; when `perm = TRUE`, permutation $p$-values are given in the Permutation test section.

---

## References

- COVID-19 trial data: [RECOVERY Collaborative Group, *NEJM* (2021)](https://www.nejm.org/doi/full/10.1056/NEJMoa2021436).
