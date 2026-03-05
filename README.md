
Zachary McCaw\
Updated: 2026-03-05

[![R-CMD-check](https://github.com/zrmacc/MargRates/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zrmacc/MargRates/actions/workflows/R-CMD-check.yaml)

## Overview

Given stratified binary event data for two arms, this package calculates
summary statistics comparing two arms with respect to the marginal event
rate. Marginal event rates are calculated as the stratum-size
weighted-average of the per-stratum event rates, then compared via the
risk difference, risk ratio, and odds ratio. For comparing marginal
survival rates in the presence of stratification, see
[StratSurv](https://github.com/zrmacc/StratSurv).

## Installation

``` r
devtools::install_github(repo = 'zrmacc/MargRates')
```

## Example

Consider the following 28-day unadjusted mortality data from a recent
[COVID-19 clinical
trial](https://www.nejm.org/doi/full/10.1056/NEJMoa2021436), stratified
by respiratory support at randomization

``` r
# Event counts.
y0 <- c(283, 682, 145)
n0 <- c(683, 2604, 1034)
y1 <- c(95, 298, 89)
n1 <- c(324, 1279, 501)

# Marginal Odds Ratio
library(MargRates)
set.seed(2013)
out <- CompMargRates(
  y0 = y0,
  n0 = n0,
  y1 = y1,
  n1 = n1,
  alpha = 0.05,
  boot = TRUE,
  perm = TRUE,
  reps = 2e3
)
show(out)
```

    ## Marginal Rates:
    ##   Arm    N     Rates
    ## 1   0 4321 0.2567286
    ## 2   1 2104 0.2292085
    ## 
    ## 
    ## Risk Difference:
    ##       Method     Stat        Est         SE       Lower        Upper          P
    ## 1 Asymptotic RiskDiff -0.0275201 0.01121997 -0.04951084 -0.005529367 0.01417575
    ## 4  Bootstrap RiskDiff -0.0275201 0.01123516 -0.04957270 -0.005407053 0.01700000
    ## 
    ## 
    ## Risk Ratio:
    ##       Method      Stat       Est         SE     Lower     Upper          P
    ## 2 Asymptotic RiskRatio 0.8928047 0.04218031 0.8138450 0.9794251 0.01639499
    ## 5  Bootstrap RiskRatio 0.8928047 0.04233066 0.8123457 0.9782870 0.01700000
    ## 
    ## 
    ## Odds Ratio:
    ##       Method      Stat       Est         SE     Lower     Upper          P
    ## 3 Asymptotic OddsRatio 0.8609283 0.05334131 0.7624797 0.9720881 0.01565469
    ## 6  Bootstrap OddsRatio 0.8609283 0.05358627 0.7608769 0.9712936 0.01700000
    ## 
    ## 
    ## Permutation test:
    ##        Stat        Est          P
    ## 1  RiskDiff -0.0275201 0.01299350
    ## 2 RiskRatio  0.8928047 0.01149425
    ## 3 OddsRatio  0.8609283 0.01199400
