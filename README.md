
<!-- README.md is generated from README.Rmd. Please edit that file -->

# criskdiff

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Estimation and Inference for the Common Risk Difference from Stratified
2x2 Tables.

## Installation

You can install the development version of the package from
[GitHub](https://github.com/) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("chstock/criskdiff")
```

## Getting started

``` r
library(criskdiff)
data(myel)
criskdiff_mh_sato(myel)
#           est          var           se          lcl          ucl         pval 
#   0.057168322  0.006382032  0.079887623 -0.099408542  0.213745186  0.474232662
```

``` r
criskdiff_mh_klingenberg(myel)
#          est         var          se         lcl         ucl        pval 
#   0.05716832          NA  0.07989779 -0.10192703  0.21126657          NA
```

``` r
criskdiff_score(myel)
#           est          var           se          lcl          ucl         pval 
#   0.032341403  0.003876782  0.062263807 -0.089693416  0.154376222           NA
```
