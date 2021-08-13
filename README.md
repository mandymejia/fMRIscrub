
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fMRIscrub

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mandymejia/fMRIscrub.svg?branch=master)](https://travis-ci.org/github/mandymejia/fMRIscrub)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mandymejia/fMRIscrub?branch=master&svg=true)](https://ci.appveyor.com/project/mandymejia/fMRIscrub)
[![Coveralls test
coverage](https://coveralls.io/repos/github/mandymejia/fMRIscrub/badge.svg)](https://coveralls.io/r/mandymejia/fMRIscrub?branch=master)
<!-- badges: end -->

`fMRIscrub` is a collection of routines for data-driven scrubbing
(projection scrubbing and DVARS), motion scrubbing, and other fMRI
denoising strategies such as anatomical CompCor, detrending, and
nuisance regression. The data-driven scrubbing methods are also
applicable to other outlier detection tasks involving high-dimensional
data.

## Installation

You can install the development version of fMRIscrub from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mandymejia/fMRIscrub")
```

## Quick start guide

``` r
s_Dat1 <- scrub(Dat1)
plot(s_Dat1)
Dat1_cleaned <- Dat1[!s_Dat1$outlier_flag,]
```

## Vignette

See [this
link](https://htmlpreview.github.io/?https://github.com/mandymejia/fMRIscrub/blob/master/vignettes/fMRIscrub_vignette.html)
to view the tutorial vignette.

## Citation

If using projection scrubbing, you can cite our pre-print at
<https://arxiv.org/abs/2108.00319>.
