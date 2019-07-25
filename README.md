# EDMsimulate - an R package for simulating fish populations in the context of Empirical Dynamic Modelling

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/andrew-edwards/EDMsimulate.svg?branch=master)](https://travis-ci.org/andrew-edwards/EDMsimulate)
[![Coverage status](https://codecov.io/gh/andrew-edwards/EDMsimulate/branch/master/graph/badge.svg)](https://codecov.io/github/andrew-edwards/EDMsimulate?branch=master)
<!-- badges: end -->

Not operational yet.

### Vignette

The vignette showing examples of the functions is available [here](http://htmlpreview.github.io/?https://github.com/andrew-edwards/EDMsimulate/blob/master/doc/salmon_sim.html), and directly in the package (see below).

### To install from GitHub

You need the package `devtools`, so you need to install it once:
```
install.packages("devtools")
```

This enables installation directly from the GitHub site:

```
devtools::install_github("andrew-edwards/EDMsimulate", build_vignettes = TRUE)
library(EDMsimulate)
```

Then try:

```
vignette("salmon_sim", package="EDMsimulate")
```

which should show the vignette in an html browser.

It's hard for me to test a clean build, let me know if it's working. 
