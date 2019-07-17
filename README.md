# EDMsimulate - an R package for simulating fish populations and testing using Empirical Dynamic Modelling

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/andrew-edwards/EDMsimulate.svg?branch=master)](https://travis-ci.org/andrew-edwards/EDMsimulate)
<!-- badges: end -->

Not operational yet.

### Vignette

(From eDNA:
The vignette showing examples of the functions is available [here](http://htmlpreview.github.io/?https://github.com/andrew-edwards/eDNAcutoff/blob/master/inst/doc/remove-false-positives.html), and directly in the package (see below).
)

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

Then try TODO:

```
vignette("remove-false-positives", package="eDNAcutoff")
```

which should show the vignette in an html browser.

It's hard for me to test a clean build (I think Travis ignores vignettes), so make an Issue on this repo to let me know if it's working. 
