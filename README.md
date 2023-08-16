# EDMsimulate - an R package for simulating fish populations in the context of Empirical Dynamic Modelling

<!-- badges: start -->
[![R-CMD-check](https://github.com/andrew-edwards/EDMsimulate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andrew-edwards/EDMsimulate/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/andrew-edwards/EDMsimulate/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andrew-edwards/EDMsimulate?branch=main)
<!-- badges: end -->

Simulating populations for some EDM analyses. Under development, and not really designed to be used by others. 

report/ contains earlier analyses that have now been included in first manuscript, and code likely better as vignettes in pbsEDM. 

We use the R package `cmdstanr` which uses `cmdstan` which needs Rtools4.3 (if running R 4.3). Install Rtools. Do `library(cmdstanr)` and follow the instructions for getting `cmdstan` and setting it up. 

### Vignette

The vignette showing examples of the functions is available [here](http://htmlpreview.github.io/?https://github.com/andrew-edwards/EDMsimulate/blob/master/doc/salmon_sim.html), and directly in the package (see below).

### To install from GitHub (Carrie and Luke)

I gave you access to push to this repo. So no need to fork on GitHub, just clone the repository onto your computer:
```
git clone https://github.com/andrew-edwards/EDMsimulate
```
then in R in the `EDMsimulate/` directory:
```
install(build_vignettes = TRUE)
```

Then try:

```
vignette("salmon_sim", package="EDMsimulate")
```

which should show the vignette in an html browser.

It's hard for me to test a clean build, so let me know if it's working. 

### To install from GitHub (not Carrie and Luke)

You need the package `devtools`, so you need to install it once:
```
install.packages("devtools")
```

This enables installation directly from the GitHub site:

```
devtools::install_github("andrew-edwards/EDMsimulate", build_vignettes = TRUE)
library(EDMsimulate)
```


Also, this packages requires the Larkin package, which is available here:
```
devtools::install_github("pbs-assess/larkin", build_vignettes = TRUE)
library(larkin)
```
