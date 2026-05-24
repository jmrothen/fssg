
# fssg: Flexsurv "Shotgun" Approach

<!-- badges: start -->
<!-- badges: end -->

This package is designed to provide a simple, one-line-of-code approach to testing a variety of parametric survival distributions on a data set. 
This is done by running 50+ different parametric survival curves in order to see which one may be best suited to describing your data! 

## Installation

You can install the package through CRAN using:

``` r
install.packages('fssg')
```

You can install the development version of `fssg` like so:

``` r
devtools::install_github('jmrothen/fssg')
```

## Example

The package is designed to be mostly contained to one function, `fssg`. Simply provide a survival formula, and receive a table of each model run, and how it fits:

``` r
library(fssg)

# sample dataset available in <survival>
library(survival)
fssg(Surv(time, status)~1, data=aml, dump_models = TRUE)

```

For a more in-depth example, please refer to the vignette titled `fssg`.
