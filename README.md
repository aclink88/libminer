
<!-- README.md is generated from README.Rmd. Please edit that file -->

# libminer

<!-- badges: start -->

[![R-CMD-check](https://github.com/aclink88/libminer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aclink88/libminer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of libminer is to provide an overview of your R library setup.
It is a toy package as part of a workshop and not meant for serious use.

## Installation

You can install the development version of libminer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aclink88/libminer")
```

## Example

To get a count of installed packages in each of your libraries,
optionally with the toal sizes, use `lib_summary()`.

``` r
library(libminer)

lib_summary()
#>                                                                library
#> 1                                   C:/Program Files/R/R-4.3.1/library
#> 2                       C:/Users/aclin/AppData/Local/R/win-library/4.3
#> 3 C:/Users/aclin/AppData/Local/Temp/RtmpaMmBJV/temp_libpath83dc5ba65af
#>   n_packages
#> 1         30
#> 2        223
#> 3          1
# specify sizes = True
lib_summary(sizes=TRUE)
#>                                                                library
#> 1                                   C:/Program Files/R/R-4.3.1/library
#> 2                       C:/Users/aclin/AppData/Local/R/win-library/4.3
#> 3 C:/Users/aclin/AppData/Local/Temp/RtmpaMmBJV/temp_libpath83dc5ba65af
#>   n_packages  lib_size
#> 1         30  68858812
#> 2        223 490257466
#> 3          1     17317
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
