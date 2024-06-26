# Miscellaneous Functions for 'SciViews::R' <a href='https://www.sciviews.org/svMisc'><img src='man/figures/logo.png' align='right' height='139'/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/SciViews/svMisc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SciViews/svMisc/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/SciViews/svMisc/branch/main/graph/badge.svg)](https://codecov.io/gh/SciViews/svMisc?branch=main) [![CRAN status](https://www.r-pkg.org/badges/version/svMisc)](https://cran.r-project.org/package=svMisc) [![r-universe status](https://sciviews.r-universe.dev/badges/svMisc)](https://sciviews.r-universe.dev/svMisc) [![License](https://img.shields.io/badge/license-GPL-blue.svg)](https://www.gnu.org/licenses/gpl-2.0.html) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

{svMisc} provides a series of functions that are used by the other packages that make the SciViews::R dialect. You can also use most of them directly, like a progress bar, a function to run analyses in batch (and recover in case of error), management of a temporary environment, etc.

## Installation

{svMisc} is available from CRAN, but it is an old version. You should install it from the [SciViews R-Universe](https://sciviews.r-universe.dev). To install this package and its dependencies, run the following command in R:

```{r, eval=FALSE}
install.packages('svMisc', repos = c('https://sciviews.r-universe.dev',
  'https://cloud.r-project.org'))
```

An older version of {svMisc} can be installed from [CRAN](http://cran.r-project.org):

``` r
install.packages("svMisc")
```

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {svMisc} package from GitHub (source from **main** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/svMisc")
```

R should install all required dependencies automatically, and then it should compile and install {svMisc}.


## Further explore {svMisc}

You can get further help about this package this way: Make the {svMisc} package available in your R session:

``` r
library("svMisc")
```

Get help about this package:

``` r
library(help = "svMisc")
help("svMisc-package")
vignette("svMisc") # None is installed with install_github()
```

For further instructions, please, refer to the help pages at <https://www.sciviews.org/svMisc/>.

## Code of Conduct

Please note that the {svMisc} package is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Note to developers

This package used to be developed on R-Forge in the past. However, the latest [R-Forge version](https://r-forge.r-project.org/projects/sciviews/) was moved to this Github repository on 2016-03-16 (SVN version 569). **Please, do not use R-Forge anymore for SciViews development, use this Github repository instead.**
