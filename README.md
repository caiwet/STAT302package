  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/caiwet/STAT302package.svg?branch=master)](https://travis-ci.com/caiwet/STAT302package)
  [![Codecov test coverage](https://codecov.io/gh/caiwet/STAT302package/branch/master/graph/badge.svg)](https://codecov.io/gh/caiwet/STAT302package?branch=master)
  <!-- badges: end -->
  
## Installation

To download the STAT302package, use the code below.

``` r
# install.packages("devtools")
devtools::install_github("caiwet/STAT302package")
library(STAT302package)
```
  
## Use

The vignette demonstrates example usage of all main functions. Please [file an issue](https://github.com/bryandmartin/corncob/issues) if you have a request for a tutorial that is not currently included. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("caiwet/STAT302package", build_vignette = TRUE, build_opts = c())
library(STAT302package)
# Use this to view the vignette in the corncob HTML help
help(package = "STAT302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302package")
```
