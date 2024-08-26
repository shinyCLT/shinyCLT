
# Installation

You may need to install Shiny package before installation shinyCLT.

``` r
install.packages("shiny")
```

Then proceed to installation of shinyCLT package from CRAN:

```r
install.packages("shinyCLT")
```

or GitHub

```r
devtools::install_github("shinyCLT/shinyCLT", build_vignettes = TRUE)
```

Then you could load the library and run the application.

```r
library(shinyCLT)
CLT()
```
For more detailed information, see `vignette("shinyCLT")`.
