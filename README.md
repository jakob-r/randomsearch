---
output:
  md_document:
    variant: markdown_github
---
# randomsearch

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/randomsearch)](https://cran.r-project.org/package=randomsearch)
[![Build Status](https://travis-ci.org/jakob-r/randomsearch.png?branch=master)](https://travis-ci.org/jakob-r/randomsearch)
[![Build status](https://ci.appveyor.com/api/projects/status/gvr607kqcl78qjq9/branch/master?svg=true)](https://ci.appveyor.com/project/jakob-r/randomsearch/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakob-r/randomsearch/badge.svg?branch=master)](https://coveralls.io/github/jakob-r/randomsearch?branch=master)
[![Monthly RStudio CRAN Downloads](https://cranlogs.r-pkg.org/badges/randomsearch)](https://CRAN.R-project.org/package=randomsearch)

## Random Search for Expensive Functions

 Simple Random Search function for the [smoof](https://cran.r-project.org/package=smoof) and [ParamHelpers](https://cran.r-project.org/package=ParamHelpers) ecosystem with termination criteria and parallelization.

* [Documentation](https://jakob-r.github.io/randomsearch/)
* [Issues, Requests and Bug Tracker](https://github.com/jakob-r/randomsearch/issues)



# Installation

We recommend to install the official release version:


```r
install.packages("randomsearch")
```

For experimental use you can install the latest development version:


```r
devtools::install_github("jakob-r/randomsearch")
```

# Usage


```r
library(randomsearch)
fun = function(x) x[1]^2 + sin(x[2])
res = randomsearch(fun, lower = c(-1, -2), upper = c(2,4), minimize = TRUE, max.evals = 30)
summary(res)
```

```
## Randomsearch Result: 
## Eval no.: 9
## x: x=-0.262,-1.75
## y: -0.9158972
```

For a more detailed documentation you can check the Vignette.
