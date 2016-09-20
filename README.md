dlstats: R package download stats
---------

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dlstats?color=green)](https://cran.r-project.org/package=dlstats)
![](https://cranlogs.r-pkg.org/badges/grand-total/dlstats?color=green)
![](https://cranlogs.r-pkg.org/badges/dlstats?color=green)
![](https://cranlogs.r-pkg.org/badges/last-week/dlstats?color=green)
[![gitter](https://img.shields.io/badge/GITTER-join%20chat-green.svg)](https://gitter.im/GuangchuangYu/Bioinformatics)


## Authors ##

Guangchuang YU <https://guangchuangyu.github.io>

School of Public Health, The University of Hong Kong 

## Installation ##

Get the released version from CRAN:

```r
install.packages("dlstats")
```

Or the development version from github:

```r
## install.packages("devtools")
devtools::install_github("GuangchuangYu/dlstats")
```

## Examples ##

```r
library(dlstats)
cran_stats(c("dlstats", "emojifont", "rvcheck"))

bioc_stats(c("ChIPseeker", "clusterProfiler", "DOSE", "ggtree", "GOSemSim", "ReactomePA"))
```
