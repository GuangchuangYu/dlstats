dlstats: R package download stats
---------

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/dlstats?color=green)](https://cran.r-project.org/package=dlstats)
![](https://cranlogs.r-pkg.org/badges/grand-total/dlstats?color=green)
![](https://cranlogs.r-pkg.org/badges/dlstats?color=green)
![](https://cranlogs.r-pkg.org/badges/last-week/dlstats?color=green)
[![saythanks](https://img.shields.io/badge/say-thanks-ff69b4.svg)](https://saythanks.io/to/GuangchuangYu)
[![](https://img.shields.io/badge/follow%20me%20on-WeChat-green.svg)](https://guangchuangyu.github.io/blog_images/biobabble.jpg)



## :writing_hand: Authors ##

Guangchuang YU <https://guangchuangyu.github.io>

School of Basic Medical Sciences, Southern Medical University


## :arrow_double_down: Installation ##

Get the released version from CRAN:

```r
install.packages("dlstats")
```

Or the development version from github:

```r
## install.packages("devtools")
devtools::install_github("GuangchuangYu/dlstats")
```

## :book: Examples ##

```r
library(dlstats)
cran_stats(c("dlstats", "emojifont", "rvcheck"))

bioc_stats(c("ChIPseeker", "clusterProfiler", "DOSE", "ggtree", "GOSemSim", "ReactomePA"))
```
