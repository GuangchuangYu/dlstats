##' monthly download stats of Bioconductor software package(s)
##'
##'
##' @title bioc_stats
##' @param packages packages
##' @param type one of "Software", "AnnotationData", "ExperimentData", and "Workflow"
##' @param use_cache logical, should cached data be used? Default: TRUE. If set to FALSE, it will
##'   re-query download stats and update cache.
##' @return data.frame
##' @export
##' @examples
##' \dontrun{
##' library("dlstats")
##' pkgs <- c("ChIPseeker", "clusterProfiler", "DOSE", "ggtree", "GOSemSim", "ReactomePA")
##' y <- bioc_stats(pkgs, use_cache=TRUE)
##' head(y)
##' }
##' @author Guangchuang Yu
bioc_stats <- function(packages, use_cache=TRUE, type = "Software") {
    stats_cache <- get_from_cache(packages)
    if (use_cache) {
        packages <- packages[!packages %in% stats_cache$package]
    }
    if (length(packages) == 0) {
        return(stats_cache)
    }

    stats <- lapply(packages, bioc_stats2, type = type) %>% do.call('rbind', .)
    if (is.null(stats))
        return(NULL)

    res <- setup_stats(stats, packages)
    dlstats_cache(res)

    if(use_cache) return(rbind(res, stats_cache))
    return(res)
}

##' @importFrom utils read.table
bioc_stats2 <- function(pkg, type = "Software") {
    type <- match.arg(type, c("Software", "AnnotationData", "ExperimentData", "Workflow"))
    url <- switch(type,
                  Software = "https://bioconductor.org/packages/stats/bioc/",
                  AnnotationData = "https://bioconductor.org/packages/stats/data-annotation/",
                  ExperimentData = "https://bioconductor.org/packages/stats/data-experiment/",
                  Workflow = "http://bioconductor.org/packages/stats/workflows/"
    )
    url <- paste0(url, pkg, "/", pkg, "_stats.tab", collapse='')
    x <- tryCatch(read.table(url, header=TRUE, stringsAsFactors = FALSE), error=function(e) NULL)
    if (is.null(x)) {
        warning(paste("--> OMITTED:", pkg, "download stats not found or currently not available..."))
        return(NULL)
    }

    x <- x[x$Month != 'all',]
    start <- paste(x$Year, month2num(x$Month), '01', sep='-') %>% as.Date
    x$start <- start
    x <- x[order(x$start),]
    x <- x[x$Nb_of_downloads > 0,]
    x$end <- c(x$start[-1] - 1 , as.Date(format(Sys.time(), "%Y-%m-%d")))
    x$package <- pkg
    x <- x[, c("start", "end", "Nb_of_distinct_IPs", "Nb_of_downloads", "package")]
    return(x)
}

month2num <- function(x) {
    c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)[tolower(x)]
}

