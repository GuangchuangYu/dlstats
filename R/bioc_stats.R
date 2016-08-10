##' monthly download stats of cran package(s)
##'
##' 
##' @title bioc_stats
##' @param packages packages
##' @return data.frame
##' @export
##' @examples
##' \dontrun{
##' library("dlstats")
##' pkgs <- c("ChIPseeker", "clusterProfiler", "DOSE", "ggtree", "GOSemSim", "ReactomePA")
##' y <- bioc_stats(pkgs)
##' head(y)
##' }
##' @author Guangchuang Yu
bioc_stats <- function(packages) {
    stats_cache <- get_from_cache(packages)
    packages <- packages[!packages %in% stats_cache$package]
    
    if (length(packages) == 0) {
        return(stats_cache)
    }
    
    stats <- lapply(packages, bioc_stats2) %>% do.call('rbind', .)
    res <- setup_stats(stats, packages)
    dlstats_cache(res)

    rbind(res, stats_cache)
}

##' @importFrom utils read.table
bioc_stats2 <- function(pkg) {
    url <- paste0("https://bioconductor.org/packages/stats/bioc/", pkg, "/", pkg, "_stats.tab", collapse='')
    x <- tryCatch(read.table(url, header=TRUE, stringsAsFactors = FALSE), error=function(e) NULL)
    if (is.null(x)) {
        ## warning(paste("--> OMITTED:", pkg, "is not published in Bioconductor..."))
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

