get_from_cache <- function(packages) {
    .dlstats <- get(".dlstats", envir=.GlobalEnv)
    stats <- lapply(packages, function(pkg) tryCatch(get(pkg, envir=.dlstats), error=function(e) NULL))
    names(stats) <- packages
    res <- do.call('rbind', stats[!sapply(stats, is.null)])
    rownames(res) <- NULL
    return(res)
}


dlstats_cache <- function(stats) {
    new_query <- split(stats, stats$package)
    .dlstats <- get(".dlstats", envir=.GlobalEnv)
    for (i in seq_along(new_query)) {
        assign(names(new_query)[i], new_query[[i]], envir=.dlstats)
    }
}
