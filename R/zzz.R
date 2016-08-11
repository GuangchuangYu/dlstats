.onLoad <- function(libname, pkgname) {
    .initial()
}

.initial <- function() {
    pos <- 1
    assign(".dlstats", new.env(), envir=as.environment(pos))
}


