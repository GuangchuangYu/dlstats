.onLoad <- function(libname, pkgname) {
    .initial()
}

.initial <- function() {
    assign(".dlstats", new.env(), envir=.GlobalEnv)
}


