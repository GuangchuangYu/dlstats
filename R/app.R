##' run shiny App for CRAN packages
##'
##' 
##' @title cranApp
##' @return NULL
##' @export
##' @author Guangchuang Yu
cranApp <- function() {
    dlstats_runApp("cranApp")
}

##' run shiny App for BioC packages
##'
##' 
##' @title biocApp
##' @return NULL
##' @export
##' @author Guangchuang Yu 
biocApp <- function() {
    dlstats_runApp("biocApp")
}


dlstats_runApp <- function(app) {
    dir <- system.file(app, package="dlstats")
    options(shiny.launch.browser = TRUE)
    runApp <- eval(parse(text="shiny::runApp"))
    runApp(dir)
}


