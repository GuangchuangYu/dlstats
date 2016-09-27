##' @importFrom utils available.packages
dlstats <- function(packages) {
    cran_db <- available.packages(repos="http://cloud.r-project.org/")
    bioc_db <- available.packages(repos="https://bioconductor.org/packages/devel/bioc")
    cran_pkg <- cran_db[, "Package"]
    bioc_pkg <- bioc_db[, "Package"]
    pkgs <- packages[packages %in% c(cran_pkg, bioc_pkg)]
    msg <- paste0("--> Packages: '", paste(packages[! packages %in% pkgs], collapse='/'), "', not available in either CRAN or Bioconductor...")
    warning(msg)

    ds1 <- cran_stats(pkgs[pkgs %in% cran_pkg])
    ds2 <- bioc_stats(pkgs[pkgs %in% bioc_pkg])
    return(list(cran_stats=ds1,
                bioc_stats=ds2)
           )
}
