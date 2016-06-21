##' monthly download stats of cran package(s)
##'
##' 
##' @title cran_stats
##' @param packages packages
##' @return data.frame
##' @importFrom magrittr %>%
##' @export
##' @author Guangchuang Yu
cran_stats <- function(packages) {
    package_stats(packages, cran_stats2)
}

##' @importFrom magrittr %<>%
package_stats <- function(packages, .fun) {
    res <- lapply(packages, .fun)
    res <- do.call('rbind', res)
    res$package <- factor(res$package, levels=packages)
    res$start %<>% as.Date
    res$end %<>% as.Date
    return(res)
}

##' @importFrom jsonlite fromJSON
cran_stats2 <- function(pkg) {
    year <- get_start_year(pkg)

    start <- as.Date(paste0(year, "-01-01"))
    end <- as.Date(format(Sys.time(), "%Y-%m-%d"))

    all_months <- seq(start, end, by = 'month')
    n <- length(all_months)
    
    res <- lapply(seq_along(all_months), function(i) {
        mstart <- all_months[i]
        if (i == n) {
            mend <- end
        } else {
            mend <- all_months[i+1]-1
        }
        
        url <- paste0("http://cranlogs.r-pkg.org/downloads/total/",
                      mstart, ":", mend, "/", pkg)
        
        return(suppressWarnings(fromJSON(readLines(url))))
    })
    res <- do.call('rbind', res)
    res <- res[res$downloads != 0,]
    return(res)
}

##' @importFrom jsonlite fromJSON
get_start_year <- function(pkg) {
    start_year <- 2012
    end_year <- format(Sys.time(), "%Y") %>% as.numeric
    for (year in start_year:end_year) {
        url <- paste0("http://cranlogs.r-pkg.org/downloads/total/",
                      year, "-01-01:", year, "-12-31/", pkg)
        d <- suppressWarnings(fromJSON(readLines(url)))
        if (d$downloads > 0) {
            return(year)
        }
    }
}

