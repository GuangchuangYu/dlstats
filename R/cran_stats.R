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
    res <- lapply(packages, .fun) %>% do.call('rbind', .)
    res$package <- factor(res$package, levels=packages)
    res$start %<>% as.Date
    res$end %<>% as.Date
    return(res)
}

##' @importFrom jsonlite fromJSON
cran_stats2 <- function(pkg) {
    year <- get_start_year(pkg)
    if (is.null(year)) {
        warning(paste("--> OMITTED:", pkg, "is not published in CRAN..."))
        return(NULL)
    }
        
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
        
        return(fromJSON(suppressWarnings(readLines(url))))
    }) %>% do.call('rbind', .)

    res$downloads %<>% as.numeric
    res <- res[res$downloads != 0,]
    return(res)
}

get_start_year <- function(pkg) {
    start_year <- 2012
    end_year <- format(Sys.time(), "%Y") %>% as.numeric
    for (year in start_year:end_year) {
        url <- paste0("http://cranlogs.r-pkg.org/downloads/total/",
                      year, "-01-01:", year, "-12-31/", pkg)
        d <- fromJSON(suppressWarnings(readLines(url)))
        if (d$downloads > 0) {
            return(year)
        }
    }
}


get_start_year2 <- function(pkg) {
    left <- 2012
    right <- format(Sys.time(), "%Y") %>% as.numeric 
    m = ceiling((right-left)/2)
    
    while(m > 0) {
        years <- seq(left, right)
    	year = years[m]
        
        url <- paste0("http://cranlogs.r-pkg.org/downloads/total/",
                      year, "-01-01:", year, "-12-31/", pkg)
        d <- suppressWarnings(fromJSON(readLines(url)))
        
        if (d$downloads == 0) {
            left = year + 1
        } else {
            right = year
        }
        m = right - left			
    }
    return(right)    
}


##' not used, use jsonlite::fromJSON instead
parseJSON <- function(x) {
    xx <- unlist(strsplit(x, ',')) %>% strsplit(., ':') %>% do.call('cbind', .)
    yy <- apply(xx, 2, function(x) gsub("^[^0-9A-Za-z]+([0-9A-Za-z\\-]+)[^0-9A-Za-z]+$", '\\1', x))
    
    colnames(yy) = yy[1,]
    yy = as.data.frame(yy, stringsAsFactors=FALSE)
    yy <- yy[-1,, drop=FALSE]

    return(yy)
}


globalVariables(".")
