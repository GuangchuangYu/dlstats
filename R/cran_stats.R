##' monthly download stats of cran package(s)
##'
##'
##' @title cran_stats
##' @param packages packages
##' @param use_cache logical, should cached data be used? Default: TRUE. If set to FALSE, it will re-query download stats and update cache.
##' @param progress logical, should a progress bar be shown? Default: TRUE. When TRUE, a text progress bar will display “fetching data for `year-month`” as each month’s range is fetched.
##' @return data.frame
##' @importFrom jsonlite fromJSON
##' @importFrom magrittr %>%
##' @export
cran_stats <- function(packages, use_cache=TRUE, progress=TRUE) {
    stats_cache <- get_from_cache(packages)
    if (use_cache) {
        packages <- packages[!packages %in% stats_cache$package]
    }
    if (length(packages) == 0) {
        return(stats_cache)
    }

    pkgs <- paste(packages, sep = ',', collapse = ',')
    year <- get_start_year(pkgs)
    if (is.null(year)) {
        warning(paste("--> OMITTED:", pkgs, "download stats not found or currently not available..."))
        return(NULL)
    }

    start <- as.Date(paste0(year, "-01-01"))
    end <- as.Date(format(Sys.time(), "%Y-%m-%d"))

    all_months <- seq(start, end, by = 'month')
    n <- length(all_months)

    urls <- sapply(seq_along(all_months), function(i) {
        mstart <- all_months[i]
        if (i == n) {
            mend <- end
        } else {
            mend <- all_months[i+1]-1
        }
        paste0("https://cranlogs.r-pkg.org/downloads/total/",
               mstart, ":", mend, "/", pkgs)
    })

    if (progress) {
        pb <- txtProgressBar(min = 0, max = n, style = 3)
        cat("\r\033[K") # clear the initial bar line
    }
    stats <- lapply(seq_along(urls), function(i) {
        mstart <- all_months[i]
        if (i == n) {
            mend <- end
        } else {
            mend <- all_months[i+1] - 1
        }

        if (progress) {
            # clear previous message+bar on iterations >1
            if (i > 1) {
                cat("\033[1A\r\033[K")  # clear old bar line
                cat("\033[1A\r\033[K")  # clear old message line
            }
            # update message with current month
            cat(sprintf("dlstats: fetching data for %s\n", format(mstart, "%Y-%m")))
            # render progress bar
            setTxtProgressBar(pb, i)
            cat("\n")
        }

        result <- tryCatch(
            fromJSON(suppressWarnings(readLines(urls[i]))),
            error = function(e) NULL
        )
        result
    }) %>% do.call('rbind', .)
    if (progress) {
        close(pb)
    }

    if (is.null(stats)) {
        warning(paste("--> OMITTED:", pkgs, "download stats not found or currently not available..."))
        return(NULL)
    }

    ## stats$downloads %<>% as.numeric
    stats <- stats[stats$downloads != 0,]
    res <- setup_stats(stats, packages)
    res <- res[order(res$package, res$start),]
    dlstats_cache(res)

    if (use_cache) return(rbind(res, stats_cache))
    return(res)
}


##' @importFrom magrittr %<>%
setup_stats <- function(stats, packages) {
    stats$package <- factor(stats$package, levels=packages)
    stats$start %<>% as.Date
    stats$end %<>% as.Date
    rownames(stats) <- NULL
    return(stats)
}

##' set query start year for `cran_stats`
##'
##'
##' @title set_cran_start_year
##' @param year start year
##' @return NULL
##' @export
##' @author Guangchuang Yu
set_cran_start_year <- function(year) {
    ## https://github.com/GuangchuangYu/dlstats/issues/4
    options(dlstats_cran_start_year = year)
    ## invisible(year)
}


get_start_year <- function(pkg) {
    # start_year <- 2012
    end_year <- format(Sys.time(), "%Y") %>% as.numeric
    start_year <- end_year - 5
    start_year <- getOption('dlstats_cran_start_year', default = start_year)



    for (year in start_year:end_year) {
        url <- paste0("https://cranlogs.r-pkg.org/downloads/total/",
                      year, "-01-01:", year, "-12-31/", pkg)
        d <- tryCatch(fromJSON(suppressWarnings(readLines(url))),
                      error = function(e) NULL)
        if (is.null(d))
            next
        if (any(d$downloads > 0)) {
            return(year)
        }
    }
    return(NULL)
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


## not used, use jsonlite::fromJSON instead
parseJSON <- function(x) {
    xx <- unlist(strsplit(x, ',')) %>% strsplit(., ':') %>% do.call('cbind', .)
    yy <- apply(xx, 2, function(x) gsub("^[^0-9A-Za-z]+([0-9A-Za-z\\-]+)[^0-9A-Za-z]+$", '\\1', x))

    colnames(yy) = yy[1,]
    yy = as.data.frame(yy, stringsAsFactors=FALSE)
    yy <- yy[-1,, drop=FALSE]

    return(yy)
}


globalVariables(".")
