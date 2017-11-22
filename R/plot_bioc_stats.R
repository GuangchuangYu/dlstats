YGC_cran_pkg <- c("badger", "dlstats", "emojifont", "ggimage", "meme","rvcheck", "scatterpie", "shadowtext")
YGC_bioc_pkg <- c("ChIPseeker", "clusterProfiler", "DOSE", "ggtree", "GOSemSim", "meshes", "ReactomePA", "seqcombo", "treeio")

##' plot bioconductor download stats
##'
##'
##' @title plot_bioc_stats
##' @param pkg packages
##' @return ggplot object
##' @importFrom RColorBrewer brewer.pal
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 geom_line
##' @importFrom ggplot2 geom_point
##' @importFrom ggplot2 scale_color_manual
##' @importFrom ggplot2 labs
##' @importFrom ggplot2 xlab
##' @importFrom ggplot2 ylab
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 theme_minimal
##' @importFrom scales comma
##' @importFrom utils read.delim
##' @export
##' @author guangchuang yu
plot_bioc_stats <- function(pkg=YGC_bioc_pkg) {
    nb <- bioc_stats(pkg)
    nb <- nb[-grep(max(nb$start), nb$start),]

    x=read.delim("https://bioconductor.org/packages/stats/bioc/bioc_pkg_scores.tab")
    x[order(x[,2], decreasing=T),] -> x
    n <- toupper(x[,1]) %>% unique %>% length

    rank = rep(NA, length(pkg))
    for (i in seq_along(pkg)) {
	j <- which(x[,1] == pkg[i])
	rank[i] = j # paste0("rank: ", j, '/', n)
    }

    cols <- brewer.pal(length(pkg), 'Set1')

    p <- plot_dlstats_internal(nb, "Nb_of_distinct_IPs")

    ii <- order(rank, decreasing=F)
    p <- p + scale_color_manual(breaks=pkg[ii],
                                labels=paste0(pkg, ": ", rank)[ii],
                                name=paste("Download rank in", n, "Bioconductor packages"),
                                values=cols)

    p <- p + labs(captions=paste0("Downloads by distinct IPs: ",
                                  scales::comma(sum(nb$Nb_of_distinct_IPs)), "/total, ",
                                  "access date: ", format(Sys.time(), "%b %Y")),
                  title="Monthly download stats")
    return(p)
}

plot_dlstats_internal <- function(nb, y) {
    ggplot(nb, aes_string(x="end", y=y,
                          group="package", color="package")) +
        geom_line() + geom_point() + theme_minimal() +
        xlab(NULL) + ylab(NULL) + theme(legend.position=c(.3, .6))
}

##' plot cran download stats
##'
##'
##' @title plot_cran_stats
##' @param pkg packages
##' @return ggplot object
##' @export
##' @author guangchuang yu
plot_cran_stats <- function(pkg=YGC_cran_pkg) {
    nb <- cran_stats(pkg)
    p <- plot_dlstats_internal(nb, "downloads")
    p + labs(captions=paste0("access date: ", format(Sys.time(), "%b %Y")),
             title="Monthly download stats") +
        scale_color_manual(values=brewer.pal(length(pkg), 'Dark2'), name="") +
        theme(legend.position=c(.1, .7))
}

