##' plot table
##'
##'
##' @title ggtable
##' @param d data frame
##' @param p ggplot object to extract color to color rownames(d), optional
##' @importFrom rlang check_installed
##' @return ggplot object
##' @export
##' @author guangchuang yu
ggtable <- function(d, p = NULL) {
    # has_package("ggplotify")
    check_installed('ggplotify', 'for `ggtable()`.')
    ggplotify::as.ggplot(tableGrob2(d, p))
}

##' @importFrom grid gpar
##' @importFrom ggplot2 ggplot_build
##' @importFrom rlang check_installed
tableGrob2 <- function(d, p = NULL, core_cex=1, col_cex=1.5, row_cex=1.5) {
    # has_package("gridExtra")
    d <- d[order(rownames(d)),]
    check_installed('gridExtra', 'for `tableGrob2()`.')
    mytheme <- gridExtra::ttheme_default(core = list(fg_params = list(cex = core_cex)), 
    colhead = list(fg_params = list(cex = col_cex)), rowhead = list(fg_params = list(cex = row_cex)))
    tp <- gridExtra::tableGrob(d, theme = mytheme)
    if (is.null(p)) {
        return(tp)
    }

    # Fix bug: The 'group' order of lines and dots/path is different
    p_data <- ggplot_build(p)$data[[1]]
    # pcol <- unique(ggplot_build(p)$data[[1]][["colour"]])
    p_data <- p_data[order(p_data[["group"]]), ]
    pcol <- unique(p_data[["colour"]])
    ## This is fine too
    ## pcol <- unique(p_data[["colour"]])[unique(p_data[["group"]])]  
    j <- which(tp$layout$name == "rowhead-fg")

    for (i in seq_along(pcol)) {
        tp$grobs[j][[i+1]][["gp"]] <- gpar(col = pcol[i])
    }
    return(tp)
}

