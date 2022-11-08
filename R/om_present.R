#' Print latex graph
#'
#' @export
#'
om_print_grp <- function(mat_g, weights,
                           fname   = "tmp.org",
                           pre_org = "#+LATEX_HEADER: \\usepackage{tikz} \n\n",
                           pre     = "\\begin{figure}\\resizebox{0.8\\textwidth}{!}{%\n",
                           post    = "\n}\n\\end{figure} \n") {

    graph <- matrix2graph(mat_g)
    graph <- setWeights(graph, weights)
    ss    <- graph2latex(graph, showAlpha = FALSE, fontsize = "huge")
    ss    <- paste(pre_org, pre, ss, post, sep = "")

    ## print out
    fileConn <- file(fname)
    writeLines(ss, fileConn)
    close(fileConn)
}
