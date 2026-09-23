#' Run Web-Based application
#'
#' Call Shiny to run \code{statideas} as a web-based application.
#'
#' @details
#'
#' A web browser will be brought up for users to access the GUI
#'
#'
#' @export
#'
si_shiny <- function(appname = "boundary", pkgname = "statideas") {
    req.pkgs        <- c("shiny", "shinythemes", "DT",
                         "knitr", "rmarkdown", "pander");
    chk.uninstalled <- sapply(req.pkgs,
                              function(x) {
                                  !requireNamespace(x, quietly = TRUE)
                              })
    chk.inx         <- which(chk.uninstalled)

    if (0 < length(chk.inx)) {
        msg <- paste("For the GUI to work, please install ",
                     ifelse(1 < length(chk.inx), "packages ", "package "),
                     paste(req.pkgs[chk.inx], collapse = ", "),
                     " by \n install.packages(",
                     paste(paste("'",
                                 req.pkgs[chk.inx],
                                 "'",
                                 sep = ""), collapse = ", "),
                     ") \n  ",
                     sep = "");
        stop(msg, call. = FALSE);
    }

    appDir <- system.file(appname, package = "statideas")
    if (appDir == "") {
        stop("Could not find Shiny directory. Try re-installing `statideas`.",
             call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal");
}
