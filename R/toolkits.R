#' Assign text to numeric vector
#'
#'
#' @export
#'
tkt_assign <- function(txt, prefix = "c(", suffix = ")") {

    rst <- NULL
    txt <- paste("rst <-", prefix, txt, suffix)

    tryCatch({
        eval(parse(text = txt))
    }, error = function(e) {
    })

    rst
}


#' Determine limits
#'
#'
#' @export
#'
tkt_lim <- function(vec, mar_p = 0.05) {
    y_range <- range(vec, na.rm = TRUE)
    y_mar   <- mar_p * (y_range[2] - y_range[1])
    y_lim   <- y_range + c(-y_mar, y_mar)

    y_lim
}
