## -----------------------------------------------------------------------------
##
##                  Minimum Detectable Difference
##
## -----------------------------------------------------------------------------


#'  Power sample size calculation
#'
#'
#'
#' @export
#'
si_mdd_samplesize <- function(delta, sigma, power, alpha = 0.05, ratio = 1) {

    z_a <- qnorm(1 - alpha / 2)
    z_b <- pnorm(power)

    n0  <- 1 + 1 / ratio
    n0  <- n0 * sigma^2 / delta^2
    n0  <- n0 * (z_a + z_b)^2
    n0  <- ceiling(n0)

    c(n0, ratio * n0, (1 + ratio) * n0)
}

#'  Minimum detectable difference
#'
#'
#'
#' @export
#'
si_mdd_mdd <- function(n0, n1, sigma, alpha = 0.05) {

    z_a <- qnorm(1 - alpha / 2)
    mdd <- z_a * sigma * sqrt(1 / n0 + 1 / n1)

    mdd
}
