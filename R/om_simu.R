#' Simulate endpoint
#'
#' @param rep    number of replications
#' @param s_mu   simulation mean
#' @param s_sd   simulation SD (i.e. individual subject)
#' @param s_cor  correlation (i.e. individual subject)
#' @param s_size sample size
#'
#' @return Matrix of simulated test statistics
#'
#' @export
#'
#'
om_simu <- function(rep, s_mu, s_sd, s_cor, s_size = 1, seed = NULL) {

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    sigma <- get_covmat(s_sd / sqrt(s_size), s_cor)
    rst   <- rmvnorm(rep, mean = s_mu, sigma = sigma)

    if (!is.null(seed))
        old_seed <- set.seed(old_seed)

    rst
}

#' Get P-values
#'
#' @param stats   simulated test statistics
#' @param null_mu mu under NULL
#' @param s_sd    simulation SD
#' @param s_cor   correlation
#'
#'
#' @return p-values
#'
#' @export
#'
om_z_pval <- function(stats, null_mu = 0, null_sd = 1,
                      type = c("two-sided", "left", "right")) {

    type    <- match.arg(type)
    n_h     <- ncol(stats)
    null_mu <- rep(null_mu, length.out = n_h)
    null_sd <- rep(null_sd, length.out = n_h)

    rst <- NULL
    for (i in seq_len(n_h)) {
        cur_z <- (stats[, i] - null_mu[i]) / null_sd[i]
        cur_p <- switch(type,
                        "left"      = pnorm(cur_z),
                        "right"     = 1 - pnorm(cur_z),
                        "two-sided" = 2 * apply(cbind(pnorm(cur_z),
                                                      1 - pnorm(cur_z)),
                                                1, min))

        rst   <- cbind(rst, cur_p)
    }

    ## return
    rst
}

## ----------------------------------------------------------------------------
##
##              PRIVATE FUNCTIONS
##
## ----------------------------------------------------------------------------
get_covmat <- function(sd_cov, cor_cov) {
    n_x      <- length(sd_cov)
    vars     <- sd_cov^2
    cov_mat  <- matrix(NA, n_x, n_x)

    for (i in seq_len(n_x)) {
        cov_mat[i, i] <- vars[i]

        for (j in i:n_x) {
            if (j == i) {
                cov_mat[i, i] <- vars[i]
                next
            }
            cov_mat[i, j] <- cor_cov * sd_cov[i] * sd_cov[j]
            cov_mat[j, i] <- cov_mat[i, j]
        }
    }

    cov_mat
}
