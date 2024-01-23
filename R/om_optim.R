#' Compute rejection target function
#'
#' @param theta parameters in weights and G
#' @param weights vector of the original weights
#' @param mat_g transition matrix G
#' @param p_values vector of p-values for the elementary hypotheses
#' @param alpha overall alpha level
#' @param utility rejection utility of each elementary hypothesis
#' @param null_h null hypothesis index
#'
#' @return vector of the following values: 1. rejection utility, by default,
#'     this is the average number of rejected hypothesis 2. reject any null
#'     hypothesis rate, i.e., familywise false discovery rate 3. average
#'     rejection rate for each hypothesis
#'
#' @export
#'
om_rejection <- function(weights, mat_g, p_values, alpha = 0.05,
                         utility = 1,
                         null_h = NULL) {

    ## apply algorithm
    rej <- c_mtp(p_values,
                 alphas = alpha * as.numeric(weights),
                 mat_g  = mat_g)

    ## rejection rate separately
    rej_rates <- apply(rej, 2, mean)

    ## rejection any null
    if (0 == length(null_h)) {
        rej_any <- 0
    } else {
        rej_any <- apply(rej[, null_h, drop = FALSE],
                         1, sum)
        rej_any <- mean(rej_any > 0)
    }

    ## rejection utility
    inx_h <- seq_len(length(weights))
    if (0 < length(null_h))
        inx_h <- inx_h[-null_h]

    if (0 == length(inx_h)) {
        rej_uti <- 0
    } else {
        utility <- rep(utility, length.out = length(inx_h))
        rej_uti <- sum(utility * rej_rates[inx_h])
    }

    ## return
    c(rej_uti     = rej_uti,
      rej_anynull = rej_any,
      rej_rates)
}


#' Fill parameters to the G and weights
#'
#' @inheritParams om_rejection
#' @param uncons_theta whether theta are unconstrained. If TRUE, convert using
#'     exponential function. Example: (row tot - row sum fixed values)
#'     [e^a/(1+e^a+e^b) e^b/(1+e^a+e^b)]
#'
#' @export
#'
om_theta_fill <- function(theta = NULL, weights, mat_g,
                          uncons_theta = TRUE, tot = 1) {
    fill_vec <- function(vec, ta) {
        if (is.null(ta))
            return(vec)

        inx_na <- which(is.na(vec))
        n_na   <- length(inx_na)

        if (1 == n_na) {
            vec[inx_na] <- tot - sum(vec[-inx_na])
        } else if (1 < n_na) {
            i_ta      <- seq_len(n_na - 1)
            cur_theta <- ta[i_ta]

            if (uncons_theta) {
                ## exponential convert
                cur_theta <- exp(cur_theta)
                cur_theta <- cur_theta / (1 + sum(cur_theta))
                cur_theta <- (1 - sum(vec, na.rm = TRUE)) * cur_theta
            }

            vec[inx_na[-1]] <- cur_theta
            vec[inx_na[1]]  <- tot - sum(vec[-inx_na[1]])
            ta              <- ta[-i_ta]
        }

        list(vec, ta)
    }

    n_h <- length(weights)
    stopifnot(n_h == ncol(mat_g))

    ## fill in weights
    fw       <- fill_vec(weights, theta)
    weights  <- fw[[1]]
    theta    <- fw[[2]]

    ## fill G
    rst_g <- NULL
    for (i in seq_len(nrow(mat_g))) {
        fw    <- fill_vec(mat_g[i, ], theta)
        rst_g <- rbind(rst_g, fw[[1]])
        theta <- fw[[2]]
    }

    ## return
    list(weights = weights,
         mat_g   = rst_g)
}

#' Fill parameter then calculate rejection
#'
#' @inheritParams om_rejection
#' @inheritParams om_theta_fill
#'
#' @export
#'
om_theta_rejection <- function(theta, weights, mat_g,
                               uncons_theta = TRUE, ...) {
    fill_theta <- om_theta_fill(theta, weights, mat_g,
                                uncons_theta = uncons_theta)
    om_rejection(fill_theta$weights, fill_theta$mat_g, ...)
}


#' Get optim constraints
#'
#' @inheritParams om_rejection
#'
#' @export
#'
om_theta_constraints <- function(mat, tot = 1) {
    n_theta <- NULL
    row_sum <- NULL
    for (i in seq_len(nrow(mat))) {
        vec     <- mat[i, ]
        inx_na  <- which(is.na(vec))
        n_na    <- length(inx_na)
        if (n_na < 2)
            next

        n_theta <- c(n_theta, n_na - 1)
        row_sum <- c(row_sum, sum(vec, na.rm = TRUE))
    }

    if (is.null(n_theta)) {
        return(list(tot_theta = 0))
    }

    tot_theta <- sum(n_theta)
    tot_rows  <- length(n_theta)

    ## all parameters >= 0
    rst_ci_0 <- rep(0, tot_theta)
    rst_ui_0 <- diag(tot_theta)

    ## all sum < 1
    rst_ci_1   <- tot - row_sum
    rst_ui_1   <- NULL
    init_theta <- rep(0, tot_theta)
    for (i in seq_len(tot_rows)) {
        if (1 == i) {
            inx_1 <- 1
        } else {
            inx_1 <- 1 + sum(n_theta[1:(i-1)])
        }
        inx_2 <- inx_1 + n_theta[i] - 1

        c_inx          <- inx_1:inx_2
        c_ui           <- rep(0, tot_theta)
        c_ui[c_inx]    <- 1

        ## set init theta
        init_theta[c_inx] <- rst_ci_1[i] / (1 + length(c_inx))

        ## append
        rst_ui_1 <- rbind(rst_ui_1, c_ui)
    }

    ## constraints
    ui <- rbind(rst_ui_0, - rst_ui_1)
    ci <- c(rst_ci_0, - rst_ci_1)

    ## return
    return(list(
        ui         = ui,
        ci         = ci,
        init_theta = init_theta,
        tot_theta  = tot_theta
    ))
}

#' Optimize weights and G
#'
#' @inheritParams om_rejection
#' @inheritParams om_theta_fill
#'
#' @param par_optim options for constrOptim function
#' @param tot restrict on the sum of parameters
#' @param init_theta initial values for theta. Default NULL, which will use
#'     00000.1 fro all theta as initial values.
#'
#'
#' @details Parameters in weights and mat_g should be entered as NA. The
#'     constraints require all the parameter to be non-negative and each row in
#'     mat_g and weights have total 1.
#'
#'     Note that the first NA in each row of mat_g or weights are fixed values
#'     given the other elements in the row in order to satisfy the constraint
#'     that the sum is 1.
#'
#'
#' @export
#'
om_rejection_optim <- function(weights, mat_g, ...,
                               tot = 1,
                               init_theta = NULL,
                               uncons_theta = TRUE,
                               par_optim = list()) {

    ## calculate target function
    f_opt <- function(theta) {
        uti <- om_theta_rejection(theta, weights, mat_g,
                                  uncons_theta, ...)

        - uti[1]
    }

    ## constrained optimization
    f_cons <- function() {
        if (ui_ci$tot_theta > 1) {
            rst <- do.call(constrOptim, c(
                list(
                    f = f_opt,
                    grad = NULL,
                    theta = init_theta,
                    ui = ui_ci$ui,
                    ci = ui_ci$ci
                ),
                par_optim
            ))
        } else {
            rst <- do.call(optim, c(
                list(
                    par = init_theta,
                    fn = f_opt,
                    gr = NULL,
                    method = "Brent",
                    lower = 0,
                    upper = max(abs(ui_ci$ci))
                ),
                par_optim
            ))
        }
        rst
    }

    ## unconstrained optimization
    f_uncons <- function() {
        rst <- do.call(optim,
                       c(list(par = init_theta,
                              fn = f_opt,
                              gr = NULL),
                       par_optim))
        rst
    }

    ## ----------------- start here ----------------------------------

    ## get constraints, index and boundary
    ui_ci <- om_theta_constraints(rbind(weights, mat_g, tot = tot))

    ## initial values
    if (is.null(init_theta)) {
        if (uncons_theta) {
            tmp <- 0
        } else {
            tmp <- ui_ci$init_theta
        }
    } else {
        tmp <- init_theta
    }
    init_theta <- rep(tmp, length.out = ui_ci$tot_theta)

    ## optimize
    if (0 == ui_ci$tot_theta) {
        theta <- NULL
        value <- - f_opt(NULL)
    } else {
        if (uncons_theta) {
            rst <- f_uncons()
        } else {
            rst <- f_cons()
        }

        if (0 != rst$convergence) {
            theta <- NA
            value <- NA
        } else {
            theta <- rst$par
            value <- -rst$value
        }
    }

    ## return
    fill_theta <- om_theta_fill(theta, weights, mat_g,
                                uncons_theta = uncons_theta)
    list(theta   = theta,
         weights = fill_theta$weights,
         mat_g   = fill_theta$mat_g,
         rej_uti = value)
}
