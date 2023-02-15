

#' Get posterior distribution
#'
#'
#' @export
#'
si_bayes_beta_post <- function(x = seq(0, 1, by = 0.01),
                               obs_y = 1, obs_n = 10,
                               pri_ab = c(1, 1)) {
    pos_a <- pri_ab[1] + obs_y
    pos_b <- pri_ab[2] + obs_n - obs_y

    rst   <- sapply(x, function(v) {
        c(dbeta(v, pos_a, pos_b),
          1 - pbeta(v, pos_a, pos_b))
    })

    list(pri_ab  = pri_ab,
         pos_ab  = c(pos_a, pos_b),
         density = data.frame(x  = x,
                              y  = rst[1, ],
                              px = rst[2, ]))
}

#' Elicit prior
#'
#'
#' @export
#'
si_bayes_elicit <- function(x_range = c(0.8, 0.9),
                            x_prob  = 0.9,
                            ub      = 1000) {
    fp <- function(b) {
        a     <- b * mu / (1 - mu)
        cur_p <- pbeta(x_range[2], a, b) - pbeta(x_range[1], a, b)
        cur_p - x_prob
    }

    mu <- mean(x_range)
    b  <- uniroot(fp, c(0, ub))$root

    c(pri_a = b * mu / (1 - mu),
      pri_b = b)
}

## -----------------------------------------------------------------------------
##
## PRESENTATION FUNCTIONS
##
## -----------------------------------------------------------------------------

#' Plot distribution
#'
#'
#' @export
#'
si_bayes_plt_dist <- function(dat,
                              ver_x = NULL,
                              xlim  = c(0, 1),
                              group = NULL) {

    if (!is.null(group))
        dat$grp <- dat[[group]]

    rst <- ggplot(data = dat, aes(x = x, y = y)) +
        theme_bw() +
        xlim(xlim[1], xlim[2]) +
        labs(x = "Rate", y = "Density")

    if (is.null(group)) {
        rst <- rst + geom_line()
    } else {
        rst <- rst +
            geom_line(aes(group = grp, col = grp))
    }

    if (!is.null(ver_x) &
        is.null(group)) {
        dsub   <- dat %>% dplyr::filter(x >= ver_x)
        min_x  <- dsub[1, "x"]
        min_px <- dsub[1, "px"]
        max_x  <- max(dsub$x)
        max_y  <- max(dsub$y)

        x_txt  <- paste("P(R >=",
                        sprintf("%5.1f%%) =", 100 * min_x),
                        sprintf("%5.2f", min_px))

        dsub   <- dsub %>%
            rbind(data.frame(x  = c(max_x, min_x),
                             y  = c(0,     0),
                             px = c(NA,    NA)))

        rst <- rst +
            geom_vline(xintercept = ver_x, lty = 2, col = "brown") +
            geom_polygon(data = dsub, aes(x = x, y = y), fill = "gray60") +
            geom_label(x = min_x,
                       y = max(dat$y) * 0.5,
                       label = x_txt)
    }

    rst
}

## -----------------------------------------------------------------------------
##
## CALL STAN
##
## -----------------------------------------------------------------------------

#' Bayesian hierarchical modeling
#'
#'
#' @export
#'
si_bayes_hierarchy <- function(vec_y,
                               vec_n,
                               ...,
                               pri_sig   = 2.5,
                               adjust    = 1.5,
                               by_study  = FALSE,
                               pri_ab    = c(0.5, 0,5)) {

    ## stan sampling
    lst_data <- list(ns      = length(vec_y),
                     y       = array(vec_y),
                     n       = array(vec_n),
                     pri_sig = pri_sig)

    rst <- si_stan(lst_data,
                   stan_mdl = "hier",
                   ...)

    theta <- rstan::extract(rst, pars = "theta")$theta

    ## create result data frame
    rst   <- NULL
    for (i in seq_len(length(vec_y))) {
        cur_d <- density(theta[, i], adjust = adjust)
        rst   <- rbind(rst,
                       data.frame(x     = cur_d$x,
                                  y     = cur_d$y,
                                  grp   = "Hierarchical",
                                  study = i))

        if (by_study) {
            cur_d <- si_bayes_beta_post(obs_y  = vec_y[i],
                                        obs_n  = vec_n[i],
                                        pri_ab = pri_ab)$density

            rst   <- rbind(rst,
                           data.frame(x     = cur_d$x,
                                      y     = cur_d$y,
                                      grp   = "Study",
                                      study = i))
        }
    }

    ## return
    rst
}
