## -----------------------------------------------------------------------------
##
##                  Desirability
##
## -----------------------------------------------------------------------------


#'  Simulate Data by Trial
#'
#' @export
#'
si_door_simu <- function(n, lst_outcomes, seed = NULL) {

    if (!is.null(seed)) {
        old_seed <- set.seed(seed)
    }

    com_bin <- NULL
    com_con <- NULL
    rst     <- data.frame(id  = seq(1,  2 * n),
                          arm = rep(c("Treatment", "Control"),
                                    each = n))

    for (i in seq_len(length(lst_outcomes))) {
        cur_outcome <- lst_outcomes[[i]]

        if ("Binary" == cur_outcome$type) {
            cur_trt <- rbinom(n,
                              size = 1,
                              prob = cur_outcome$mean_trt)

            cur_ctl <- rbinom(n,
                              size = 1,
                              prob = cur_outcome$mean_ctl)

            cur_val <- c(cur_trt, cur_ctl)

            if (is.null(com_bin)) {
                com_bin <- cur_val
            } else {
                com_bin <- apply(cbind(com_bin, cur_val),
                                 1,
                                 function(x) as.numeric(1 == any(x)))
            }
        } else {
            cur_trt <- rnorm(n,
                             cur_outcome$mean_trt,
                             cur_outcome$sd)
            cur_ctl <- rnorm(n,
                              cur_outcome$mean_ctl,
                              cur_outcome$sd)

            cur_val <- round(c(cur_trt, cur_ctl),
                             2)

            if (is.null(com_con)) {
                com_con <- cur_val
            } else {
                com_con <- apply(cbind(com_con, cur_val),
                                 1,
                                 function(x) min(x))
            }
        }

        rst[[cur_outcome$name]] <- cur_val
    }

    ## composite
    if (!is.null(com_bin)) {
        rst$Any_Binary <- com_bin
    }

    if (!is.null(com_con)) {
        rst$Min_Continous <- com_con
    }

    if (!is.null(seed)) {
        set.seed(old_seed)
    }

    ## return
    rst
}


#' DOOR Analysis
#'
#'
#' @export
#'
si_door_ana <- function(dta, lst_outcomes) {

    rst <- NULL
    for (i in seq_len(length(lst_outcomes))) {
        cur_outcome <- lst_outcomes[[i]]
        cur_dta     <- dta
        cur_dta$y   <- cur_dta[[cur_outcome$name]]

        if ("Continuous" == cur_outcome$type) {
            cur_rst <- si_ana_ttest(cur_dta)
        } else {
            cur_rst <- si_ana_fisher(cur_dta)
        }

        cur_rst <- cbind(Outcome = cur_outcome$name,
                         cur_rst)

        rst <- rbind(rst, cur_rst)
    }


    if (!is.null(dta$Any_Binary)) {
        cur_dta     <- dta
        cur_dta$y   <- cur_dta$Any_Binary
        cur_rst     <- si_ana_fisher(cur_dta)
        cur_rst     <- cbind(Outcome = "Any Binary", cur_rst)
        rst         <- rbind(rst, cur_rst)
    }

    if (!is.null(dta$Min_Continous)) {
        cur_dta     <- dta
        cur_dta$y   <- cur_dta$Min_Continous
        cur_rst     <- si_ana_ttest(cur_dta)
        cur_rst     <- cbind(Outcome = "Min of Continuous", cur_rst)
        rst         <- rbind(rst, cur_rst)
    }

    ## return
    rownames(rst) <- NULL
    rst
}


#' Rank subjects
#'
#'
#' @export
#'
si_door_rank_single <- function(dta_a, dta_b, lst_outcomes) {

    n_outcome <- length(lst_outcomes)
    rst       <- NULL
    last_comp <- "Tie"

    for (i in seq_len(n_outcome)) {
        cur_name  <- lst_outcomes[[i]]$name
        cur_delta <- lst_outcomes[[i]]$delta
        cur_a     <- dta_a[1, cur_name]
        cur_b     <- dta_b[1, cur_name]

        if (cur_a - cur_b > cur_delta) {
            cur_comp <- "Better"
        } else if (cur_a - cur_b < cur_delta) {
            cur_comp <- "Worse"
        } else {
            cur_comp <- "Tie"
        }

        if ("Tie" == last_comp) {
            last_comp <- cur_comp
        } else {
            cur_comp  <- "Unnecessary to Compared"
        }

        rst <- rbind(rst,
                     data.frame(Outcome    = cur_name,
                                Delta      = cur_delta,
                                Patient_A  = cur_a,
                                Patient_B  = cur_b,
                                Comparison = cur_comp))
    }

    list(data = rst,
         rank = last_comp)
}


#' DOOR Rank All
#'
#'
#' @export
#'
si_door_rank <- function(dta_trt, dta_ctl, lst_outcomes) {

    n_outcome   <- length(lst_outcomes)
    vec_outcome <- NULL
    for (i in seq_len(n_outcome)) {
        vec_outcome <- c(vec_outcome,
                         lst_outcomes[[i]]$name)
    }

    dta_ctl <- dta_ctl %>%
        arrange(across(all_of(vec_outcome)))

    rst <- NULL
    for (i in seq_len(nrow(dta_trt))) {
        dta_a   <- dta_trt[i, ]
        cur_rst <- 0
        for (j in nrow(dta_ctl):1) {
            dta_b <- dta_ctl[j, ]
            tmp   <- si_door_rank_single(dta_a, dta_b, lst_outcomes)$rank

            if ("Tie" == tmp) {
                cur_rst <- cur_rst + 0.5
            } else if ("Better" == tmp) {
                cur_rst <- cur_rst + 1
            } else {
                break
            }
        }

        rst <- c(rst, cur_rst)
    }

    list(data  = cbind(dta_trt, DOOR = rst),
         sum   = sum(rst),
         n_trt = nrow(dta_trt),
         n_ctl = nrow(dta_ctl))
}

#' DOOR Rank All with BS
#'
#'
#' @export
#'
si_door_rank_bs <- function(dta_trt, dta_ctl, lst_outcomes,
                            n_bs    = 100,
                            n_cores = 10,
                            seed    = NULL) {

    if (!is.null(seed)) {
        old_seed <- set.seed(seed)
    }

    rst <- parallel::mclapply(
                         1:n_bs,
                         function(x) {

                             print(x)
                             cur_trt <- dta_trt
                             cur_ctl <- dta_ctl

                             if (x > 1) {
                                 inx_trt <- sample(seq_len(nrow(cur_trt)),
                                                   replace = TRUE)
                                 inx_ctl <- sample(seq_len(nrow(cur_ctl)),
                                                   replace = TRUE)

                                 cur_trt <- cur_trt[inx_trt, ]
                                 cur_ctl <- cur_trt[inx_ctl, ]
                             }

                             cur_rst <- si_door_rank(cur_trt,
                                                     cur_ctl,
                                                     lst_outcomes)

                             c(inx   = x,
                               sum   = cur_rst$sum,
                               n_trt = cur_rst$n_trt,
                               n_ctl = cur_rst$n_ctl)
                         },
                         mc.cores = n_cores
                     )

    if (!is.null(seed)) {
        set.seed(old_seed)
    }

    rst <- t(simplify2array(rst))
    data.frame(rst)
}


#' Organize outcome list to table
#'
#'
#' @export
#'
si_door_outcomes_to_table <- function(lst_outcomes) {

    if (is.null(lst_outcomes))
        return(NULL)

    if (0 == length(lst_outcomes)) {
        return(NULL)
    }

    rst <- NULL
    for (i in seq_len(length(lst_outcomes))) {
        cur_o   <- lst_outcomes[[i]]
        cur_rst <- data.frame(Name           = cur_o$name,
                              Type           = cur_o$type,
                              Mean_Treatment = cur_o$mean_trt,
                              Mean_Control   = cur_o$mean_ctl,
                              SD             = cur_o$sd,
                              Delta          = cur_o$delta)

        if ("Binary" == cur_o$type) {
            cur_rst[1, "SD"]    <- NA
            cur_rst[1, "Delta"] <- NA
        }

        rst <- rbind(rst, cur_rst)
    }

    rst
}
