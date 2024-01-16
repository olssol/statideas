#' Simulate Enrollment Time
#'
#'
#' @export
#'
si_simu_enroll <- function(ntot, enroll_duration = 12, min_fu = NULL,
                           date_bos = NULL, mth_to_days = 30.4,
                           round_days = FALSE, ...) {

    rand_enroll <- runif(ntot, 0, enroll_duration)
    day_enroll  <- rand_enroll * mth_to_days

    if (round_days) {
        day_enroll <- floor(day_enroll)
    }

    rst <- data.frame(sid        = seq_len(ntot),
                      day_enroll = day_enroll)

    ## set up end of study time by minimum follow up
    if (!is.null(min_fu)) {
        day_eos     <- max(rand_enroll) + min_fu
        day_eos     <- day_eos * mth_to_days
        day_eos     <- floor(day_eos)
        rst$day_eos <- day_eos - day_enroll
    }


    ## set up dates in addition to days
    if (!is.null(date_bos)) {
        rst$date_bos    <- date_bos
        rst$date_eos    <- date_bos + day_eos
        rst$date_enroll <- date_bos + day_enroll
    }

    ## return
    rst
}

#' Simulate Continuous Outcome
#'
#'
#' @export
#'
si_simu_cont <- function(ntot, mean = 0, sd = 1, seed = NULL) {

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    rst <- data.frame(sid  = seq_len(ntot),
                      y    = rnorm(ntot, mean = mean, sd = sd))

    if (!is.null(seed))
        set.seed(old_seed)

    ## return
    rst
}

#' Simulate Binary Outcome
#'
#'
#' @export
#'
si_simu_binary <- function(ntot, mean = 0.5, seed = NULL) {

    if (!is.null(seed))
        old_seed <- set.seed(seed)

    rst <- data.frame(sid = seq_len(ntot),
                      y   = rbinom(ntot, size = 1, prob = mean))

    if (!is.null(seed))
        set.seed(old_seed)

    ## return
    rst
}

#' TTest
#'
#' @export
#'
si_ana_ttest <- function(dta) {
    d_trt <- dta %>% filter(arm == "Treatment")
    d_ctl <- dta %>% filter(arm == "Control")

    rst_test <- t.test(d_trt$y, d_ctl$y)

    data.frame(
        Test      = "T Test",
        Mean_Trt  = rst_test$estimate[1],
        Mean_Ctl  = rst_test$estimate[2],
        Conf_Low  = rst_test$conf.int[1],
        Conf_High = rst_test$conf.int[2],
        p_Value   = rst_test$p.value)
}

#' TTest
#'
#' @export
#'
si_ana_fisher <- function(dta) {
    d_trt <- dta %>% filter(arm == "Treatment")
    d_ctl <- dta %>% filter(arm == "Control")

    rst_test <- fisher.test(d_trt$y, d_ctl$y)

    data.frame(
        Test      = "Fisher Exact",
        Mean_Trt  = mean(d_trt$y),
        Mean_Ctl  = mean(d_ctl$y),
        Conf_Low  = rst_test$conf.int[1],
        Conf_High = rst_test$conf.int[2],
        p_Value   = rst_test$p.value)
}
