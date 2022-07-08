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
si_simu_cont <- function(ntot, mean = 0, sd = 1) {
    rst <- data.frame(sid  = seq_len(ntot),
                      y    = rnorm(ntot, mean = mean, sd = sd))

    ## return
    rst
}
