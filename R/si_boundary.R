#' GSD
#'
#' Functions for understanding group sequential design and interim analysis
#' boundaries
#'
#'
#' @keywords internal
#'


#' Simulate a single trial
#'
#'
#' @export
#'
si_bd_simu_trial <- function(n_tot = 200, rand_ratio = 1,
                             trt_mean = 0, ctl_mean = 0,
                             trt_sd = 1, ctl_sd = 1, ...,
                             seed = 0) {

    if (seed > 0) {
        old_seed <- set.seed(seed)
    }

    ntrt <- rbinom(1, n_tot, rand_ratio / (rand_ratio + 1))

    ## enrollment
    dta_trt <- si_simu_enroll(ntrt, ...)
    dta_ctl <- si_simu_enroll(n_tot - ntrt, ...)

    ## outcome
    y_trt <- si_simu_cont(ntrt,         trt_mean, trt_sd)
    y_ctl <- si_simu_cont(n_tot - ntrt, ctl_mean, ctl_sd)

    if (seed > 0) {
        set.seed(old_seed)
    }

    ## return
    dta_trt %>%
        mutate(arm = "Treatment") %>%
        left_join(y_trt, by = "sid") %>%
        rbind(dta_ctl %>%
              mutate(arm = "Control") %>%
              left_join(y_ctl, by = "sid")) %>%
        arrange(day_enroll) %>%
        mutate(sid = row_number())
}

#' Simulate all trials
#'
#'
#' @export
#'
si_bd_simu_all <- function(n_rep = 100, ..., n_cores = 5) {

    rst <- parallel::mclapply(seq_len(n_rep),
                              function(x) {
                                  rst <- si_bd_simu_trial(...) %>%
                                      mutate(rep = x) %>%
                                      data.frame()
                                  si_bd_ana_trial(rst)
                              },
                              mc.cores = n_cores)

    rbindlist(rst)
}


#' Analyze a single trial
#'
#'
#' @export
#'
si_bd_ana_trial <- function(dat, y_sd = 1) {
    xx <- dat %>%
        mutate(tt = if_else("Treatment" == arm, 1, 0)) %>%
        select(tt, y) %>%
        as.matrix()

    diff <- c_two_arm_diff(xx)

    dat %>%
        mutate(mean       = diff[, 6],
               difference = diff[, 3] / diff[, 2] - diff[, 5] / diff[, 4],
               sd         = y_sd * sqrt(1 / diff[, 2] + 1 / diff[, 4]),
               zscore     = difference / sd,
               pvalue     = 1 - pnorm(zscore))
}

#' GS Tests
#'
#'
#' @export
#'
si_bd_gs_trial <- function(dat, bds = cbind(1, 1.96)) {
    dat        <- data.frame(dat)
    n          <- nrow(dat)
    inx        <- ceiling(n * bds[, 1])
    ind_test   <- as.numeric(dat[inx, "zscore"] >= bds[, 2])
    difference <- as.numeric(dat[inx, "difference"])

    inx_na     <- which(is.na(ind_test))
    if (length(inx_na) > 0)
        ind_test[inx_na] <- FALSE

    cum_test <- cumsum(ind_test) > 0

    ## return
    rst <- data.frame(interim_id  = seq_len(nrow(bds)),
                      interim     = bds[, 1],
                      significant = ind_test,
                      cumu_sig    = cum_test,
                      effect      = difference)

    rst
}

#' GS Tests for all
#'
#'
#' @export
#'
si_bd_gs_all <- function(dat, bds = cbind(1, 1.96)) {

    f_gs <- function(dat, rep) {
        rst     <- si_bd_gs_trial(dat, bds)
        rst$rep <- as.matrix(rep)[1, 1]

        rst
    }

    rst <- dat %>%
        group_by(rep) %>%
        group_map(.f = ~f_gs(dat = .x, rep = .y),
                  .keep = TRUE) %>%
        bind_rows()

    list(dat      = dat,
         rej      = rst,
         bds      = bds,
         n_rep    = max(dat$rep))
}


#' Plot data for single trial
#'
#'
#' @export
#'
si_bd_plt_single <- function(dat,
                             type = c("y",    "mean",
                                      "difference", "zscore", "pvalue"),
                             ref_line = NULL,
                             col      = "black",
                             n_first = NULL) {

    plt_arm <- function(dat) {
         ggplot(data = dat,
               aes(x = day_enroll, y = outcome)) +
            geom_line(aes(group = arm, col = arm)) +
            geom_point(aes(group = arm, col = arm)) +
            labs(x = x_lab, y = type) +
            theme_bw() +
            theme(legend.position = "bottom",
                  legend.title    = element_blank())
    }

    plt_diff <- function(dat) {
        ggplot(data = dat %>%
                   dplyr::filter(!is.na(outcome)),
               aes(x = day_enroll, y = outcome)) +
            geom_line() +
            geom_point() +
            labs(x = x_lab, y = type) +
            theme_bw()
    }

    type        <- match.arg(type)
    dat$outcome <- dat[[type]]

    ## xlim and ylim
    x_max   <- max(dat$day_enroll)
    y_lim   <- tkt_lim(c(dat$outcome, ref_line))

    ## filter the first patients
    if (!is.null(n_first)) {
        dat   <- dat %>%
            arrange(day_enroll) %>%
            filter(sid <= n_first)

        x_lab <- paste("Days (Up to N=", nrow(dat), ")", sep = "")
    } else {
        x_lab <- "Days"
    }

    if (type %in% c("y", "mean")) {
        rst <- plt_arm(dat)
    } else {
        rst <- plt_diff(dat)
    }

    ## set xy_lim
    if (!is.null(n_first)) {
        rst <- rst +
            xlim(0, x_max) +
            ylim(y_lim[1], y_lim[2])
    }

    ## reference line
    if (!is.null(ref_line)) {
        for (j in ref_line) {
            rst <- rst +
                geom_hline(yintercept = j, col = col, lty = 2)
        }
    }

    ## return
    rst
}

#' Plot data for all
#'
#'
#' @export
#'
si_bd_plt_rep <- function(dat_rej, sel = NULL,
                          type = c("zscore", "difference", "pvalue"),
                          ref_line = TRUE) {

    type <- match.arg(type)
    dat  <- dat_rej$dat %>%
        left_join(dat_rej$rej %>%
                  filter(interim == 1),
                  by = "rep")

    dat$outcome <- dat[[type]]

    if (!is.null(sel)) {
        if (1 == length(sel)) {
            sel <- sample(dat_rej$n_rep, min(sel, dat_rej$n_rep))
        }

        dat  <- dat %>%
            filter(rep %in% sel)
    }

    dat <- dat %>%
        filter(!is.na(outcome))

    rst <- ggplot(data = dat, aes(x = sid, y = outcome)) +
        geom_line(data = dat %>% filter(cumu_sig == FALSE),
                  aes(group = rep),
                  col = "gray80") +
        geom_line(data = dat %>% filter(cumu_sig == TRUE),
                  aes(group = rep),
                  col = "brown") +
        theme_bw() +
        theme(legend.position = "none") +
        labs(x = "Number of Patients", y = type)

    if (ref_line & type != "difference") {
        n_tot   <- max(dat$sid)
        bds     <- dat_rej$bds

        if ("pvalue" == type)
            bds[, 2] <- 1 - pnorm(bds[, 2])

        dat_ref <- data.frame(x = ceiling(n_tot * bds[, 1]),
                              y = bds[, 2])

        for (i in 1:nrow(dat_ref)) {
            rst <- rst +
                geom_vline(xintercept = dat_ref[i, 1],
                           col = "red", lty = 2)
        }

        y_lim <- tkt_lim(c(dat$outcome, dat_ref$y))
        rst   <- rst +
            geom_line(data = dat_ref, aes(x = x, y = y),
                      lty = 2, lwd = 2, col = "blue") +
            geom_point(data = dat_ref, aes(x = x, y = y),
                       pch = 17, size = 3) +
            ylim(y_lim[1], y_lim[2])
    }

    ## return
    rst
}

#' Plot data for alpha summary
#'
#'
#' @export
#'
si_bd_plt_alpha <- function(rej, alpha = 0.05) {
    ggplot(data = rej, aes(x = interim, y = rejection)) +
        geom_line(aes(group = type, col = type)) +
        geom_point(aes(group = type, pch = type)) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        xlim(0, 1) +
        geom_hline(yintercept = alpha, col = "red", lty = 2)
}


#' Summarize test results
#'
#'
#' @export
#'
si_bd_sum_rep <- function(dat_rej) {

    rst <- dat_rej$rej %>%
        filter(!is.na(significant)) %>%
        group_by(interim_id, interim) %>%
        summarize(n_rejection    = sum(significant),
                  prop_rejection = n_rejection / n(),
                  n_cum_rej      = sum(cumu_sig),
                  prop_cum_rej   = n_cum_rej / n()) %>%
        arrange(interim_id)

    rej <- rst %>%
        gather(key   = "type",
               value = "rejection",
               prop_rejection, prop_cum_rej) %>%
        mutate(type = factor(type,
                             levels = c("prop_rejection",
                                        "prop_cum_rej"),
                             labels = c("Individual Rejection",
                                        "Cumulative Rejection")))


    rst_est <- dat_rej$rej %>%
        filter(1 == significant) %>%
        group_by(interim_id) %>%
        summarize(Treatment_Effect = mean(effect))


    rej_tbl <- rst %>%
        rename(Information_Fraction = interim,
               Rej_Trials_N         = n_rejection,
               Rej_trials_Rate      = prop_rejection,
               Cumu_Rej_Trials_N    = n_cum_rej,
               Cumu_Rej_Trials_Rate = prop_cum_rej) %>%
        left_join(rst_est, by = "interim_id")


    list(rej_wide = rst,
         rej      = rej,
         rej_tbl  = rej_tbl)
}

#' Get Boundary
#'
#'
#' @export
#'
si_bd_get_bd <- function(type = c("NO", "P", "OF",
                                  "HP", "asP", "asOF"),
                         alpha = 0.025, n_interim = 1,
                         info_frac = NULL) {

    type <- match.arg(type)

    if (is.null(info_frac))
        info_frac <- seq(0, 1, length.out = n_interim + 2)[-1]

    if ("NO" == type) {
        bds     <- cbind(info_frac, qnorm(1 - alpha))
        details <- NULL
    } else {
        details <- getDesignGroupSequential(sided = 1,
                                            alpha = alpha,
                                            informationRates = info_frac,
                                            typeOfDesign     = type)

        bds <- cbind(info_frac, details$criticalValues)
    }

    list(bds     = bds,
         details = details)
}

#' Get All Boundares
#'
#'
#' @export
#'
si_bd_plt_bd <- function(n_interim = 3, designs = c("P", "OF", "HP"),
                         type = 1,
                         alpha = 0.025) {

    ds <- list()
    for (ty in designs) {
        d <- getDesignGroupSequential(sided        = 1,
                                      alpha        = alpha,
                                      kMax         = n_interim + 1,
                                      typeOfDesign = ty)
        ds <- c(ds, d)
    }

    designSet <- getDesignSet(designs          = ds,
                              variedParameters = "typeOfDesign")

    plot(designSet, type = type)
}
