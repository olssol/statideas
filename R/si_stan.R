## -----------------------------------------------------------------------------
##
##                 STAN
##
## -----------------------------------------------------------------------------
#' Call STAN models
#'
#' Call STAN models. Called by \code{psrwe_powerp}.
#'
#' @param lst_data List of study data to be passed to STAN
#' @param stan_mdl STAN model name
#' @param chains STAN parameter. Number of Markov chainsm
#' @param iter STAN parameter. Number of iterations
#' @param warmup STAN parameter. Number of burnin.
#' @param control STAN parameter. See \code{rstan::stan} for details.
#' @param ... other options to call STAN sampling such as \code{thin},
#'     \code{algorithm}. See \code{rstan::sampling} for details.#'
#'
#' @return Result from STAN sampling
#'
#' @export
#'
si_stan <- function(lst_data,
                    stan_mdl = c("hier"),
                    chains = 4, iter = 2000, warmup = 1000, cores = 4,
                    control = list(adapt_delta = 0.95), ...) {

    stan_mdl <- match.arg(stan_mdl)
    stan_rst <- rstan::sampling(stanmodels[[stan_mdl]],
                                data    = lst_data,
                                chains  = chains,
                                iter    = iter,
                                warmup  = warmup,
                                cores   = cores,
                                control = control,
                                ...)

    stan_rst
}
