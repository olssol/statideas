#' The 'statideas' package.
#'
#' @docType package
#' @name    statideas-package
#' @aliases statideas
#' @useDynLib statidea, .registration = TRUE
#'
#' @import methods
#' @import stats
#' @import ggplot2
#' @import Rcpp
#'
#' @importFrom grDevices colors
#' @importFrom graphics axis box legend lines par plot points text arrows grid
#'     rect
#' @importFrom parallel detectCores
#' @importFrom utils as.roman
#' @importFrom dplyr %>% group_by_ group_by summarize mutate count mutate_if
#'     rename filter select arrange ungroup n distinct left_join if_else rowwise
#'     slice_head
#' @importFrom tidyr gather
#' @importFrom data.table rbindlist
#'
#' @importFrom gsDesign gsDesign gsBoundSummary sfPower sfPoints
#' @importFrom rpact getDesignGroupSequential getDesignSet
#'
#' @importFrom rstan sampling
#' @importFrom mvtnorm rmvnorm pmvnorm dmvnorm
#' @importFrom GGally ggpairs
#'
#' @description Demonstrate statistics ideas and concepts through interactive
#'     Shiny applications.
#'
#'
NULL
