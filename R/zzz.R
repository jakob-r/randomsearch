#' @import checkmate
#' @import mlr
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @import purrr
#' @import fs
NULL

.onLoad = function(libname, pkgname) { # nocov start
  backports::import(pkgname)
  parallelRegisterLevels(package = "randomsearch", levels = c("feval"))
} # nocov end