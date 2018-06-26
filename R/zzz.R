#' @import checkmate
#' @import parallelMap
#' @import ParamHelpers
#' @import smoof
#' @import fs
NULL

.onLoad = function(libname, pkgname) { # nocov start
  parallelRegisterLevels(package = "randomsearch", levels = c("feval"))
} # nocov end