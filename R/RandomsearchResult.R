#' @title Generate result summary
#' 
#' @description
#' Generate results from randomsearch run.
#' For normal single objective runs the best result is returned.
#' For mulit-objective runs the pareto front is returned.
#' 
#' @param object [\code{\link[ParamHelpers]{OptPath}}]\cr
#'   Optimization path.
#' @param ... [any] \cr
#'   Currently ignored.
#' 
#' @export
summary.RandomsearchResult = function(object, ...) {
  yn = object$y.names
  if (length(yn) == 1) {
    ind = getOptPathBestIndex(object)
    best.el = getOptPathEl(object, ind)
    res = list(best.ind = ind, best.x = best.el$x, best.y = best.el$y, opt.path = object)
    class(res) = c("RandomsearchResultSummarySingleCrit", class(res))
  } else {
    cat("Multiobjective Search Pareto Front", "\n")
    pf = getOptPathParetoFront(object)
    opdf = as.data.frame(object, include.rest = FALSE)
    pfc = merge(opdf, pf, all.x = FALSE, all.y = TRUE)
    rownames(pfc) = rownames(pf)
    res = list(pareto.front = pfc, opt.path = object)
    class(res) = c("RandomsearchResultSummaryMultiCrit", class(res))
  }
  return(res)
}

#' @export
print.RandomsearchResult = function(x, ...) {
  print(summary(x))
}

#' @export
print.RandomsearchResultSummarySingleCrit = function(x, ...) {
  cat("Randomsearch Result:", "\n")
  cat("Eval no.: ", x$best.ind, "\n", sep = "")
  cat("x: ", paramValueToString(par = x$opt.path$par.set, x = x$best.x), "\n", sep = "")
  cat(x$opt.path$y.names, ": ", x$best.y, "\n", sep = "")
}

#' @export
print.RandomsearchResultSummaryMultiCrit = function(x, ...) {
  cat("Randomsearch Result:", "\n")
  cat("Multiobjective Search Pareto Front", "\n")
  print(x$pareto.front, ...)
}