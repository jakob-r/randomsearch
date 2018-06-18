#' @title Optimizes a function with random search.
#'
#' @description
#' This function is analog to `mlrMBO::mbo` and can be parallelized.
#'
#' @param fun [\code{smoof_function}]\cr
#'   Fitness function to optimize.
#'   For one dimensional target functions you can obtain a \code{smoof_function} by using \code{\link[smoof]{makeSingleObjectiveFunction}}.
#'   For multi dimensional functions use \code{\link[smoof]{makeMultiObjectiveFunction}}.
#'   It is possible to return even more information which will be stored
#'   in the optimization path. To achieve this, simply append the attribute \dQuote{extras}
#'   to the return value of the target function. This has to be a named list of scalar values.
#'   Each of these values will be stored additionally in the optimization path.
#' @param design [\code{data.frame}]\cr
#'   Initial design as data frame.
#'   If the y-values are not already present in design, randomsearch will evaluate the points.
#'   If the parameters have corresponding trafo functions, the design must not be transformed before it is passed!
#'   Functions to generate designs are available in \code{ParamHelpers}: \code{\link[ParamHelpers]{generateDesign}}, \code{\link[ParamHelpers]{generateGridDesign}}, \code{\link[ParamHelpers]{generateRandomDesign}}.
#'   Default is \code{NULL}, which means \code{\link[ParamHelpers]{generateDesign}} is called and a design of size 4 times number of all parameters is created
#'   The points are drawn via \code{\link[lhs]{maximinLHS}} to maximize the minimal distance between design points.
#' @param max.evals [\code{integer(1)}]\cr
#'   Maximum number of evaulations of the objective functions.
#'   Includes the initial design.
#' @param max.execbudget [\code{integer(1)}]\cr
#'   Exceution time budget in seconds.
#' @param target.fun.value [\code{numeric(1)}]\cr
#'   Traget function value.
#' @param design.y.cols [\code{characer()}]\cr
#'   The name of the column containing the function outcomes.
#'   One for single-crit optimization.
#'   Multiple for multi-crit optimization.
#' @param par.dir [\code{character(1)}]\cr
#'   Location to store parallel communication files.
#' @param par.jobs [\code{integer(1)}]\cr
#'   How many parallel jobs do jo want to run to evaluate the random search?
#' @template arg_showinfo
#' @return [\code{\link[ParamHelpers]{OptPath}}
#' @export
#' @examples
#' # simple 2d objective function
#' obj.fun = makeSingleObjectiveFunction(
#'  fn = function(x) x[1]^2 + sin(x[2]),
#'  par.set = makeNumericParamSet(id = "x", lower = -1, upper = 1, len = 2)
#' )
#'
#' # create initial design
#' des = generateDesign(n = 5L, getParamSet(obj.fun), fun = lhs::maximinLHS)
#' 
#' # start random search
#' res = randomsearch(obj.fun, design = des, max.evals = 10)
randomsearch = function(fun, design = NULL, max.evals = 20, max.execbudget = NULL, target.fun.value = NULL, show.info = getOption("randomsearch.show.info", TRUE), design.y.cols = NULL, par.dir = "~/.randomsearch/", par.jobs = NULL) {

  assertDataFrame(design)
  assertClass(smoof.fun, "smoof_function")

  par.set = getParamSet(smoof.fun)
  x.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  if (is.null(design.y.cols)) {
    if (getNumberOfObjectives(smoof.fun) == 1){
      design.y.cols = "y"
    } else {
      design.y.cols = paste("y", seq_len(d), sep = "_")
    }
  }
  assertCharacter(design.y.cols)

  if (is.null(par.jobs)) {
    par.jobs = parallelGetOptions()$settings$cpus
  }
  assertCount(par.jobs, low)

  opt.path = makeOptPathDF(par.set = par.set, y.names = design.y.cols, minimize = shouldBeMinimized(fun), add.transformed.x = FALSE, include.exec.time = TRUE)
  time.start = Sys.time()

  # Treat Initial Design
  if (!is.null(design)) {
    xs = dfRowsToList(design[, getParamIds(par.set, repeated = TRUE, with.nr = TRUE)], par.set)

    # evaluate initial design
    if (! design.y.cols %in% colnames(design)) {
      if (hasTrafo(par.set)) {
        xs.trafo = lapply(xs, trafoValue, par = par.set)
      } else {
        xs.trafo = xs
      }
      wrap.fun = function(...) {
        st = proc.time()
        y = smoof.fun(...)
        st = proc.time() - st
        list(y = y, time = st[3])
      }
      ys = parallelMap(wrap.fun, xs.trafo, level = "randomsearch.feval")
    # split y values
    } else {
      ys = design[, design.y.cols, drop = FALSE]
      ys = apply(ys, 1, function(y) list(y = y, time = NA_real_))
    }

    # add inital design to opt path
    lapply(seq_along(xs), function(i)
      addOptPathEl(opt.path, x = xs[[i]], y = ys[[i]]$y, dob = i, exec.time = ys[[i]]$time)
    )
  }

  
  

  

  time.end = Sys.time()

  lapply(seq_along(xs), function(i)
    addOptPathEl(opt.path, x = xs[[i]], y = ys[[i]]$y, dob = i, exec.time = ys[[i]]$time)
  )

  return(opt.path)
  
}
