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
#' @return [\code{\link[ParamHelpers]{OptPath}}
#' @export
#' @examples
#' # simple 2d objective function
#' obj.fun = makeSingleObjectiveFunction(
#'  fn = function(x) x[1]^2 + sin(x[2]),
#'  par.set = makeNumericParamSet(id = "x", lower = -1, upper = 1, len = 2)
#' )
#' 
#' # start random search
#' res = randomsearch(obj.fun, max.evals = 10)
randomsearch = function(fun, design = NULL, max.evals = 20, max.execbudget = NULL, target.fun.value = NULL, design.y.cols = NULL, par.dir = "~/.randomsearch/", par.jobs = NULL) {

  assertDataFrame(design, null.ok = TRUE)
  design.n = nrow(design) %??% 0
  assertClass(fun, "smoof_function")

  if (length(unique(shouldBeMinimized(fun))) > 1) {
    stop("Objective functions with mixed minimization/maximization for each objective are not allowed.")
  }

  max.evals = asInt(max.evals, lower = design.n)
  max.evals = max.evals - design.n

  par.set = getParamSet(fun)
  x.ids = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)

  if (is.null(design.y.cols)) {
    d = getNumberOfObjectives(fun)
    if (d == 1){
      design.y.cols = "y"
    } else {
      design.y.cols = paste("y", seq_len(d), sep = "_")
    }
  }
  assertCharacter(design.y.cols)

  if (is.null(par.jobs)) {
    par.jobs = parallelGetOptions()$settings$cpus
  } else {
    assertCount(par.jobs)
  }
  
  # what mode should we use
  if (!is.null(max.evals) && is.null(max.execbudget) && is.null(target.fun.value)) {
    mode = "fast.parallel"
  } else if ((!is.null(max.execbudget) || !is.null(target.fun.value)) && !is.na(par.jobs)) {
    mode = "slow.parallel"
  } else {
    mode = "normal"
  }

  opt.path = makeOptPathDF(par.set = par.set, y.names = design.y.cols, minimize = shouldBeMinimized(fun), add.transformed.x = FALSE, include.exec.time = TRUE)

  wrap.fun = function(...) {
    st = proc.time()
    y = fun(...)
    st = proc.time() - st
    list(y = y, time = st[3])
  }

  time.start = Sys.time()

  # Treat Initial Design
  if (!is.null(design)) {
    xs = dfRowsToList(design[, getParamIds(par.set, repeated = TRUE, with.nr = TRUE), drop = FALSE], par.set)

    # evaluate initial design
    if (! all(design.y.cols %in% colnames(design))) {
      if (hasTrafo(par.set)) {
        xs.trafo = lapply(xs, trafoValue, par = par.set)
      } else {
        xs.trafo = xs
      }
      ys = parallelMap(wrap.fun, xs.trafo, level = "randomsearch.feval", simplify = FALSE)
    # split y values
    } else {
      ys = design[, design.y.cols, drop = FALSE]
      ys = apply(ys, 1, function(y) list(y = y, time = NA_real_))
    }
    # add inital design to opt path
    lapply(seq_along(xs), function(i)
      addOptPathEl(opt.path, x = xs[[i]], y = ys[[i]]$y, dob = 0, exec.time = ys[[i]]$time)
    )
  }

  if (mode == "fast.parallel") {
    xs = sampleValues(par.set, max.evals, trafo = FALSE)
    xs.trafo = lapply(xs, trafoValue, par = par.set)
    ys = parallelMap(wrap.fun, xs.trafo, level = "randomsearch.feval", simplify = FALSE)
    lapply(seq_along(xs), function(i)
      addOptPathEl(opt.path, x = xs[[i]], y = ys[[i]]$y, dob = i, exec.time = ys[[i]]$time)
    )
  } else if (mode == "slow.parallel") {
    par.id = paste0(sample(c(letters,LETTERS,0:9), 5), collapse = "")
    par.path = path(par.dir, par.id)
    dir_create(par.path)
    wrap.fun2 = function(par.id = Sys.getpid()) {
      res = list()
      term = FALSE
      i = 1
      pid = Sys.getpid()
      while (!file.exists(path(par.path,"done")) || term == FALSE) {
        x = sampleValues(par.set, 1)
        x.trafo = trafoValue(x, par = par.set)
        st = proc.time()
        y = fun(x.trafo)
        i = i + 1
        st = proc.time() - st
        res = c(res, list(list(x = x, x.trafo = x.trafo, time = st[3], y = y)))

        # check termination
        term = checkTermination(fun, y, i, time.start, target.fun.value, max.execbudget, max.evals)
        if (!term && !is.null(max.evals)) {
          # look at all files of the scheme (par.id)_(i) and sum all i
          files = dir_ls(par.path, regexp = "\\d*_")
          files = basename(files)
          files = regmatches(files, regexpr("(?<=_).*", files, perl = TRUE))
          files = as.numeric(files)
          sum(files)
          term = sum(files)+1 >= max.evals # this node has not added itself yet
        }
        # write term file or counter file
        if (term) {
          file_create(path(par.path,"done"))
        } else {
          files = dir_ls(par.path, regexp = paste0(par.id, "_"))
          file_delete(files)
          file_create(path(par.path, paste0(par.id, "_", i)))
        }
      }
      return(res)
    }

    res.all = parallelMap(wrap.fun2, seq_len(par.jobs), level = "randomsearch.feval", simplify = FALSE)
    res.all = unlist(res.all, recursive = FALSE)
    dir_delete(par.path)
    lapply(seq_along(res.all), function(i)
      addOptPathEl(opt.path, x = res.all[[i]]$x, y = res.all[[i]]$y, exec.time = res.all[[i]]$time, dob = i)
    )
  } else if (mode == "normal") {
    term = FALSE
    i = 0
    while (term == FALSE) {
      x = sampleValue(par.set)
      x.trafo = trafoValue(x, par = par.set)
      st = proc.time()
      y = fun(x.trafo)
      i = i + 1
      st = proc.time() - st
      addOptPathEl(opt.path, x = x, y = y, exec.time = st[3], dob = i)
      # check termination
      term = checkTermination(fun, y, i, time.start, target.fun.value, max.execbudget, max.evals)
    }
  }

  time.end = Sys.time()

  return(opt.path)
  
}