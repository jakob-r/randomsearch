checkTermination = function(fun, y, i, time.start, target.fun.value, max.execbudget, max.evals) {
  term = FALSE
  if (!is.null(target.fun.value)) {
    # checks if all values that should be minimized are below the target value and wheter all values that should be maximized are above the target value
    term == all((y <= target.fun.value) == shouldBeMinimized(fun))
  }
  if (!term && !is.null(max.execbudget)) {
    term = max.execbudget <= as.double(Sys.time() - time.start, units = "secs")
  }
  if (!term && !is.null(max.evals)) {
    term = i > max.evals
  }
  return(term)
}
  