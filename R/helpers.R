checkTermination = function(fun, y, i, time.start, target.fun.value, max.execbudget, max.evals) {
  if (!is.null(target.fun.value)) {
    if (shouldBeMinimized(fun)) {
      term = y <= target.fun.value
    } else {
      term = y >= target.fun.value
    } 
  }
  if (!term && !is.null(max.execbudget)) {
    term = max.execbudget <= as.double(Sys.time() - time.start, units = "secs")
  }
  if (!term && !is.null(max.evals)) {
    term = i >= max.evals
  }
}
  