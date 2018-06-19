checkTermination = function(fun, y, i, time.start, target.fun.value, max.execbudget, max.evals) {
  term = FALSE
  if (!is.null(target.fun.value)) {
    if (unique(shouldBeMinimized(fun))) {
      term = all(y <= target.fun.value)
    } else {
      term = all(y >= target.fun.value)
    } 
  }
  if (!term && !is.null(max.execbudget)) {
    term = max.execbudget <= as.double(Sys.time() - time.start, units = "secs")
  }
  if (!term && !is.null(max.evals)) {
    term = i > max.evals
  }
  return(term)
}
  