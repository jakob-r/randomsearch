# design: data.frame (n x d)
# smoof.fun: smoof.fun to evaluate on x poitns

# out
# single obj: y-vector
# multi obj: matrix (n x p)

evalDesign = function(design, smoof.fun, design.y.cols) {
  assertDataFrame(design)
  assertClass(smoof.fun, "smoof_function")

  par.set = getParamSet(smoof.fun)

  xs = dfRowsToList(design[, getParamIds(par.set, repeated = TRUE, with.nr = TRUE)], par.set)

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
  y = map(ys, "y")
  y = do.call(rbind, y) # y now should always be a matrix [nrow(design) x d]
  colnames(y) = design.y.cols
  exec.times = unname(unlist(map_dbl(ys, "time")))
  setAttribute(y, "exec.times", exec.times)
}

if (FALSE) {
  mydev()
  parallelRegisterLevels(package = "randomsearch", levels = c("feval"))
  smoof.fun = makeZDT1Function(dimensions = 2L)
  design = generateRandomDesign(5, par.set = getParamSet(smoof.fun))
  ys = evalDesign(design, smoof.fun, c("y_1", "y_2"))
}