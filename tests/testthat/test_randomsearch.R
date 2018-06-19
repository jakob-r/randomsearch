context("randomsearch")

test_that("simple randomsearch works without init design", {
  for (tn in names(testfs)) {
    res = randomsearch(fun = testfs[[tn]], max.evals = 10)
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf, nrow = 10)
  }
})

test_that("simple randomsearch works with init design", {
  for (tn in names(testfs)) {
    res = randomsearch(fun = testfs[[tn]], max.evals = 10, design = testds[[tn]])
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf, nrow = 10)
  }
})

test_that("simple randomsearch works with max.execbudget", {
  for (tn in names(testfs)) {
    res = randomsearch(fun = testfs[[tn]], max.evals = 10000, max.execbudget = 1)
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf)
  }
})

test_that("simple randomsearch works with target.fun.value", {
  for (tn in names(testfs)) {
    fun = testfs[[tn]]
    target.fun.value = fun(sampleValue(getParamSet(fun)))
    res = randomsearch(fun = fun, max.evals = 10000, target.fun.value = target.fun.value)
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf)
  }
})

test_that("parallel randomsearch works with max.execbudget", {
  for (tn in names(testfs)) {
    parallelMap::parallelStartMulticore(2)
    res = randomsearch(fun = testfs[[tn]], max.evals = 10000, max.execbudget = 1, par.jobs = 2)
    parallelMap::parallelStop()
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf)
  }
})

test_that("parallel randomsearch works with target.fun.value", {
  for (tn in names(testfs)) {
    fun = testfs[[tn]]
    target.fun.value = fun(sampleValue(getParamSet(fun)))
    # parallelMap::parallelStartMulticore(2)
    res = randomsearch(fun = fun, max.evals = 10000, target.fun.value = target.fun.value, par.jobs = 2)
    # parallelMap::parallelStop()
    expect_class(res, "OptPath")
    opdf = as.data.frame(res)
    expect_data_frame(opdf)
  }
})

