testf.fsphere.1d = makeSphereFunction(dimensions = 1L)
testd.fsphere.1d = generateDesign(5L, testp.fsphere.1d)

testf.fsphere.2d = makeSphereFunction(dimensions = 2L)
testd.fsphere.2d = generateDesign(10L, testp.fsphere.2d)

testf.zdt1.2d = makeZDT1Function(dimensions = 2L)
testd.zdt1.2d = generateDesign(10L, testp.zdt1.2d)

if (FALSE) {
  fun = testf.fsphere.2d
  par.set = getParamSet(fun)
  sampleValues(par.set, max.ev, trafo = TRUE)
}