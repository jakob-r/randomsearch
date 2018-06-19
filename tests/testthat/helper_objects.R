testf.fsphere.1d = makeSphereFunction(dimensions = 1L)
testd.fsphere.1d = generateDesign(5L, getParamSet(testf.fsphere.1d))

testf.fsphere.2d = makeSphereFunction(dimensions = 2L)
testd.fsphere.2d = generateDesign(10L, getParamSet(testf.fsphere.1d))

testf.zdt1.2d = makeZDT1Function(dimensions = 2L)
testd.zdt1.2d = generateDesign(10L, getParamSet(testf.fsphere.1d))