testfs = list(
  sphere.1d = makeSphereFunction(dimensions = 1L),
  sphere.2d = makeSphereFunction(dimensions = 2L),
  zdt1.2d = makeZDT1Function(dimensions = 2L)
)
testds = list(
  sphere.1d = generateRandomDesign(5L, getParamSet(testfs$sphere.1d)),
  sphere.2d = generateRandomDesign(10L, getParamSet(testfs$sphere.2d)),
  zdt1.2d = generateRandomDesign(10L, getParamSet(testfs$zdt1.2d))
)

testdsy = list(
  sphere.1d = cbind(testds$sphere.1d, y = seq_len(nrow(testds$sphere.1d))),
  sphere.2d = cbind(testds$sphere.2d, y = seq_len(nrow(testds$sphere.2d))),
  zdt1.2d = cbind(
    testds$zdt1.2d, 
    y_1 = seq_len(nrow(testds$zdt1.2d)), 
    y_2 = seq_len(nrow(testds$zdt1.2d))
  )
)
