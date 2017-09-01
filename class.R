setOldClass("character")
setClassUnion("character OR logical", c("character", "logical"))
setClass(
  "neuron",
  representation(
    "neurons"    = "numeric",
    "activations" = "character OR logical"
  ),
  prototype(
    "neurons"    = c(4,7,5,3),
    "activations" = c(NA,"sigmoid","sigmoid","softmax")
  )
)

setClass(
  "layer",
  representation(
    "parameters"   = "numeric",
    "accumulations" = "numeric"
  ),
  prototype(
    "parameters" = c(1:93),
    "accumulations" = c(28,35,70,75,90,3)
  ),
  contains = "neuron"
)

setClass(
  "feedforward",
  representation(
    "feedforwards" = "list"
  ),
  contains = "layer"
)

setOldClass("factor")
setClassUnion("factor OR numeric OR matrix", c("factor", "numeric", "matrix"))
setClass(
  "cost",
  representation(
    "accuracy" = "numeric",
    "cost" = "numeric",
    "loss" = "character",
    "y"    = "factor OR numeric OR matrix"
  ),
  contains = "feedforward"
)

setClass(
  "backpropagation",
  representation(
    "gradients" = "numeric"
  ),
  contains = "cost"
)
