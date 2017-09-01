setwd("C:/Users/Hou/Desktop/ann")
source("let.R");
source("feedforward.R");
source("cost.R");
source("factor.R");
source("backpropagation.R");
source("adadelta.R");
source("predict.R")
library("magrittr");
##  use iris data demo
s <- c(
  sample(x =   1:50  , size = 40),
  sample(x =  51:100 , size = 40),
  sample(x = 101:150 , size = 40)
)   
object  <- let(neurons = 4) %>% let(neurons = 15) %>% classifier(neurons = 3)
ada_fit <- adadelta(object, data = iris[s,-5], y = iris[s,5], itermax = 200, tolerance = 10^-6);
##  train accuracy
ada_fit@accuracy
##  test
y_hat <- predict.adadelta(object = ada_fit, data = iris[-s,-5])
table("y hat" = y_hat, "y" = iris[-s,5])


