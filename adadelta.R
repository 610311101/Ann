adadelta <-
  function(object, data, y, decay = 0.95, tolerance = 10^-6, itermax = 200){
    object <- layer(object);
    AccumulateGradient      <- 0;
    AccumulationUpdate     <- 0;
    iter <- 1;
    repeat{
      class(object) <- "layer";
      object <- feedforward(object,data);
      object <- cost(object, y);
      if( iter == itermax ) break;
      object <- backpropagation(object);
      gradients <- object@gradients;
      AccumulateGradient <- decay * AccumulateGradient +
        (1 - decay) * (gradients^2);
      Update <-
        - ( sqrt(AccumulationUpdate + tolerance) / sqrt(AccumulateGradient + tolerance) ) * gradients
      AccumulationUpdate <- decay * AccumulationUpdate +
        (1 - decay) * (Update^2);
      object@parameters <- object@parameters + Update;
      iter <- iter + 1;
    }
    return(object)
  }
