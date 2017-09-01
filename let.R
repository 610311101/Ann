
let <- function(object, neurons, activation){
  if( missing(object) ){
    object <- new("neuron", "neurons" = neurons, "activations" = NA);
    return(object)
  }else{
    if( missing(activation) ) activation <- "sigmoid";
    object@neurons      <- c(object@neurons, neurons);
    object@activations  <- c(object@activations, activation);
    size <- sum(
      tail(object@neurons,-1) * head(object@neurons,-1) +
      tail(object@neurons,-1)
      );
    cat("parameters:",size,"need.\n")
    return(object)
  }
}

classifier <- function(object, neurons){
  object@neurons      <- c(object@neurons, neurons);
  object@activations  <- c(object@activations, "classifier");
  size <- sum(
    tail(object@neurons,-1) * head(object@neurons,-1) +
      tail(object@neurons,-1)
  );
  cat("parameters:",size,"need.\n")
  return(object)
}

layer <- function(object, parameters){
  object <- new("layer", object)
  if( missing(parameters) ){
    size <- sum(
      tail(object@neurons, -1) * head(object@neurons, -1) +
        tail(object@neurons, -1)
    );
    object@parameters <- runif(size, -1, 1);
  }else{
    object@parameters <- parameters;
  }
  accumulations <- rbind(
    c(tail(object@neurons, -1) * head(object@neurons, -1)),
    c(tail(object@neurons, -1))
  );
  object@accumulations <- cumsum(c(accumulations));
  return(object)
}







