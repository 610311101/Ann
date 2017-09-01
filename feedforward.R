
feedforward <- function(object, data){
  object        <- new("feedforward", object);
  L             <- length(object@neurons)
  accumulations <- 1;
  i             <- 1;
  j             <- 1;

  ##  input-layer
  data                     <- t(array(data)); attr(data, "dimnames") <- NULL;
  object@feedforwards[[1]] <- data;

  ##  hidden-layer
  repeat{
    ##  i to i+1 layer weights
    weights <- array(
      object@parameters[ accumulations:object@accumulations[j] ],
      dim = c(object@neurons[i+1], object@neurons[i])
    )
    accumulations <- object@accumulations[j] + 1;
    j <- j + 1;

    ##  i to i+1 layer biases
    biases  <- array(
      object@parameters[ accumulations:object@accumulations[j] ],
      dim = c(object@neurons[i+1], 1)
    )
    accumulations <- object@accumulations[j] + 1;
    j <- j + 1;
    #show(weights);

    ##  i to i+1 layer feedforwards
    object@feedforwards[[i+1]] <-
      apply(
        weights %*% object@feedforwards[[i]],
        2,
        function(x) x + biases
      )
    #show(biases);
    ##  update i
    i <- i + 1;
    if( i == L ) break

    ##  i to i+1 layer activation
    switch(object@activations[i],
           "sigmoid" = {
             object@feedforwards[[i]] <-
               1 / ( 1 + exp( - object@feedforwards[[i]] ) );
           })
  }

  ##  output-layer
  switch(object@activations[i],
         "classifier" = {
           object@feedforwards[[i]] <-
             apply(
               object@feedforwards[[i]],
               2,
               function(x) exp(x) / sum( exp(x) )
             );
         })

  return(object)
}
