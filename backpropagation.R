
backpropagation <- function(object){
  L <- length(object@neurons);
  N <- ncol(object@feedforwards[[L]]);
  n <- seq(N);
  accumulations <- c(0, object@accumulations);
  gradients     <- rep(NA, length(object@parameters));

  switch(object@activations[L],
         "classifier" = {
           delta <- (object@feedforwards[[L]] - object@y) / N;
         })

  ##  backpropagation start
  i <- L
  j <- length(accumulations)
  repeat{
    gradients[ (accumulations[j-1]+1):accumulations[j] ] <-
      apply(delta,1,sum);
    j <- j - 1;
    gradients[ (accumulations[j-1]+1):accumulations[j] ] <-
      Reduce(
        "+",
        lapply(n, function(k){
          delta[,k] %o% object@feedforwards[[i-1]][,k];
        }
        )
      );
    j <- j - 1;
    if( (i-1)==1 ) break

    ##  update delta
    ##  i to i-1 layer
    switch(object@activations[i-1],
           "sigmoid" = {
             da <-
               object@feedforwards[[i-1]] * (1-object@feedforwards[[i-1]]);
           })
    weights <- array(
      object@parameters[ (accumulations[j]+1):accumulations[j+1] ] ,
      c(object@neurons[i],object@neurons[i-1])
    );
    delta <- t(weights) %*% delta;
    delta <-
      sapply(n, function(k){
        delta[,k] * object@feedforwards[[i-1]][,k]
      })
    i <- i - 1;
  }
  
  object <- new("backpropagation", "gradients" = gradients, object)
}

