
cost <- function(object, y){
  L      <- length(object@neurons);
  N      <- length(y);
  object <- new("cost", "y" = y, object);
  switch(object@activations[L],
         "classifier" = {
           ##  cost
           object@y    <- factor.matrix(object@y);
           object@loss <- "entropy";
           object@cost <-
             sum( - object@y * log( object@feedforwards[[L]] ) ) / N;
           cat("cost:", object@cost, "\n");
           ##  accuracy
           hat <- object@feedforwards[[L]];
           rownames( hat ) <- levels(y);
           hat <- factor.vector(hat); levels(hat) <- levels(y);
           accuracy <- sum(hat == y) / N;
           object@accuracy <- accuracy;
           cat("accuracy:", object@accuracy, "\n");
         })
  return(object)
}

