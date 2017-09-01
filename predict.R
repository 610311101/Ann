predict.adadelta <- function(object, data){
  switch(tail( object@activations, 1 ),
         "classifier" ={
           lev <- levels( factor.vector(object@y) );
           class(object) <- "layer";
           object <- feedforward(object, data);
           L      <- length(object@feedforwards);
           prob   <- object@feedforwards[[L]]
           row.names(prob) <- lev;
           return( factor.vector(prob) )
         })
}