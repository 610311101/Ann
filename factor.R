factor.matrix <- function(factor){
  label            <- diag(length(levels(factor)));
  row.names(label) <- levels(factor);
  i   <- matrix(c(1:length(factor)));
  tab <- apply(i, 1, function(ii){
    tab <- label[row.names(label) == factor[ii],];
    return(tab)
  })
  colnames(tab)  <- seq(ncol(tab));
  row.names(tab) <- levels(factor);
  return(tab);
}

factor.vector <- function(matrix){
  lab.factor <- as.factor(row.names(matrix));
  result     <- apply(matrix, 2, function(x){
    names(x) <- lab.factor;
    y        <- names(sort(x,decreasing = T)[1])
    return(y)
  })
  result <- as.factor(result);
  return(result)
}