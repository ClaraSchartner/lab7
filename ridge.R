
ridgereg <- function(formula, data){
  X <- model.matrix(formula, data)
  y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
  stopifnot(is.numeric(y)&is.numeric(X))
  xnorm<-as.matrix(data-apply(data,2,mean))%*%as.matrix(var(data)) #do you think this is right?
  
  
  
}