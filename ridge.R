
ridgereg <- function(formula, data, lambda=0.1){
  X <- model.matrix(formula, data)
  y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
  stopifnot(is.numeric(y)&is.numeric(X))
  xnorm<-as.matrix(t(t(X)-apply(X,2,mean)))#/diag(var(X)) #how do you think we have to devide my variance?
I<-matrix(0,nrow=ncol(X),ncol=ncol(X))
diag(I)<-1
  ans<-solve(t(xnorm)%*%xnorm+lambda*I)%*%(t(xnorm)%*%y)
  
}