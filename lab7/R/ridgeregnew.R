ridgereg <- function(formula, data, lambda=0.1){
  X <- model.matrix(formula, data)
  y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
  stopifnot(is.numeric(y)&is.numeric(X))
  I<-matrix(0,nrow=ncol(X),ncol=ncol(X))
  diag(I)<-1
  ans<-solve(t(X)%*%X+lambda*I)%*%(t(X)%*%y)
  beta <- as.vector(ans)
  names(beta) <- colnames(X)
  #the fitted values 
  fit <- X %*% beta
  fit <- as.vector(fit)
  
  #the residuals
  res <- y - fit
  fit.res<-data.frame(fit,res)
  #not working if class is linreg
  # class(fit.res) <- "linreg"
  names(fit.res)<-c("fit","res")
  
  
  
  res <- as.vector(res)
  
  fit.res <- data.frame(fit, res)
  names(fit.res) <- c("fit", "res")
  
  
  #the degree of freedoms
  n <- nrow(X)
  p <- ncol(X)
  df <- n - p
  a <- list(coefficients = beta, fitted = fit, residuals = res)
  a$call <- match.call()
  class(a) <- "ridgereg"
  return(a)   
}