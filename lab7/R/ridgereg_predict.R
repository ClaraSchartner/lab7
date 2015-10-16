#'Predict method for Ridge regression model
#'
#'\code{predict} give predict values based on class \code{"ridgereg"}.
#'
#'@param x an object of class.
#'@param ... further arguments passed to or from other methods.
#'
#'@return produce a vector of predictions.
#'

<<<<<<< HEAD
predict.ridgereg <- function(x, x_values = "default"){
  if(identical(x_values, "default")){
    # predict the same data the model was created on:
    return(x$fitted)
  }else{
    xnorm<-as.matrix(x)
    X<-as.matrix(cbind(1,x_values))
    y<-as.numeric(y)
    I <- matrix(0,nrow=ncol(X),ncol=ncol(X))
    diag(I) <- 1
    ans <- solve(t(xnorm)%*%xnorm+lambda*I)%*%(t(xnorm)%*%y)
    
    beta <- as.vector(ans)
    names(beta) <- colnames(X)
    #the fitted values 
    fit <- X %*% beta ##include in ridge!!
    fit <- as.vector(fit)
    return(fit)
  }
  
}

=======
predict.ridgereg <- function(x, ...){
    return(x$fitted)
}
>>>>>>> 45e83badb03fd6c7058ccc83db3136696dc111fc
