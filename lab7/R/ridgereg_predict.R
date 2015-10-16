#'Predict method for Ridge regression model
#'
#'\code{predict} give predict values based on class \code{"ridgereg"}.
#'
#'@param x an object of class.
#'@param ... further arguments passed to or from other methods.
#'
#'@return produce a vector of predictions.
#'

predict.ridgereg <- function(x, newdata=NULL){
  if(is.null(newdata)){
    x$fitted
    }
  else{ 
    newdata <- as.matrix(newdata)
   
  fit <- newdata %*% x$coefficients
return(as.vector(fit))

  }
}


