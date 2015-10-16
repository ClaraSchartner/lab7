#'Predict method for Ridge regression model
#'
#'\code{predict} give predict values based on class \code{"ridgereg"}.
#'
#'@param x an object of class.
#'@param ... further arguments passed to or from other methods.
#'
#'@return produce a vector of predictions.
#'

predict.ridgereg <- function(x, ...){
    return(x$fitted)
}