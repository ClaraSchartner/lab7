#'Coefficient
#'
#'\code{coef} extract model coefficients from objects of class \code{"ridgereg"}.
#'
#'@param x an object of class.
#'@param ... further arguments passed to or from other methods.
#'
#'@return coefficients extracted from the model object.
#'

coef.ridgereg <- function(x, ...){
    return(x$coefficients)
}