#'Print values
#'
#'\code{print} prints its argument and returns it out for class \code{"ridgereg"}.
#'
#'@param x an object used to select a method.
#'@param ... further arguments passed to or from other methods.
#'
#'@return print out the coefficients and coefficient names.
#'

print.ridgereg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}



