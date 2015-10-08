#ridgereg <- function(x, ...){
 # UseMethod("ridgereg")
#}

print.ridgereg <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

predict.ridgereg <- function(x, ...){
  return(x$fitted)
}

coef.ridgereg <- function(x, ...){
  return(x$coefficients)
}