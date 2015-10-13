#'Ridge regression
#'
#'It is one type of regression using to reduce multicollinearity amongst regression predictor variables in a model.
#'
#'@param formula a symbolic description of the model to be fitted.
#'@param waiting an optional data frame or list containing the variables in the model.
#'@param lambda an optional parameter which is a scalar of ridge constant. The default value is set to 0.1.
#'
#'@return Returns an object of class \code{"ridgereg"}
#'
#'Followings are object of class \code{"ridgereg"}:
#'\itemize{
#'  \item \code{coefficients}: a named vector of ridge regression coefficients
#'  \item \code{predict}: a predicted values
#'}
#'
#'@examples 
#'ridereg(eruptions~waiting, data=faithful)
#'

ridgereg <- function(y, x, lambda=0.1){ #later try to find best lambda!! just temporary solution!
   # X <- model.matrix(formula, data)
    #y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
   # stopifnot(is.numeric(y)&is.numeric(X))
     library(som)
  # xnorm<- normalize(X, byrow=TRUE)
   # xnorm <- as.matrix(t(t(X)-apply(X,2,mean)))#/diag(var(X)) #how do you think we have to devide my variance?
  # xmean<-as.matrix(t(t(X)-apply(X,2,mean)))
   #xnorm<-xmean/apply(xmean, 2, sd)
  xnorm<-as.matrix(x)
  X<-as.matrix(x)
  y<-as.numeric(y)
    I <- matrix(0,nrow=ncol(X),ncol=ncol(X))
    diag(I) <- 1
    ans <- solve(t(xnorm)%*%xnorm+lambda*I)%*%(t(xnorm)%*%y)
    
    beta <- as.vector(ans)
    names(beta) <- colnames(X)
    #the fitted values 
    fit <- X %*% beta
    fit <- as.vector(fit)
    
    #the residuals
    res <- y - fit
    fit.res<-data.frame(fit,res)
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