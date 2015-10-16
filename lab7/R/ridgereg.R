#'Ridge regression
#'
#'It is one type of regression using to reduce multicollinearity amongst regression predictor variables in a model.
#'Rigreg1 does not take a formula but two matrizes.
#'
#'@param Y is the matrix of dependent variable
#'@param X independent variable
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

ridgereg1 <- function(y, x, lambda=0.1){ 

  X<-as.matrix(x)
  y<-as.numeric(y)
    I <- matrix(0,nrow=ncol(X),ncol=ncol(X))
    diag(I) <- 1
    ans <- solve(t(X)%*%X+lambda*I)%*%(t(X)%*%y)
    
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