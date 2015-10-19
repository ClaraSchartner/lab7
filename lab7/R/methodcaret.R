library(dplyr)
library(tidyr)
library(caret)

ridge <- list(type=c("Regression"), 
              library="lab7",
              loop=NULL)

ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")



Fit <- function(y, x, lambda, param, lev, last, classProbs, ...){
  ridgereg1(y=y, x=x, lambda=param$lambda) 
}
ridge$fit <- Fit

ridge$prob<-list(NULL)



ridge$predict<-function(modelFit, newdata, preProc=NULL, submodels=NULL){
  predict(modelFit, newdata)
}

ridge$sort<-function (x) x[order(-x$lambda), ]

ridge$label<-"Ridge"

#fit a ridge regresstion 
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

ridge$grid <- function(y,x, len=NULL, search="grid"){
  data.frame(lambda=c(0.001,0.5,5,30,70,400))
}
