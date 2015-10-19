## ---- echo=FALSE---------------------------------------------------------
data("iris")
data("faithful")

## ------------------------------------------------------------------------
library(lab7)
result<-ridgereg(eruptions~waiting,faithful)
result

## ------------------------------------------------------------------------
head(predict(result))

## ------------------------------------------------------------------------
coef(result)

## ------------------------------------------------------------------------
print(result)

## ------------------------------------------------------------------------
library(caret)
library(mlbench)
data("BostonHousing")
BostonHousing$chas <- as.numeric(BostonHousing$chas)-1 ##CHECK!!
intrain<-createDataPartition(BostonHousing$crim, p=0.75, list=FALSE)
training<-BostonHousing[intrain,]
testing<-BostonHousing[-intrain,]

## ------------------------------------------------------------------------
lm.s<-train(crim ~ .,data = training, method = "lm")

## ------------------------------------------------------------------------
lm.fore<-train(crim ~ .,data = training, method = "leapForward")

## ------------------------------------------------------------------------
pre.fore<-predict(newdata=testing, lm.fore)
plot(testing$crim,col="red")
lines(pre.fore)

## ------------------------------------------------------------------------
# #evaluate performance of linear regression model on training dataset
# pre.lm.train <- predict(lm.s, newdata=training)
# 
# #evaluate performance of linear regression model on test dataset
# pre.lm<-predict(newdata=testing, lm.s)

# evaluate performance of linear regression model with forward selection on test dataset
# pre.fore<-predict(newdata=testing, lm.fore) #i dont think this should be like this, see below
# 
# plot(testing$crim,col="red",type="l")
# lines(pre.fore)
# 
#  
# tenfoldcross <- trainControl(method = "repeatedcv", repeats=10)

## ---- eval=FALSE---------------------------------------------------------
#  ridge <- list(type=c("Regression"),
#                   library="lab7",
#                   loop=NULL)
#  
#  ridge$parameters <- data.frame(parameter="lambda",
#                    class="numeric",
#                    label="lambda")
#  
#  ridge$grid <- function(y,x, len=NULL, search="grid"){
#   data.frame(lambda=c(0.01,0.5,1,3,8,30))
#  }
#  
#  Fit <- function(y, x, lambda, param, lev, last, classProbs, ...){
#     lab7::ridgereg1(y=y, x=x, lambda=param$lambda)
#  }
#  ridge$fit <- Fit
#  
#  ridge$prob<-list(NULL)
#  
#  ridge$predict<-function(modelFit, newdata,preProc=NULL, submodels=NULL){
#    predict(modelFit, newdata)
#  }
#  
#  ridge$sort<-function (x) x[order(-x$lambda), ]
#  
#  ridge$label<-"Ridge"

## ------------------------------------------------------------------------
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
#fit a ridge regresstion 
ridgeTrain <- train(y=training$crim, x=training[,-1], method=ridge, trControl = fitControl)
#ridgeTrain              

