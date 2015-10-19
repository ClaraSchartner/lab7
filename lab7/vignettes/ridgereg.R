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
set.seed(3245)
lm<-train(crim ~ .,data = training, method = "lm")
lm

## ------------------------------------------------------------------------
set.seed(3245)
lm.forward<-train(crim ~ .,data = training, method = "leapForward")
lm.forward

## ------------------------------------------------------------------------
# pre.forward<-predict(newdata=training, lm.forward)
# plot(training$crim,col="red")
# lines(pre.forward)

## ---- eval=FALSE---------------------------------------------------------
#  
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
#  ridge$fit <- function(y, x, lambda, param, lev, last, classProbs, ...){
#     lab7::ridgereg1(y=y, x=x, lambda=param$lambda)
#  }
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

set.seed(3245)
lm.ridge <- train(y=training$crim, x=training[,-1], method=ridge, trControl = fitControl)
lm.ridge            

## ------------------------------------------------------------------------
set.seed(3245)
lm.test <- train(crim ~ .,data = testing, method = "lm")
lm.test

lm.forward.test <- train(crim ~ .,data = testing, method = "leapForward")
lm.forward.test

lm.ridge.test <- train(y=testing$crim, x=testing[,-1], method=ridge, trControl = fitControl)
lm.ridge.test

