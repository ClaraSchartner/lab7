library(dplyr)
library(tidyr)

library(nycflights13)
data("flights")
#data("airports")

flights$dep_time<-round(flights$dep_time/100,0) #necesary for matching

data("weather")

rel.we<-left_join(flights,weather,by=c("origin","year","month", c("dep_time"="hour")))
rel<-select(rel.we, dep_time, dep_delay, distance, air_time, arr_delay, temp, dewp, humid, wind_speed, pressure)
rel %>% filter(complete.cases(.))

abe<-filter(rel,complete.cases(rel))
library(caret)
ind<-createDataPartition(abe$arr_delay, p=c(0.8), list=FALSE)
temp<-dim(abe)[1]
temp1<-c(1:temp) %in%  ind  #checking if index in to get TRUE FALSE and put them into filter #optimize
training<-filter(abe, temp1)
valtest<-filter(abe, !temp1)
ind<-createDataPartition(valtest$arr_delay, p=c(0.25), list=FALSE) #5% of whole dataset is 25% of the 20%
temp<-dim(valtest)[1]
temp1<-c(1:temp) %in%  ind 
test<-filter(valtest, temp1)
validation<-filter(valtest, !temp1)
#
#ridgereg(abe$arr_delay~, abe)
#r<-ridgereg(arr_delay~dep_time+distance+air_time+temp+dewp+humid+wind_speed+pressure, abe)
#coef(r)




####-------------##-----------------------####

ridge <- list(type=c("Regression"), 
              library="lab7",
              loop=NULL)

#the parameter element
ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")


#the grid element
ridge$grid <- function(y,x, len=NULL, search="grid"){
  data.frame(lambda=c(1,3,10,11,15,30,50,60,70))
}

#ridge$grid <- ridge.grid

#the fit element
Fit <- function(y, x, lambda, param, lev, last, classProbs, ...){
  #library(lab7)
  ridgereg1(y=y, x=x, lambda=param$lambda) 
}

ridge$fit <- Fit
ridge$prob<-list(NULL)

ridge$predict<-function(modelFit, newdata,preProc=NULL, submodels=NULL){
  predict(modelFit, newdata)
}


ridge$sort<-function (x) x[order(-x$lambda), ]
ridge$label<-"Ridge"
fitControl <- trainControl(
  method = "repeatedcv",
  number = 2,
  repeats = 2)
#####----######
#fit a ridge regresstion 
#train(y=abe$arr_delay, x=abe[,-5], method=ridge, trControl = fitControl)
