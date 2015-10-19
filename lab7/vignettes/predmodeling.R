## ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(caret)
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
#since 80% of the data is too much to handle apparently, i am scaling it down to 50% of it.
ind<-createDataPartition(abe$arr_delay, p=c(0.1), list=FALSE)
temp<-dim(abe)[1]
temp1<-c(1:temp) %in%  ind  #checking if index in to get TRUE FALSE and put them into filter #optimize
abe<-filter(abe, temp1)
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



#####----######


train(y=training$arr_delay, x=training[,-5], method=ridge, trControl = fitControl)
model<-ridgereg1(validation$arr_delay, x=validation[,-5], lambda=1)
#There seems to be no good value for lambda, since all the results are the same.
#Therefore value 1 is chosen.
f<-predict(model, test[,-5])


hist(as.vector(as.matrix(test[,5]-f)))

