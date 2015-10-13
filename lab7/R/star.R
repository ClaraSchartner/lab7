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


#
#ridgereg(abe$arr_delay~, abe)
r<-ridgereg(arr_delay~dep_time+distance+air_time+temp+dewp+humid+wind_speed+pressure, abe)
coef(r)