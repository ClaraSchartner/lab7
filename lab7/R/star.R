library(dplyr)
library(tidyr)

library(nycflights13)
data("flights")
#data("airports")

flights$dep_time<-round(flights$dep_time/100,0) #necesary for matching

#rel<-select(flights, dep_time, dep_delay, distance, air_time, arr_delay)
data("weather")
#weather
rel.we<-left_join(flights,weather,by=c("origin","year","month", c("dep_time"="hour")))
rel<-select(rel.we, dep_time, dep_delay, distance, air_time, arr_delay, temp, dewp, humid, wind_speed, pressure)
