library(dplyr)
library(tidyr)
library(nycflights13)
library(ggplot2)

# the mean delay of flights 
# for different airports by longitude and latitude 
# delays is a variable in the flights dataset and 
# airport information is in the airports dataset 

visualize_airport_delays <- function(){
    subset_f <- select(flights, flight, origin, dest, dep_delay, arr_delay)
    subset_a <- select(airports, faa, name, lat, lon)
    new_flights <- mutate(subset_f, total_delay = dep_delay + arr_delay)
    
    temp <- inner_join(new_flights,subset_a,by=c("dest"="faa"))
    by_name <- group_by(temp,name)
    delay <- summarize(by_name, m_delay = mean(total_delay, na.rm=TRUE))
    
    data <- inner_join(subset_a,delay,by="name")
    
    graph <- ggplot(data=data, aes(x=lon,y=lat))+geom_point(aes(color=m_delay))+
        xlab("Longitude")+ylab("Latitude")+ggtitle("The mean delay of flights for different airports")+
        scale_color_gradient(low="blue", high="red")
    
    return(graph)
}