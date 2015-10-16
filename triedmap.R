
al1<-get_map(location = c(lon = c(40,60), lat = c(-89,-120),zoom = 2, maptype = 'roadmap'))
al1MAP<-ggmap(al1)+geom_point(data=data, aes(x=lon, y=lat), colour=data$m_delay)+
  xlab("Longitude")+ylab("Latitude")+ggtitle("The mean delay of flights for different airports")+
  scale_color_gradient(low="blue", high="red")

return(al1MAP)
#try for map did not work