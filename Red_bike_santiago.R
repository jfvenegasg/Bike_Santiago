library(httr)

datos_bike <- GET("http://api.citybik.es/v2/networks/bikesantiago")

contenido_datos_bike<-content(datos_bike)

n_estaciones<-length(contenido_datos_bike$network$stations)

id<-seq(1,n_estaciones)*0
direccion<-id
empty<-id
free<-id
dock<-id
latitud<-id
longitud<-id

#Ordenando los datos que vienen del archivo json  
for(i in seq(1,n_estaciones)){
  
  id[i]<-contenido_datos_bike$network$stations[[i]][[2]][[7]]
  direccion[i]<- contenido_datos_bike$network$stations[[i]][[7]]
  empty[i]<-contenido_datos_bike$network$stations[[i]][[1]]
  free[i]<-contenido_datos_bike$network$stations[[i]][[3]]
  dock[i]<-empty[i]+free[i]
  latitud[i]<-contenido_datos_bike$network$stations[[i]][[5]]
  longitud[i]<-contenido_datos_bike$network$stations[[i]][[6]]
}

contenido_datos_bike
  
datos<-data.frame(id,direccion,empty,free,dock,latitud,longitud)

class(datos$id)
datos$id<-as.integer(datos$id)

# class(datos$latitud)
# class(datos$longitud)

datos <- datos[with(datos, order(datos$id)), ]
datos

library(leaflet)

m<-leaflet()%>%addTiles()%>%addCircles(lng = longitud[1:10],lat = latitud[1:10],
                                       popup = paste(datos$direccion[1:10],"Latitud:",latitud),radius = 3,color = "blue",weight = 10)

m
