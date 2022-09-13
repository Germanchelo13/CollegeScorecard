library(tidygeocoder)

# creacion mapa estados
datos<- read.csv('CollegeScorecard_result.csv')
filtro<-is.na(datos$LATITUDE) | is.na(datos$LONGITUDE) 
rest_coord<- datos[filtro,c("UNITID",'INSTNM',"CITY","STABBR","ZIP" )]
rest_coord$address<-paste(rest_coord$INSTNM, rest_coord$CITY,rest_coord$STABBR,rest_coord$ZIP, 'USA', sep=",")
new_localizacion<-geocode(rest_coord,'address', method = "arcgis" )
datos[filtro,'LATITUDE']<- new_localizacion$lat
datos[filtro,'LONGITUDE']<- new_localizacion$long
write.csv(datos,'CollegeScorecard_result.csv',row.names = FALSE)
writ
datos<- read.csv('CollegeScorecard_result_cod.csv')
mapa_usa<-st_read('map_states/geoBoundaries-USA-ADM1_simplified.shp')
mapa_usa<-mapa_usa %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857)


datos$CLUSTER<-factor(datos$CLUSTER)
for (var_ in var_cat){
  datos[,var_]<-factor(datos[,var_])
}
datos_geo <- datos %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE')) %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857) 
mapa_puntos<- datos_geo %>%
  st_intersection(mapa_usa)
prueba<-mapa_puntos %>% group_by(shapeName ) %>% summarise( conteo=length(unique(STABBR)) )

