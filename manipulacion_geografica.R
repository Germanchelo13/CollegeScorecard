

# creacion mapa estados
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

