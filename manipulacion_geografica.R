library(tidygeocoder) # adres to lat long
library(sf)
library(dplyr)
# creacion mapa estados
datos<- read.csv('CollegeScorecard_result.csv') # lectura resultado de python
##################################################################################
filtro<-is.na(datos$LATITUDE) | is.na(datos$LONGITUDE) 
rest_coord<- datos[filtro,c("UNITID",'INSTNM',"CITY","STABBR","ZIP" )]
rest_coord$address<-paste(rest_coord$INSTNM, rest_coord$CITY,rest_coord$STABBR,rest_coord$ZIP, 'USA', sep=",")
new_localizacion<-geocode(rest_coord,'address', method = "arcgis" )
datos[filtro,'LATITUDE']<- new_localizacion$lat
datos[filtro,'LONGITUDE']<- new_localizacion$long
write.csv(datos,'CollegeScorecard_result.csv',row.names = FALSE)
##################################################################################
datos<- read.csv('CollegeScorecard_result.csv')
mapa_usa<-st_read('map_states/geoBoundaries-USA-ADM1_simplified.shp')

mapa_usa<-mapa_usa %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857)
datos_geo <- datos %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE')) %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857) 
mapa_puntos<- datos_geo %>%
  st_intersection(mapa_usa)

top_cluster<-function(vector_ ){
  names(sort(table(vector_ ),decreasing = T))[1]
}
frec_cluster<-function(vector_, cluster){
  frec_=length( vector_[vector_==cluster])
  frec_
}

mapa_state<-mapa_puntos %>% 
  group_by(shapeName) %>% 
  summarise(Cluster_top=top_cluster(CLUSTER),
            mean_TUITFTE=mean(TUITFTE),
            mean_INEXPFTE=mean(INEXPFTE),
            Cluster_1=frec_cluster(CLUSTER,'Cluster_1' ),
            Cluster_2=frec_cluster(CLUSTER,'Cluster_2' ),
            Cluster_3=frec_cluster(CLUSTER,'Cluster_3' ),
            Cluster_4=frec_cluster(CLUSTER,'Cluster_4' )
  )
mapa_state<-as.data.frame(mapa_state)

mapa_result<-inner_join(mapa_usa, mapa_state[,names(mapa_state)[1:8]], by ="shapeName" )
st_write(mapa_result, 'map_states/mapa_CollegeScorecard.shp')

names(mapa_result)
