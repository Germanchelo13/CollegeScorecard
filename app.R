library(DT) # print table beutefull
library(sf) # manupulación geolocalizaciones
library(tmap)  # graficos interactivos de mapas
library(dplyr) # manejo de funciones
library(shiny) # aplicacion
library(plotly) # graficos 
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)# to add a loader while graph is populating
url_github<-"https://github.com/Germanchelo13/CollegeScorecard.git"
mapa_usa<-st_read('map_states/mapa_CollegeScorecard.shp')
names(mapa_usa)<-c("shapeName","Shape_Leng"  ,  "Shape_Area"  ,  "Level",    "shapeISO",
                   "shapeID",  "shapeGroup" ,   "shapeType","Cluster_top" ,  "mean_TUITFTE" ,
                   "mean_INEXPFTE", "Cluster_1","Cluster_2","Cluster_3","Cluster_4"   , 
                   "geometry" )

datos<- read.csv('CollegeScorecard_result.csv')
datos<-datos[,names(datos)[2:length(names(datos))]]
datos$address<-paste(datos$INSTNM,  # direccion 
               datos$CITY,
               datos$STABBR,
               datos$ZIP, sep=", ")
datos[datos$st_fips=='68','st_fips']<-'Marshall Islands'
diccionario<-list('TUITFTE'='Costo de matricula promedio.',
                  'INEXPFTE'='Inversión de la universidad por estudiante.',
                               'NUM_PROGRAM'='Número total de programas que ofrece la universidad',
                  'CONTROL'='Tipo de universidad.',
                  'HIGHDEG'='Nivel academico mas alto que ofrece la universidad.',
                  'PREDDEG'='Nivel academico que destaca en la universidad.',
                  'DISTANCEONLY'='Si la universidad solo ofrece modalidad virtual.',
                  'HCM2'='Si la universidad tiene riesgo financiero.')
var_numeric<-c("TUITFTE","INEXPFTE",'NUM_PROGRAM' )
var_cat<-c("CONTROL","HIGHDEG","PREDDEG","DISTANCEONLY","HCM2")


datos$CLUSTER<-factor(datos$CLUSTER)
for (var_ in var_cat){
  datos[,var_]<-factor(datos[,var_])
}

datos_geo <- datos %>%
  st_as_sf(coords = c('LONGITUDE', 'LATITUDE')) %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857) 

cluster_total<-datos%>% 
  group_by(CLUSTER)%>%
  summarise(total=n())
usuario<- fluidPage(
  dashboardPage(
    # header princial
    dashboardHeader(title="University EEUU.",
                   # titleWidth = 400, 
                    tags$li(class="dropdown",
                            tags$a(href=url_github, 
                                   icon("github"), 
                                   "Source Code", 
                                   target="_blank"))),
    # left 
    dashboardSidebar(
      sidebarMenu(id="sidebar",
        fluidRow(style='height:5vh'),
        menuItem("Introduction", tabName="intro", icon=icon("users")),
        menuItem("Dataset", tabName="data", icon=icon("database")),
        menuItem(text= "Geo loc",tabName = "map",icon = icon("earth-americas"))
      )  ),
    # itmes
    dashboardBody(tabItems (
      # mapa
      tabItem(tabName="map",
              tabBox(id="t3",width= 12,
                     # puntos universidades
                     tabPanel(title="Geo-localized  Universities",
                              icon=icon('map'),
                              fluidPage(
                                fluidRow(column(6,
                                                selectInput(inputId ="cluster" ,label= "Seleccione el cluster",
                                                            choices =unique(datos$CLUSTER),multiple = TRUE)),
                                         column(6, selectInput(inputId ="estado" ,label= "Seleccione el estado",
                                                                                       choices =unique(datos$st_fips),multiple = TRUE) ) ),
                                fluidRow(uiOutput("info_points") ,br(),br(), 
                                  tmapOutput("map_plot"),
                                  br(),uiOutput("urls")
                                  ))),
                     # estados
                     tabPanel(title = 'Geo-localized states',
                              fluidRow(uiOutput("info_state") ,br(),br(), 
                                       tmapOutput("map_plot_state")
                              ))
                     
              )),
      # item descripcion 
      tabItem(tabName = 'intro',fluidRow(style='height:5vh'),
              tabBox(id='t3',width=12,tabPanel(HTML('<i class="fa-solid fa-book"></i> Contexto'), 
                                                fluidPage(
                                                  fluidRow(uiOutput('intro_'))
                                                )),
                    tabPanel(HTML('<i class="fa-solid fa-graduation-cap"></i> Caracterización'), fluidPage(fluidRow( 
                      uiOutput('caracterizacion'),
                      plotlyOutput('torta'))) ) )) ,
      tabItem(tabName="data",
              tabBox(id="t3",width= 12,
                     tabPanel(title="Data frame Universities",icon=icon('table'),
                              fluidPage(
                                fluidRow( DT::dataTableOutput('datos_') )
                              )))),
      tabItem(tabName = 'miembros',tabBox(tabPanel(title='Mermbers',icon=icon('users-rectangle'),
                                                fluidPage(
                                                  fluidRow(uiOutput('info'))
                                                )))) 
    )
    
    )
  )
)
servidor<-function(input, output) {
  output$caracterizacion<-renderUI({

    HTML("
    <br><h2>¿Cómo son los cluster?  </h6>
    </br>
    <br>
    <b>Cluster 1 :</b> Las universidades que pertenecen a este grupo cuentan con un costo de matrícula promeido de
    7.6 mil (mas alto) dolares con una inversión por estudiante en promedio de 3.9 mil dolares
En este grupo se encuentran 2635 instituciones educativas con un costo, sin embargo, cuentan con un promedio de 2 programas, en su mayoría son instituciones privadas y con ánimo de lucro.
la gran parte de instituciones predominan y el maximo nivel academico son las licenciaturas.
</br>
<br>
<br> <b>Cluster 2:</b>
Las universidades que pertenecen a este grupo cuentan con de matrícula promedio de 14 mil dólares y una inversión promedio
por estudiante de 7.3 mil dólares, pero albergan un promedio de 15 programas, en su mayoria son instituciones privadas sin ánimo de lucro.
en cuanto a su nivel academico 
En este grupo se encuentran 1.937 instituciones educativas con un costo de matrícula cercano a 14 mil dólares, y con una inversión próxima a 7.3 miles de dólares.
En promedio estas instituciones educativas albergan 15 programas, y la mayoría 
son privadas sin ánimo de lucro. Dentro de estas predominan las carreras profesionales, y su máximo nivel educativo son los posgrados. 
</br>
<br>
<b>Cluster 3:</b>
En este grupo se encuentran 2097 instituciones educativas con un costo por matrícula cercano a 5 mil dólares, y con una inversión próxima a 4.6 mil dólares. En promedio estas instituciones educativas albergan 20 programas académicos, y son públicas o privadas con ánimo de lucro. Dentro de estas predominan los grados asociados, que a su vez son el nivel educativo más alto.
</br>
<br>
<b>Cluster 4:</b>
En este grupo se encuentran 610 instituciones educativas con un costo por matrícula cercano a 6.5 mil dólares, y con una inversión próxima a 8 mil dólares. En promedio estas instituciones educativas albergan 27 programas académicos, y son públicas. Dentro de estas predominan las carreras profesional, y su nivel educativo más alto son los posgrados
</br>
<br> </br>
         ")
  })
output$intro_<-renderUI({
  members_<-tags$div(
    tags$b('Authors:'),tags$br(),
    HTML(paste('&#9658' ,tags$a(href="https://www.linkedin.com/in/germ%C3%A1n-alonso-pati%C3%B1o-hurtado-828783241/", 
                                icon("linkedin"), "Germán Patiño", target="_blank"),
               'Estudiante de Estadística en la Universidad Nacional de Colombia.' ) ),
    tags$br(),
    HTML('&#9658 David Andres Cano Gonzalez Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.' ),
    tags$br(),
    HTML('&#9658 David Garcia Blandon Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.' ),
    tags$br(),
    HTML(paste('&#9658',tags$a(href='https://www.linkedin.com/in/juan-pablo-buitrago-diaz-5b960922b/',icon("linkedin"), 'Juan Pablo Buitrago Diaz', target="_blank"), 'Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia.') ),
  )
  HTML("<h2 style='text-align:center' > Introducción <h2/>
  <h5>Para este aplicativo se considero una base de datos que se encuentra en <a href= 'https://data.world/exercises/cluster-analysis-exercise-2' 
  target='_blank'> CollegeScorecard </a> que cuenta con información de 7804 universidades en 
  Estados Unidos con cerca de 1000 columnas, pero en este aplicativo solo se considero 7279, las universidades fueron segmentadas en 
  4 grupos donde el usuario y una selección de 23 columnas.<h5/>
  <h2 style='text-align:center'> Objetivo <h2/>
  <h5>Que el usuario tenga otra alternativa para encontrar una universidad que cumpla con sus espectativas y que se 
  ubique en un lugar que sea acorde a sus necesidades.<h5/>
  <h2 style='text-align:center'> ¿A quién va dirigido? <h2/>
<h5>  A personas interedas en buscar universidades que cumplan con sus expectativas, pueden observar
  la descripción de los 4 grupos e identificar cuales son de su interés, luego pueden ver la 
  ubicación geográfica de las universidades según los grupos y el estado donde más se sientan 
  comodos, ya sea por que busquen una universidad que cumpla con sus espectativas pero no a 
  una distancia tan lejana de donde residen, el usuario puede dar click en un punto del mapa y 
  obtener la URL de la página principal de dicha universidad.<h5>
<h2 style='text-align:center'> Video promocional <h2/>
 <iframe width='560' height='315' style='text-align:center' 
 src='https://www.youtube.com/embed/CqstGgo_E4c'
 title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>
        <br/>   
           <b >Miembros:</b>
   <h5> &#9658 <a href='https://www.linkedin.com/in/germ%C3%A1n-alonso-pati%C3%B1o-hurtado-828783241/' target='_blank'>
  <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
  Germán Patiño
</a> Estudiante de Estadística en la Universidad Nacional de Colombia.<h5/>
  <h5> &#9658 David Andres Cano Gonzalez Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>

  <h5> &#9658 David Garcia Blandon Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>
  <h5> &#9658 <a href='https://www.linkedin.com/in/juan-pablo-buitrago-diaz-5b960922b/' target='_blank'> 
  <i class='fab fa-linkedin' role='presentation' aria-label='linkedin icon'></i>
  Juan Pablo Buitrago Diaz
</a> Estudiante de ingenieria en sistemas en la Universidad Nacional de Colombia. <h5/>  
             " )  

}  )
  # mapa university 
  output$map_plot <- renderTmap({

      filtro<-is.element(datos_geo$CLUSTER,  input$cluster) | is.null(input$cluster)
  
      filtro2<-is.element(datos_geo$st_fips, input$estado ) | is.null(input$estado)
#    tmap_mode('view') %>%
      tm_shape(shp = datos_geo[filtro & filtro2,])+ # coordenadas lat long
      tm_dots(size = 0.05,col = "CLUSTER",popup.vars=c('address',var_numeric,var_cat)) })
  values<-reactiveValues(universidad=c(), url_1=c(),url_2=c())
    output$map_plot_state <- renderTmap({
      tm_shape(mapa_usa )+
      tm_polygons('Cluster_top',popup.vars= c("mean_TUITFTE" ,
                  "mean_INEXPFTE", "Cluster_1","Cluster_2","Cluster_3","Cluster_4"))})
    observeEvent(input$map_plot_marker_click,{
      click_<-input$map_plot_marker_click
      filtro<-paste('X',datos$OPEID,sep='')==click_$id
      uni_click<-datos[filtro,c('address','INSTURL')]
      filtro<-!is.element(values$universidad,uni_click[1]$address)
      values$universidad<-c(values$universidad[filtro], uni_click[1]$address)
      values$url_1<-c(values$url_1[filtro], uni_click[2]$INSTURL)

      print(values$universidad)
      output$urls<- renderUI({ 

                     contar<-3
                     n_<-length(values$universidad)
                     text_<-list(tags$b('Show last 6 university.'),tags$br())
                     indice<-n_
                     while(indice>0 & n_-indice<7  ){
                       text_[[contar]]<-tags$b( values$universidad[indice])
                       text_[[contar+1]]<-HTML(values$url_1[indice])
                       contar<-contar+2
                       indice<-indice-1
                     }
                     print(text_)
                     tagList(text_)
                   })
               })
  #,escape=1)

  output$torta<-renderPlotly({
    fig <- plot_ly(data=cluster_total, labels = ~CLUSTER, values = ~total, type = 'pie')
    fig <- fig %>% layout(title = 'Distribución de los cluster.',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$datos_<-DT::renderDataTable({
    datos}, options = list(scrollX = TRUE))
  
}
shinyApp(
  ui = usuario,

  server = servidor
)






  