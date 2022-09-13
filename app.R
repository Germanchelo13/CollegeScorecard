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
mapa_usa<-st_read('map_states/geoBoundaries-USA-ADM1_simplified.shp')
mapa_usa<-mapa_usa %>%
  st_set_crs(value = 4326) %>%
  st_transform(crs = 3857)
datos<- read.csv('CollegeScorecard_result.csv')
datos<-datos[,names(datos)[2:length(names(datos))]]
datos$address<-paste(datos$INSTNM,  # direccion 
               datos$CITY,
               datos$STABBR,
               datos$ZIP, sep=", ")
diccionario<-list('TUITFTE'=c('average cost of tuition per student.'),
                  'INEXPFTE'=c('Instructional expenses divided by the number of
                               full-time students.',
                               'NUM_PROGRAM'=c('Number of program ')) )
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
mapa_puntos<- datos_geo %>%
  st_intersection(mapa_usa)
prueba<-mapa_puntos %>% group_by(shapeName ) %>% summarise( conteo=length(unique(STABBR)) )
cluster_total<-datos%>% 
  group_by(CLUSTER)%>%
  summarise(total=n())
usuario<- fluidPage(
  dashboardPage(
    # header princial
    dashboardHeader(title="University EEUU.",
                    titleWidth = 400, 
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
        menuItem(text= "Visualization",tabName = "viz",icon = icon("chart-line")),
        menuItem(text= "Geo loc",tabName = "map",icon = icon("earth-americas")),
        menuItem("Members", tabName="miembros", icon=icon("users"))
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
                                                                                       choices =unique(datos$STABBR),multiple = TRUE) ) ),
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
      tabItem(tabName = 'viz',
              tabBox(width= 12,tabPanel('Numeric',icon=icon('chart-scatter'),
                              fluidPage( 
                                fluidRow( br(), 
                                          selectInput(inputId ="var_numeric" , label = "Select variable"
                                                      , choices = var_numeric ),
                                          uiOutput('descripcion_numerica'),
                                          plotlyOutput('boxplot_comp'), br(),br()

                                ) 
                                )
                              ),
                     tabPanel('Scatter',
                              fluidPage(
                                fluidRow(
                                  fluidRow(column(6,
                                                  selectInput(inputId ="var_numeric_1" , label = "Select var"
                                                              , choices = var_numeric )),
                                           column(6,
                                                  selectInput(inputId ="var_numeric_2" , label = "Select var"
                                                              , choices = var_numeric )) ),
                                  uiOutput('descripcion_numerica_2'),
                                  plotlyOutput('scatter_comp')
                                )
                              )
                              ),
                     tabPanel('Qualitative',icon=icon('chart-column'),
                              fluidPage(
                                fluidRow( selectInput(inputId ="var_cat" , 
                                                      label = "Select variable"
                                                      , choices = var_cat ),
                                          uiOutput('descripcion_cate'),
                                          plotlyOutput('bar_comp')
                                          )
                              ))
                     
                     )),
      # item descripcion 
      tabItem(tabName = 'intro',fluidRow(style='height:5vh'),
              tabBox(tabPanel(title='Contexto',width=12,
                                                fluidPage(
                                                  fluidRow(uiOutput('intro_'),
                                                           plotlyOutput('torta_'))
                                                )),
                    tabPanel('Caracterización', fluidPage(fluidRow( 
                      uiOutput('caracterizacion'),
                      plotlyOutput('torta'))) ),
                    tabPanel('Video ',icon=icon('youtube'),fluidPage(
                      fluidRow(uiOutput('video_') )
                    )  ) )) ,
      tabItem(tabName="data",
              tabBox(id="t3",width= 12,
                     tabPanel(title="Data frame Universities",icon=icon('table'),
                              fluidPage(
                                fluidRow( DT::dataTableOutput('datos_') )
                              ))
                     
              )),
      tabItem(tabName = 'miembros',tabBox(tabPanel(title='Mermbers',icon=icon('users-rectangle'),
                                                fluidPage(
                                                  fluidRow(uiOutput('info'))
                                                )))) 
    )
    
    )
  )
)
servidor<-function(input, output) {
output$intro_<-renderUI({
  HTML("
  <h2>Analisis: Instituciones de educacion de Estados Unidos</h2>
  <p>Realizado por:<br>-David Andres Cano Gonzalez<br>-David Garcia Blandon<br>-German Alonso Patino Hurtado<br>-Juan Pablo Buitrago Diaz<br</p>


  <p>Mediante el presente, se prete realizar un analisis estadistico para el conjunto de instituciones educativas del departamento de educacion de Estados Unidos. Este se realiza con el fin de ayudar a padres, estudiantes y politicos con la toma de decisiones. Para esto, se propuso y realizo una agrupacion de las diferentes instituciones educativas, basandonos en diferentes aspectos y atributos de las mismas, y mediante la tecnica de clustering.</p>
<p>El objetivo del agrupamiento, fue el de identificar grupos de instituciones educativas de caracteristicas similares. Los datos fueron sacados de la base de datos  <a href= 'https://data.world/exercises/cluster-analysis-exercise-2' target='_blank'> CollegeScorecard </a> . 
Antes de realizar la tecnica de clustering, se necesito realizar una preparacion de los datos, ya que muchos tenian valores vacios o erroneos. De las 7804 universidades que se encontraron en la base de datos se trabajo solo con 7279. Luego se realizaron pruebas para verificar la pertinencia y eficacia del metodo. Luego se realizo el clustering, y por ultimo se analizaron los resultados.</p>
       ")
}  )
output$video_<- renderUI(
  HTML("<iframe width='420' height='315'
src='https://www.youtube.com/embed/tgbNymZ7vqY'>
</iframe>")
)
  output$info<-renderUI(  {
    tags$div(
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
  })
  # mapa university 
  output$map_plot <- renderTmap({

      filtro<-is.element(datos_geo$CLUSTER,  input$cluster) | is.null(input$cluster)
  
      filtro2<-is.element(datos_geo$STABBR, input$estado ) | is.null(input$estado)
#    tmap_mode('view') %>%
      tm_shape(shp = datos_geo[filtro & filtro2,])+ # coordenadas lat long
      tm_dots(size = 0.05,col = "CLUSTER") })
  values<-reactiveValues(universidad=c(), url_1=c(),url_2=c())
    output$map_plot_state <- renderTmap({
      tm_shape(mapa_usa )+
      tm_polygons()})
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
    output$descripcion_numerica<-renderUI({
      var_temp<-input$var_numeric
      HTML(paste('<b>',var_temp,'</b> :', diccionario[[var_temp]]),sep='' ) })
  output$boxplot_comp<- renderPlotly({
      plot_ly() %>%
      add_boxplot(x=datos[,'CLUSTER'], y=datos[,input$var_numeric],
                  color=datos[,'CLUSTER']  ) %>%
      layout(xaxis=list(title= 'Cluster' ),
             yaxis=list(title= input$var_numeric ),
             legend=list(title=list(text='Cluster')))
  })

  output$scatter_comp<- renderPlotly({
    plot_ly(data=datos, x=~get(input$var_numeric_1),
            y=~get(input$var_numeric_2),type='scatter',
            mode = 'markers',split =~CLUSTER  ) %>%
      layout(xaxis=list(title= input$var_numeric_1),
             yaxis=list(title= input$var_numeric_2),
             legend=list(title=list(text='Cluster'))
      )
      
  }) 
  output$bar_comp<-renderPlotly({
    datos_gruop<-datos %>% 
      group_by(CLUSTER,get(input$var_cat) ) %>%
      summarise(freq=n())
    datos_gruop$prop<-0
    for (i in cluster_total$CLUSTER){
      filtro<-datos_gruop$CLUSTER==i
      total_clusteri<-cluster_total$total[cluster_total$CLUSTER==i]
      datos_gruop[filtro,]$prop<-round(100*datos_gruop[filtro,]$freq/total_clusteri,2)
    }
    names(datos_gruop)[2]<-input$var_cat
    datos_gruop$CLUSTER<-factor(datos_gruop$CLUSTER)
    
    plot_ly(data=datos_gruop, x=~CLUSTER, y=~prop ,color=~get(input$var_cat),text=~freq,type='bar')%>%
      layout(yaxis = list(title = 'Percentaje'),legend=list(title='Tipo universidad'))
    
  } )
  output$torta<-renderPlotly({
    fig <- plot_ly(data=cluster_total, labels = ~CLUSTER, values = ~total, type = 'pie')
    fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  output$descripcion_numerica_2<- renderUI({
    HTML(paste('<b>',input$var_numeric_1,'</b> :', diccionario[[input$var_numeric_1]],
         '<br /> ','<b>',input$var_numeric_2,'</b> :', diccionario[[input$var_numeric_2]],sep='' ))
  })
  
  output$datos_<-DT::renderDataTable({
    datos}, options = list(scrollX = TRUE))
  
}
shinyApp(
  ui = usuario,

  server = servidor
)






  