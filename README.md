# CollegeScorecard
Segmentación de universidades en Estados Unidos de los datos abiertos en <a href= 'https://data.world/exercises/cluster-analysis-exercise-2' target='_blank'> CollegeScorecard </a>

En la siguiente carpeta se encuentra un análisis de clustering realizado a una base de datos con instituciones de educación superior de los Estados Unidos. El presente fue realizado con el fin de identificar grupos de instituciones educativas de características similares.

En la carpeta se encuentran los siguientes elementos:

- **CollegeScorecard.csv**: Es la base de datos adquirida <a href= 'https://data.world/exercises/cluster-analysis-exercise-2' target='_blank'> CollegeScorecard </a>  sin cambios.
- **CollegeScorecard_result.csv**:Es la base de datos producto con los cambios realizados.
- **map_states**: Se encuentran archivos tipo shape para la gráficación de estados 
- **readme.md**: Archivo con información general del proyecto. 	
- **app.R**: Es la página en la cual se muestran los resultados del análisis.
- **desarrollo_cluster.ipynb**: En este archivo se definió la creación de los clusters.
- **funciones.py**: Definición de funciones para la creación de los clusters.
- **manipulacion_geografica.R**: En este archivo se realizó la depuración de las coordenadas geográficas.

## Metodologia:

Las variables seleccionadas tratan de explicar el costo por estudiante, el nivel academico de la universidad, el tipo de universidad y el numero de programas que ofrece.
Se aplico componentes principales que explicaran al menos un 80% de variabilidad, se considero varios metodos de clustering pero el metodo seleccionado fue K-means. 

## Resultados:

Se obtienen 4 cluster donde cada cluster tiene diferentes caracteristicas y en el <a href= 'https://germanalonso.shinyapps.io/CollegeScorecard/'target='_blank'> aplicativo <a/> se logra apreciar las caracteristicas de cada cluster.
