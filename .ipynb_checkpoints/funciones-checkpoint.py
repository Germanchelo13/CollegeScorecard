# funciones 
import pandas as pd
import numpy as np
import seaborn as sns 
import sklearn.cluster 
import sklearn.decomposition
import matplotlib.pyplot as plt


def kmeans(datos_:pd.DataFrame,variables_:list ,n_:int,semilla:int  )->list:
    """
    Aplica el algoritmo
    datos: pd.DataFrame.
    variables: list Lista de variables que se incluyen kmeans (deben ser numericas).
    n_: int número de cluster.
    semilla: int semilla de aleatoriedad. 
    return: list de los segmentos, numerico.
    """
    X=datos_[variables_].copy()
    cluster_kmeans= sklearn.cluster.KMeans( n_clusters=n_, random_state=semilla).fit(X)
    segmentos_=cluster_kmeans.labels_

    return segmentos_

def spectral(datos_:pd.DataFrame, variables_:list, n_:int,semilla:int )->list:
    """
    Aplica la función EspectralClustering para la asignación de cluster.
    Aplica el algoritmo
    datos: pd.DataFrame.
    variables: list Lista de variables que se incluyen kmeans (deben ser numericas).
    n_: int número de cluster.
    semilla: int semilla de aleatoriedad. 
    return: list de los segmentos, numerico.  
    """
    X=datos_[variables_].copy()
    cluster_spectral=sklearn.cluster.SpectralClustering(
      n_clusters=n_,
      assign_labels='discretize',
      random_state=semilla).fit(X)
    segmentos_=cluster_spectral.labels_

    return segmentos_

def afinitypropagation(datos_:pd.DataFrame, variables_:list, damp:float(0.5),semilla:int)->np.array:
    """
    Aplica el algoritmo de AffinityPropagation.
    datos: pd.Dataframe
    variables: list lista de string con el nombre de variables usadas en el algoritmo.
    damp: float [0.5,1)
    semilla: int de la asignacion aleatoria. 
    return: np.array labels.
    """
    X=datos_[variables_].copy()
    cluster_afiniti_propagation= sklearn.cluster.AffinityPropagation(damping=damp,random_state=5).fit(X)
    segmentos_=cluster_afiniti_propagation.labels_
    return segmentos_

def estandarizacion(datos_:pd.DataFrame,variables_numeric:list  )->pd.DataFrame:
    """
    Estandariza las variables de un dataframe para que esten entre 0 y 1
    datos: pd.DataFrame.
    variables_numeric: Lista de variables que se quieren estandarizar 
    """
    datos_new=datos_.copy()
    valores_min=datos_new[variables_numeric].min(axis=0)
    valores_max=datos_new[variables_numeric].max(axis=0)
    datos_new[variables_numeric]=(datos_new[variables_numeric]- valores_min)/(valores_max-valores_min )

    return datos_new

def to_dummy(datos_:pd.DataFrame, variables_dumy:list)->pd.DataFrame:
    """
    Transoforma variables categoricas a dummy, solo las variables ingresadas.
    datos_: pd.DataFrame
    varaibles_dumy: list de variables categoricas que se van a crear transformar a dummy
    return: pd.DataFrame con todas las variables de datos y las variables_dumy remplezadas con la trasnformación.
    """
    for var in variables_dumy:
        dummy_temp=pd.get_dummies(datos_[var],prefix=var ).copy()
        columns_temp=dummy_temp.columns
        num_columns=len(columns_temp)
        datos_=pd.concat([datos_.drop(labels=var,axis=1),dummy_temp[columns_temp[0:num_columns-1]  ] ],axis=1)

    return datos_

def datos_to_pca(datos_:pd.DataFrame, variables_:list, prop_var:float)->pd.DataFrame:
    """
    Devuelve el numero de componentes que expliquen un prop_var*100% de variabilidad. 
    datos_: pd.DataFrame (se recomienda que esten estandarizados).
    variables_: list nombre de las variables que se les aplicara el PCA.
    prop_var: float proporción de variabilidad que se quiere explicar.
    return: pd.DataFrame con las componentes que explican al menos un % prop varianzas.
    """
    pca_=sklearn.decomposition.PCA(len(variables_))
    pca_.fit(datos_[variables_])
    varianza_acumulate=pd.Series(pca_.explained_variance_ratio_).cumsum()
    varianza_acumulate.index=varianza_acumulate.index+1
    num_componentes=varianza_acumulate[varianza_acumulate>=prop_var].index[0]
    varianza_acumulate.plot(xlabel='Número de componentes', ylabel='proporción de varibilidad acumulada',grid=True,marker='o')
    print('Las '+str(num_componentes)+' componentes explican un '
    +str(round(varianza_acumulate[num_componentes]*100,2))+
        '% de variabilidad de los datos esta función retorna las '+str(num_componentes)+'componentes')
    pca_final=sklearn.decomposition.PCA(num_componentes)
    pca_final.fit(datos_[variables_])

    return pd.DataFrame(pca_final.fit_transform(datos_[variables_]))

##############################################################################################################
###################################tablas comparativas#####################################################
##############################################################################################################
def numero_comas(numero: float) -> str:
    """
    A un numero entero de varios digitos le asigna un separador cada 3 digitos para ser mas visible
    Args:

    numero: float numero de cualquier cantidad de digitos.

    return: str del numero con las comas cada 3  digitos
    """
    numero_str_ = str(numero).split(".") # separar decimales
    numero_str=numero_str_[0] # extraer int
    dig = int(len(numero_str) / 3) # digitos cada 3
    if dig == len(numero_str) / 3: 
        dig = dig - 1
    for i in range(1, dig + 1):
        numero_str = (
            numero_str[0 : len(numero_str) - 4 * i + 1]
            + "."
            + numero_str[len(numero_str) - 4 * i + 1 :]
        )
    if len(numero_str_)>1:# concat decimales
        numero_str=numero_str+","+numero_str_[1]
    return numero_str


def contar_filas(datos:pd.DataFrame,var_agrupacion:str,value,var_comp:str,nombres_comp:list,na_column,redondeo:int)->list:
    """
    Cuenta el numero de filas junto con una proporción que hay por un valor en especifico de la variable 
    agrupación versus una variable categorica de comparación.
    
    Args: 
    datos: pd.DataFrame.
    var_agrupacion: str una variable categorica de datos de agrupación.
    value: Un valor unico de var_agrupacion.
    var_comp: str una variable categorica de los datos de comparación.
    nombres_comp: list lista de los unicos valores de var_comp.
    na_column: bool True si var_comp tiene valores NA, False sino (para no añadir a la tabla)
    redondeo: int entero para redondear.
    
    return: list con value, frecuencias y proporciones por nombres_comp, totales (NA si lo hay).
    """
    n=datos.shape[0] # total filas
    frecuencia=[] 
    if value=="Total": # sin filtro var_agrupacion
        datos_temp=datos[var_comp].copy()

    elif value=="NA": # poblacion NA
        datos_temp=datos.loc[pd.isna(datos[var_agrupacion]),var_comp].copy()
    else: # var_agrupacion= value
        datos_temp=datos.loc[datos[var_agrupacion]==value,var_comp].copy()        

    for i in nombres_comp: # por cada unico var_comp contar
        frecuencia.append(datos_temp[datos_temp==i].shape[0]) 
    if na_column==True:  # si var_comp tiene NA contarlos
        frecuencia.append(datos_temp[pd.isna(datos_temp)].shape[0])
    frec_total=[]
    total_=sum(frecuencia) # total por fila
    if total_!=0: # si total !=0
        for frec_0 in frecuencia: # por cada nombres_comp
            frec_total.append(numero_comas(frec_0) ) # add frec
            prop=round(frec_0/total_*100,redondeo)
            frec_total.append(numero_comas(prop)+"%" ) # add prop
        pro_total=numero_comas(round(total_/n*100,redondeo))+"%" # total
        frecuencia=[value,*frec_total,numero_comas(total_), pro_total] 
    else: # si total_==0 (NA) no se incluye
        frecuencia=False
    
    return frecuencia


def tabla_caso_categorico(datos:pd.DataFrame, var_agrupacion:str, var_comp:list,redondeo:int,order_var:bool)->pd.DataFrame:
    """
    Tabla pivot var_agrupacion vs var_comp confrecuencia y proporción acumulada horizontal.
    
    Args:
    datos: pd.DataFrame.
    var_agrupacion: str una variable categorica de datos de agrupación.
    var_comp: str una variable categorica de los datos de comparación.
    redondeo: int entero para redondear.
    order_var: bool True ordenar variables por frecuencia, False ordenar por nombre.
    
    return: pd.DataFrame var_agrupacion vs var_comp.
    """
    # orden por frecuencia
    nombres_agrupacion=list(datos[var_agrupacion].value_counts().index)# unicos agrupacion
    nombres_comp=list(datos[var_comp].value_counts().index) # unicos var_comp
    if order_var==False:  # orden por nombres
        nombres_agrupacion.sort(reverse=True)
        nombres_comp.sort(reverse=True)
    columnas=['Variables',var_comp,*list(np.repeat(" ",(len(nombres_comp)+1)*2-1 ))] #columnas
    nas_comp=datos[pd.isna(datos[var_comp])].shape[0] # total NA var_comp
    nombre_comp_prop=[] 
    for nombre in nombres_comp: # nombre (frec) y %
        nombre_comp_prop.append(nombre)
        nombre_comp_prop.append("Prop")
    # vars_comps=[var_agrupacion, *nombre_comp_prop]
    # primera fila 
    fila1=[var_agrupacion, *nombre_comp_prop,"Total","Prop"]
    na_column=False # no hay NA en var_comp 
    if nas_comp>0: # si hay NA entonces
        columnas=[*columnas," "," "] # añadir columnas
        # vars_comps=[*vars_comps,"NA","NA"] 
        # añadir columnas NA
        fila1=[var_agrupacion, *nombre_comp_prop,"NA","Prop","Total","Prop"]
        na_column=True # incluir columnas
    clases_=[*nombres_agrupacion,"NA","Total"] # posibles valores agrupacion
    # vars_comps=[*vars_comps,"Total","Total"]
    data_new=pd.DataFrame(columns=columnas) # creando data.frame
    data_new.loc[len(data_new)]=columnas # añadiendo columna
    data_new.loc[len(data_new)]=fila1 # añadiendo fila
    for value in clases_: # añadiendo frecuencia
        frecuencias=contar_filas(datos,var_agrupacion,value,var_comp,nombres_comp,na_column,redondeo)
        if frecuencias!=False: # si frecuencia no es nula
            data_new.loc[len(data_new)]=frecuencias # añadir 
        
    return data_new

def multi_tablas_categorico(datos:pd.DataFrame,
                            var_agrupacion:str, 
                            variables_comp:list,
                            redondeo:int,
                           order_var:bool):
    """
    Realiza multiples tablas en un pd.DataFrame de la funcion tabla_caso_categorico
    Args:
    datos: pd.DataFrame.
    var_agrupacion: str una variable categorica de datos de agrupación.
    variables_comp: list lista de variables categorica de los datos de comparación.
    redondeo: int entero para redondear.
    order_var: bool True ordenar variables por frecuencia, False ordenar por nombre.
    
    return: pd.DataFrame var_agrupacion vs cada variable de variables_comp.
    """
    num_max_columns=max(list(datos[variables_comp].nunique())) # maximo unicos por var_comp
    columnas_=list(np.repeat("-",num_max_columns*2+5 )) # columnas maximas para el dataframe
    datos_completa=pd.DataFrame(columns=columnas_) # creando dataframe
    for var_comp_ in variables_comp: # tabla_caso_categorico por cada variables_comp
        data_new_=tabla_caso_categorico(datos, var_agrupacion, var_comp_,redondeo,order_var)
        datos_completa.loc[len(datos_completa)]=columnas_ # añade "-" por fila
        for fila in data_new_.index: # añdiendo cada fila al dataframe final 
            filas_=columnas_.copy() 
            fila_new=list(data_new_.iloc[fila])
            filas_[0:len(fila_new)]=fila_new
            datos_completa.loc[len(datos_completa)]=filas_
    
    return datos_completa

def calculo_medidas(datos:pd.DataFrame,var_agrupacion:str ,value,var_comp:str,redondeo:int)->list:
    """
    Por un valor en especifico de var_agrupacion calcula estadisticos de var_comp
    
    Args: 
    datos: pd.DataFrame.
    var_agrupacion: str variable categorica de datos.
    value: un valor unico de var_agrupacion (Total: sin filtros, NA: los NA )
    var_comp: str nombre de una variable númerica.
    redondeo: int decimales que se quiere ver.
    
    return: list con value, frecuencia, quantiles, media, etc.
    """
    if value=="NA": # Para los NA de var_agrupacion 
        datos_temp=datos.loc[pd.isna(datos[var_agrupacion]),:].copy()
    elif value=="Total": # sin filtros
        datos_temp=datos.copy()
    else: # valor especifico de var_agrupacion
        datos_temp=datos.loc[datos[var_agrupacion]==value,:].copy()
    
    frec=datos_temp.shape[0]
    if frec==0:
        medidas_str=False # NA no tiene frecuencia no se incluye
    else:# para el value de var_agrupacion:
        minimo=datos_temp[var_comp].min() #minimo
        Q1=datos_temp[var_comp].quantile(0.25) # cuantiles
        Q2=datos_temp[var_comp].quantile(0.50)
        Q3=datos_temp[var_comp].quantile(0.75)
        ls=(Q3-Q1)*1.5+Q3 # limites para atipicos 
        li=Q1-(Q3-Q1)*1.5 # criterio quantiles
        maximo=datos_temp[var_comp].max() # mas 
        ati_sup=datos_temp[datos_temp[var_comp]>ls].shape[0] # atipicos superior
        ati_inf=datos_temp[datos_temp[var_comp]<li].shape[0] # atipicos inferior
        media=np.round(np.mean(datos_temp[var_comp]),redondeo) # media
        suma=datos_temp[var_comp].sum() # total
        var_=round(float(np.sqrt(datos_temp[var_comp].var()) ),redondeo) # desviacion
        medidas=[frec,minimo,Q1,Q2,Q3,maximo,ati_sup,ati_inf,media,suma,var_] 
        medidas_str=[value]
        for medida_ in medidas: # aplicando separadores de miles.
            medidas_str.append(numero_comas(round(medida_,redondeo) ))
    return medidas_str

def tabla_caso_numerico(datos:pd.DataFrame, var_agrupacion:str, var_comp:list,redondeo:int,order_var:bool)-> pd.DataFrame:
    """
    Por cada valor de var_agrupacion calcula estadísticos de cada variables de var_comp
    
    Args: 
    datos: pd.DataFrame.
    var_agrupacion: str nombre de una variable categorica de datos.
    var_comp: list lista de variables numericas de datos.
    rendondeo: int total decimales por numero.
    order_var: bool True para ordenar var_agrupacion por frecuencia, False por nombre.
    
    return: pd.DataFrame resumen estadístico de cada var_comp discriminada por var_agrupacion.
    """
    nombres_agrupacion=list(datos[var_agrupacion].value_counts().index) # unicos var_agrup
    if order_var==False:  # orden por nombres
        nombres_agrupacion.sort(reverse=True) 
    # Estadisticos de resumen 
    medidas_=[var_agrupacion,'frec',"min",'Q1','Q2','Q3','max','atipico sup','atipico inf','media',"sum",'sd']
    data_new=pd.DataFrame(columns=list(np.repeat(" ",len(medidas_) )) ) # creacion dataframe
    for variables_comp in var_comp: 
        columnas=[' ',variables_comp,*list(np.repeat(" ",len(medidas_)-2 )) ]
        data_new.loc[len(data_new)]=columnas # add nombre variable comp
        data_new.loc[len(data_new)]=[ *medidas_] # add medidas
        for value in [*nombres_agrupacion,"NA","Total"]: # medida por var_agrup
            med=calculo_medidas(datos,var_agrupacion,value,variables_comp,redondeo)
            if med!=False: # add values
                data_new.loc[len(data_new)]=med
        data_new.loc[len(data_new)]=list(np.repeat("----",len(medidas_) )) 
    return data_new



def tabla_dinamica(datos:pd.DataFrame, var_agrupacion:str, var_comp:list, tipo_var:bool,redondeo:int,order_var:bool):
    """
    Aplica multiples tablas de una variable de agrupación versus multiples variables (todas numericas o todas categoricas)
    aplicando las funciones tabla_caso_numerico o  multi_tablas_categorico según el caso
    Args: 
    datos: pd.DataFrame cuales quiera.
    var_agrupacion: str variable de agrupación para el index puede ser númerico o str.
    var_comp: list lista de variables que se quieren comparar (todas numericas o todas categoricas).
    tipo_var: bool True si las variables de var_comp es de tipo númerico, False si las variables son tipo categorico.
    redondeo: int digitos redondeo.
    order_var: True ordenar unicos var_agrupacion (var_comp son categoricas) por frecuencia, False orden alfabetico
    
    return: pd.DataFrame de las funciones  tabla_caso_numerico  o multi_tablas_categorico según sea el caso.
    """
    
    if tipo_var==True: # variables numericas
        data_new=tabla_caso_numerico(datos, var_agrupacion, var_comp,redondeo,order_var)
     
    elif tipo_var==False: # variables categoricas
        data_new=multi_tablas_categorico(datos, var_agrupacion, var_comp,redondeo,order_var)

    else: 
        print("tipo_var debe ser tipo bool.")
        
    return data_new