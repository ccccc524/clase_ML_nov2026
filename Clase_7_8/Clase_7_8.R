#######################################################################################
#######################################################################################
################      CLASE 7-8                     ###################################
################  MCD Carlos Castro Correa          ###################################
#######################################################################################
#######################################################################################

#Contenido: Reinforcement Learning

#Paquetes hasta el momento:
library(readr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rpart) #arboles de decisón
library(rpart.plot) #graficos de arbol de decision
library(readxl) #leer archivos de excel
library(xgboost) #algoritmo para paralelizar arboles de decision
library(tidymodels)
library(caret)
library(lubridate)#Paquete para lidiar con tiempos y fechas de procesamiento
library(rpart)
library(caret)


#######################################################################################
######## Unbalanced            ########################################################
#######################################################################################

#Semilla
set.seed(1035)

#Leemos los datos
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
unbalanced <- read_excel("unbalanced.xlsx")

#Observamos los datos
View(unbalanced)

#Cambiamos a factor
unbalanced$in_sf <- as.factor(unbalanced$in_sf)


#¿Cuántos registros tiene por cada categoría NY - SFO nuestra base unbalanced?
unbalanced$in_sf %>% table()

(unbalanced$in_sf %>% table()) / nrow(unbalanced)


#¿Cuál es el efecto de este desequilibrio en las predicciones?
#¿Tendrán la misma eficiencia para ambas clases?...

#Utilizamos un algoritmo de ML para probar...

#Columna auxiliar
unbalanced$ID <- 1:nrow(unbalanced)
#Estoy creando este ID para identificar los registros que utilizo como entrenamiento y prueba...

View(unbalanced) #Solo agregue un índice...

#Separamos el conjunto original en entrenamiento y prueba - considerando que no tengo que evitar filtraciones...
ent <- sample_frac(tbl = unbalanced,size = 0.8) #entrenamiento
pru <- unbalanced %>% filter(!ID %in% ent$ID) #prueba

#Reviso tamaños:
dim(ent)
dim(pru)

#¿Crees que el desbalance se herede a los conjuntos de entrenamiento y prueba?
ent$in_sf %>% table()
(ent$in_sf %>% table()) / nrow(ent)

pru$in_sf %>% table()
(pru$in_sf %>% table()) / nrow(pru)

#El de la base original era:
unbalanced$in_sf %>% table()
(unbalanced$in_sf %>% table()) / nrow(unbalanced)

#El desbalance se hereda porque.... la muestra fue aleatoria...

#Quitamos ID
ent <- ent %>% select(-ID)
pru <- pru %>% select(-ID)


#En conclusión: 
# 1.Tenemos mucho menos información de los deptos de NY... de los de SFO tenemos más
# 2.¿Qué podría suceder en las predicciones? ¿Qué resultados esperarías?...

#Veamos a continuación con un modelo de ML:


#Arbol de decisión y error de prueba...
#Modelo ML con datos de entrenamiento

#Corremos un árbol de decisión
tree_ent <- rpart(in_sf ~ ., data = ent) #Recuerda que ent, es el conjunto de entranmiento del conjunto desbalanceado...

#Predecimos en el conjunto de prueba (observa que newdata es pru, no ent... es decir, datos que no ha visto...)
pru$pred <- predict(object = tree_ent,newdata = pru,type = "class")

#Matriz de confusion prueba
conf_pru <- confusionMatrix(data=pru$pred, reference = pru$in_sf)

conf_pru$table #Observa que el accuracy no es "tan malo"... pero...

#De los registros que son de SFO, ¿cuántos son etiquetas correctamente?
24 / 26  

#De los registros que son de NY, ¿cuántos son etiquetas correctamente?
3 / 9

#La diferencia en predicciones correctas para cada clase es ABISMAL...
#y gran parte de debe al desequilibrio de clases...

#Incluso, el conjunto de prueba también esta desequilibrado:
pru$in_sf %>% table()

#¿Cómo podemos compensar este efecto? ¿Hay alguna posibilidad?...

#Por cierto, recuerda que las métricas anteriores se llaman Especifidad y Sensibilidad y nos 
#las entrega el comando confusionMatrix()

conf_pru$byClass

# Sensibilidad
conf_pru$byClass[1]
# Especifidad
conf_pru$byClass[2]




















#######################################################################################
######## Undersampling            #####################################################
#######################################################################################

#Utilizamos los mismos conjuntos de entrenamiento y prueba del ejemplo unbalanced

#Datos de entrenamiento
dim(ent) #140 registros

#Clases
ent$in_sf %>% table()

# 0   1 
# 31 109 

#Separo el conjunto de entrenamiento por clase:

#Observaciones por clase
ent_sfo <- filter(ent,in_sf == 1) #109 registros SF
dim(ent_sfo)

ent_ny <- filter(ent,in_sf == 0) #31 registros NY
dim(ent_ny)

#Tengo que hacer un nuevo conjunto de entrenamiento con 62 registros (31 SF y 31 NY)

#Con los 109 datos de SF0, hacemos clustering con 31 grupos...
model_31=kmeans(ent_sfo,31)

model_31

#En este caso, el withinss y el número de miembros por grupo no nos importan tanto...
#¿por qué?...

#Centroides
model_31$centers %>% View()
#Este es mi "nuevo conjunto" SFO


######Rediseño del conjunto de entrenamiento - 62 registros en total
ent_bis <- rbind(model_31$centers,ent_ny) #Pegamos los nuevos centroides (Nuevo datos de SF)
#y los datos de NY (el que tenía menos registros)

dim(ent_bis) #manipulado pero completamente balanceado
ent_bis$in_sf %>% table()

#Dimensiones
dim(ent) #original y desbalanceado
ent$in_sf %>% table()

#¿Cómo funcionan ahora nuestras predicciones?...

#####Modelo ML con datos undersampling (ent_bis) y 
#lo probamos con el conjunto de prueba original (pru)...

#########Diseñamos un nuevo árbol con los datos ent_bis
tree_ent_bis <- rpart(in_sf ~ ., data = ent_bis) #ent_bis ya está "eauilibrado"

#Predecimos en el conjunto de prueba original (eso no lo hemos manipulado)
pru$pred_bis <- predict(object = tree_ent_bis,newdata = pru,type = "class")

#Matriz de confusion prueba con ent_bis (con undersamplig)
conf_pru_bis <- confusionMatrix(data=pru$pred_bis, reference = pru$in_sf)

conf_pru_bis$table

#Matriz de confunsión original (sin datos undersampling...)
conf_pru$table

#¿Puedes notar la diferencia en la clase NY?...

#Sensibilidad
conf_pru$byClass[1] #Muy pocos registros de NY con los datos originales y desequilibrados...
conf_pru_bis$byClass[1] #El algoritmo predijo correctamente todos los registros de NY, con datos manipulados...

#Aclaración: Es un caso extremo con gran desempeño (no siempre es así...)


#¿Pero qué sucede con la clase SFO?...

#Especificidad
conf_pru$byClass[2]
conf_pru_bis$byClass[2] #Se redujo al manipular los datos, ¿por qué ocurre?
#Recuerda que pasamos de tener 109 registros a tener solo 31... con centroides de clústeres


#Entonces ¿qué podemos hacer?...

#1.Mezclar modelos
#2. Entrenar muchos algoritmos y medir
#3. Probar, probar y escoger la mejor opción 
#4. Modelos ensamble https://www.ibm.com/think/topics/ensemble-learning
#5. Programar de forma eficiente en memoria y aprovechar Docker y cómputo en la nube...
#https://www.docker.com/

#Mil opciones, ajustes y técnicas que nos podrían llevar todo un curso completo...o Maestría...











#######################################################################################
# Veamos el resto de los modelos de undersampling:

#Undersampling con reemplazo
#Observaciones por clase
ent_sfo <- filter(ent,in_sf == 1) #109 registros
dim(ent_sfo)

ent_ny <- filter(ent,in_sf == 0) #31 registros
dim(ent_ny)


#Con reemplazo
ent_sfo_bis <- sample_n(ent_sfo,size = 31) #Dame 31 registros de forma aleatoria, 
#cada registro puede ser seleccionado más de una vez...

######Rediseño del conjunto de entrenamiento
ent_bis <- rbind(ent_sfo_bis,ent_ny)

#Revisamos el nuevo tamaño:
dim(ent_bis) #(31 +31)


#########Diseñamos un nuevo árbol con los datos ent_bis
tree_ent_bis <- rpart(in_sf ~ ., data = ent_bis)

#Predecimos en el conjunto de prueba
pru$pred_bis <- predict(object = tree_ent_bis,newdata = pru,type = "class")

#Matriz de confusion prueba con ent_bis (con undersamplig)
conf_pru_bis_reemplazo <- confusionMatrix(data=pru$pred_bis, reference = pru$in_sf)
conf_pru_bis_reemplazo$table


#Resumen

#Normal (datos desequilibrados...)
conf_pru$table
#Undersampling con clustering
conf_pru_bis$table
#Undersampling sin reemplazo
conf_pru_bis_reemplazo$table

#Metricas resumen
mat <- data.frame(modelos = c("normal","under_clustering","under_sin"),
                  accuracy = c(conf_pru$overall[1], conf_pru_bis$overall[1],
                               conf_pru_bis_reemplazo$overall[1]))

#Observamos resultados accuracy por cada tipo de remuestreo:
mat

#¿Por qué es más bajo con clustering?... menos datos totales por undersampling...
#¿Pasa lo mismo con sensibilidad y especificidad?...













#######################################################################################
#######################################################################################
################     Aprendizaje por Refuerzo      ####################################
#######################################################################################
#######################################################################################


#Paquete nuevo a instalar
library(tidytext)
library(stringdist) 
library(bayesbio)




#######################################################################################
################     Multi-armed Bandit      

#Semilla aleatoria para nuestros experimentos:
set.seed(1035)

#Simulamos la máquina que vamos a utilizar 
#(Pensamos que hay 3 máquinas en el casino)
palanca <- sample_n(data.frame(palanca = 1:3),1)

palanca

#Importante
#Simulo la elección de palanca

  #Esto no es la recompensa solo el número de palanca que vamos a utilizar:

#Esta función se parece a simple_frac(), que utilizamos para separar conjuntos entrenamiento y prueba
sample_n(data.frame(palanca = 1:3),1) #Palanca seleccionada.. (seleccionando un registro al azar)

#Puedo repetir varias veces y selecciono una nueva palanca...
sample_n(data.frame(palanca = 1:3),1)

sample_n(data.frame(palanca = 1:3),1)

sample_n(data.frame(palanca = 1:3),1)


#Ahora sí...
#Recompensa de las palancas (cada máquina tiene un rendimiento distinto)

#MUY IMPORTANTE: En la vida real NO lo sabemos (sólo el casino lo conoce)...
#pero vamos a simular para crear un algoritmo...

#Podemos simular la ganancia de cada palanca como una poisson discreta
#Poisson https://en.wikipedia.org/wiki/Poisson_distribution

#Poisson
lambda <- c(1,5,8) #Ganancia de cada palanca...

lambda 
#Ganancia de cada palanca...
#Si sale la palanca 1, la ganancia sera una poisson con lambda = 1
#Si sale la palanca 2, la ganancia sera una poisson con lambda = 5
#Si sale la palanca 3, la ganancia sera una poisson con lambda = 8



#######################################################################################
#Ejemplo | Intentos individuales:

#Primer intento:
primer_palanca <- sample_n(data.frame(palanca = 1:3),1) #Elegimos la palanca
primer_palanca$palanca

#La recompensa obtenida en el primer intento:
rpois(1,lambda = primer_palanca$palanca)



#Segundo intento:
segunda_palanca <- sample_n(data.frame(palanca = 1:3),1) #Elegimos la palanca
segunda_palanca

#La recompensa obtenida en el segundo intento:
rpois(1,lambda = segunda_palanca$palanca)



#Tercer intento:
tercer_palanca <- sample_n(data.frame(palanca = 1:3),1) #Elegimos la palanca
tercer_palanca

#La recompensa obtenida en el segundo intento:
rpois(1,lambda = tercer_palanca$palanca)




#######################################################################################
#Ejemplo | Varios intentos SOBRE LA MISMA PALANCA:

#Ganancia al jugar 10 veces la palanca_seleccionada_10:
palanca_seleccionada_10 <- sample_n(data.frame(palanca = 1:3),1) %>% unlist()
palanca_seleccionada_10

win_10 <- rpois(10, lambda = lambda[palanca_seleccionada_10])
win_10

#Ganancia al jugar 100 veces la palanca_seleccionada_100:
palanca_seleccionada_100 <- sample_n(data.frame(palanca = 1:3),1) %>% unlist()
palanca_seleccionada_100

win_100 <- rpois(100, lambda = lambda[palanca_seleccionada_100])
win_100

#Ganancia al jugar 1000 veces la palanca_seleccionada_1000:
palanca_seleccionada_1000 <- sample_n(data.frame(palanca = 1:3),1) %>% unlist()
palanca_seleccionada_1000

win_1000 <- rpois(1000, lambda = lambda[palanca_seleccionada_100])
win_1000

#Palancas seleccionadas en cada ejercicio:
c(palanca_seleccionada_10,palanca_seleccionada_100,palanca_seleccionada_1000)

#Recordamos las ganancias de cada palanca
lambda

#¿Cambia mucho la ganancia promedio al aumentar el número de intentos?
c(mean(win_10),mean(win_100),mean(win_1000))

  #IMPORTANTE:
    #Esto ya lo sabíamos porque usamos una Poisson perooo en la vida real tendrías
    #que probar para intentarlo...

#Podríamos repetir muchas veces el experimento entre las 3 máquinas...



#Ahora, buscamos una mejor estrategia explorando palancia por palanca

#IMPORTANTE:
#NOTA: Recuerda que NO conocemos en realidad, la distribución de ganancias de cada palanca..
#es decir, el parámetro de las distribuciones sería una incógnita...

#Máquina 1
rpois(1, lambda = lambda[1])

rpois(1, lambda = lambda[1])

rpois(1, lambda = lambda[1])

#Podría repetir e intentar...



#Máquina 2 - intento más veces...
rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])

rpois(1, lambda = lambda[2])


#La máquina 2 parece mucho mejor....
#entonces: ¿Gasto todo mi dinero en la máquina 2?


#1. Quizás la máquina 1 tienen mejores rendimiento pero intente poco...
#2. Aún me falta explorar la máquina 3


#Máquina 3 - intento más veces... aún desconozco su rendimiento
rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

rpois(1, lambda = lambda[3])

#Podemos observar que:

#La máquina 1 me da menores rendimiento, sin embargo obtuve algunas ganancias altas...

#Las máquinas 2 y 3 parecen mejores pero también las probé más ocasiones

#Quizas la máquina 3 parece mejor, sin embargo, no debo descartar la máquina 2
#Por otro lado, conozco muy poco sobre la máquina 1; tal vez deba seguir explorándola eventualmente...


#Y RECUERDA IMPORTANTE: En la vida real, NO CONOZCO la distribución de las recompensas (Poisson).
#por lo que NECESITARIAS PROBAR para entender el rendimiento real...

#Pregunta: ¿Qué puedo utilizar para maximizar mi ganancia en cada intento?
#Algoritmo épsilon - miope...
















#######################################################################################
#######################################################################################
################     Algoritmo épsilon - miope            #############################
#######################################################################################
#######################################################################################

#Lambda
lambda <- c(1,3,10)

#Epsilón
eps <- 0.1

#Pregunta importante:
#¿Qué implica que eps tenga este valor?

#Matriz para ejercicio
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_7_8")
M <- read_csv("maquina_algoritmo.csv")

#Observamos la matriz, sus variables y componentes
View(M)

#Funcion ganancias
ganancias <- function(tasa){
  return(rpois(1, lambda = tasa))
}

#Ejemplo
ganancias(lambda[3]) #Ganancias de la palanca 3


#Iteracion inicial
inicial <- sample_n(tbl = data.frame(palanca = 1:3),size = 1)

#Elección inicial
inicial$palanca #Por consturcción, la selecciono aleatoria en la primera iteración

#Ganancia inicial
win_inicial <- ganancias(lambda[inicial$palanca])
win_inicial

#Registro mi ganancia en la primera entrada de la matriz

#Pedazo de matriz
aux <- filter(M,iteracion == 1)

View(aux)
#Aparte un pedacito de la matriz y luego la volveré a regresar...

#Las palancas no seleccionadas empiezan en 0
aux$veces_seleccionada[is.na(aux$veces_seleccionada)] <- 0
aux$suma_ganancia[is.na(aux$suma_ganancia)]           <- 0
aux$ganancia_promedio[is.na(aux$ganancia_promedio)]   <- 0

#Maquina seleccionada
inicial$palanca

#Registro variables
aux$veces_seleccionada[inicial$palanca] <- 1

View(aux)

#Ganancia obtenida
aux$suma_ganancia[inicial$palanca] <- win_inicial

View(aux)


#Ganancia promedio
aux$ganancia_promedio[inicial$palanca] <- aux$suma_ganancia[inicial$palanca] / aux$veces_seleccionada[inicial$palanca]


View(aux)

#Registramos el valor de aux en la matriz M
M[1:3,] <- aux


#Iteraciones posteriores
#2 a la 100

#i <- 2

for(i in 2:100){ #Repetimos 100 veces los pasos anteriores y los acumulamos
  
  #Matriz de la iteraccion actual
  aux <- filter(M,iteracion == i)
  
  aux
  
  
  #Matriz de la iteracion anterior
  aux_anterior <- filter(M,iteracion == (i-1))
  
  aux_anterior
  
  #maquina
  if(runif(1) < eps){
    
    #Explorar (probabilidad eps): palanca al azar
    maquina <- sample_n(tbl = data.frame(palanca = 1:3),size = 1)
    
  } else {
    
    #IMPORTANTE:
      #Esta es la implementación de épsilon-miope (utilizo EVIDENCIA ACUMULADA PARA EXPLOTAR)
    
    #Explotar (probabilidad 1 - eps): palanca con mayor ganancia promedio
    #hasta la iteración anterior; si hay empate, se elige al azar entre ellas
    prom    <- aux_anterior$ganancia_promedio
    mejores <- which(prom == max(prom))
    maquina <- data.frame(palanca = mejores[sample(length(mejores), 1)])
    
  }
  
  #Elección
  #maquina$palanca
  
  #Ganancia
  win <- ganancias(lambda[maquina$palanca])
  #win
  
  #Registro y actualizacion de valores
  
  #Suma ganancia (ganancia de la iteracion i)
  aux$suma_ganancia[maquina$palanca] <- win
  
  #Seleccionada
  aux$veces_seleccionada[maquina$palanca] <- 1
  
  #Lo sumo a la iteracion anterior y acumulado
  aux$suma_ganancia[maquina$palanca] <- aux$suma_ganancia[maquina$palanca] +
    aux_anterior$suma_ganancia[maquina$palanca]
  
  #Lo sumo a las veces seleccionada anterior y acumulados
  aux$veces_seleccionada[maquina$palanca] <- aux$veces_seleccionada[maquina$palanca] +
    aux_anterior$veces_seleccionada[maquina$palanca]
  
  #Ganancia promedio
  aux$ganancia_promedio[maquina$palanca] <- aux$suma_ganancia[maquina$palanca] /
    aux$veces_seleccionada[maquina$palanca]
  
  #Actualizo las demás entradas con la información previa
  aux[setdiff(1:3,maquina$palanca),3:5] <- aux_anterior[setdiff(1:3,maquina$palanca),3:5]
  
  #Actualizo la matriz M
  M[((1:3)+(3*(i-1))),] <- aux
  
}


#Resultado final
View(filter(M, iteracion == 100))

#Los resultados:
#¿tienen sentido con las recomensas de la palanca?
lambda

#Realidad:
filter(M, iteracion == 100) %>%
  select(ganancia_promedio)

#Ganancia total de todo el experimento
ganancia_total_ep <- filter(M, iteracion == 100) %>%
  select(suma_ganancia) %>% 
  unlist() %>% 
  sum()

#En todo el experimento gané:
ganancia_total_ep

#######################################################################################




#######################################################################################
#######################################################################################
#¿Cómo se comportaron las palancas para que obtuviera estas ganacias?


#Grafica de ganancias por palanca

#Maquina 1
p_1 <- data.frame(rend = filter(M,maquina == 1) %>% 
                    select(ganancia_promedio) %>%
                    unlist())

#Ganacias promedio de la máquina 1
View(p_1)

plot(p_1$rend)
lines(p_1$rend)



#Maquina 2
p_2 <- data.frame(rend = filter(M,maquina == 2) %>% 
                    select(ganancia_promedio) %>%
                    unlist())

#Ganacias promedio de la máquina 2
View(p_2)

plot(p_2$rend)
lines(p_2$rend)



#Maquina 3
p_3 <- data.frame(rend = filter(M,maquina == 3) %>% 
                    select(ganancia_promedio) %>%
                    unlist())

#Ganacias promedio de la máquina 3
View(p_3)

plot(p_3$rend)
lines(p_3$rend)


#Todas las lineas juntas
M %>%
  ggplot( aes(x=iteracion, y=ganancia_promedio, group=maquina, color=maquina)) +
  geom_line()

#Recordemos las recompensadas fijadas a-priori para el experimento:
lambda



#Preguntas importantes:

#1. Por construcción, ¿a qué valor tiende la ganancia promedio? ¿Coincide con el valor teórico esperado?

#2. ¿Qué puedes decir sobre la varianza en la ganancia promedio de cada palanca, segun la iteración?
#Importante: ¿Por qué la varianza es mayor en el inicio de las iteraciones de todas las palancas?...


#https://es.wikipedia.org/wiki/Teorema_del_l%C3%ADmite_central

#Además observa...
#¿Cuántas veces se utilizo cada palanca?
filter(M,iteracion == 100) %>% select(veces_seleccionada) 

#Proporción
filter(M,iteracion == 100) %>% select(veces_seleccionada)  / 100

#Observa el aprendizaje de nuestro algoritmo al detectar la mejor palanca...

#...
#...

#Pregunta:
  #¿Qué habría pasado si no utilizamos épsilon-miope? 
    #i.e. si utilizamos una elección al azar, sin actualizar estrategias con la evidencia






#######################################################################################
################     Algoritmo selección-aleatoria        #############################
#######################################################################################

#Lambda
lambda <- c(1,3,10) #Recuerda: son las recompensas de cada palanca...

#Epsilón
#eps <- 0.1 #Observa que no necesitamos una tasa de aprendizaje (selección de palanca)

#Pregunta importante:
  #¿Qué implica que eps tenga este valor?

#Matriz para ejercicio
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_7_8")
M <- read_csv("maquina_algoritmo.csv")

#Funcion ganancias
ganancias <- function(tasa){
  return(rpois(1, lambda = tasa))
}

#Ejemplo
ganancias(3) #Ganancias de la palanca 3


#Iteracion inicial
inicial <- sample_n(tbl = data.frame(palanca = 1:3),size = 1)

#Elección inicial
inicial$palanca #Por consutrcción, la selecciono aleatoria en la primera iteración

#Ganancia inicial
win_inicial <- ganancias(inicial$palanca)
win_inicial

#Registro mi ganancia en la primera entrada de la matriz

#Pedazo de matriz
aux <- filter(M,iteracion == 1)

#View(aux)
  #Aparte un pedacito de la matriz y luego la volveré a regresar...

#Maquina seleccionada
inicial$palanca

#Registro variables
aux$veces_seleccionada[inicial$palanca] <- 1

#View(aux)

#Ganancia obtenida
aux$suma_ganancia[inicial$palanca] <- win_inicial

#View(aux)


#Ganancia promedio
aux$ganancia_promedio[inicial$palanca] <- aux$suma_ganancia[inicial$palanca] / aux$veces_seleccionada[inicial$palanca]


#View(aux)

#Registramos el valor de aux en la matriz M
M[1:3,] <- aux


#IMPORANTE:
  #Observa que en adelante, NO HAY NINGUNA ACTUALIZACIÓN SOBRE LA MEJOR PALANCA...
    #LA EVIDENCIA DE QUE HAY MEJORES O PEORES PALANCAS NO ES UTILIZADA NUNCA:

#Iteraciones posteriores
#2 a la 100

#i <- 2

for(i in 2:100){ #Repetimos 100 veces los pasos anteriores y los acumulamos
  
  #Matriz de la iteraccion actual
  aux <- filter(M,iteracion == i)
  
  aux
  
  
  #Matriz de la iteracion anterior
  aux_anterior <- filter(M,iteracion == (i-1))
  
  aux_anterior
  
  #maquina
  maquina <- sample_n(tbl = data.frame(palanca = 1:3),size = 1)
  
  #Elección 
  #maquina$palanca
  
  #Ganancia inicial
  win <- ganancias(maquina$palanca)
  #win
  
  #Registro y actualizacion de valores
  
  #Suma ganancia (ganancia de la iteracion i)
  aux$suma_ganancia[maquina$palanca] <- win
  
  #Seleccionada
  aux$veces_seleccionada[maquina$palanca] <- 1
  
  #Lo sumo a la iteracion anterior y acumulado
  aux$suma_ganancia[maquina$palanca] <- aux$suma_ganancia[maquina$palanca] + 
    aux_anterior$suma_ganancia[maquina$palanca]
  
  #Lo sumo a las veces seleccionada anterior y acumulados
  aux$veces_seleccionada[maquina$palanca] <- aux$veces_seleccionada[maquina$palanca] +
    aux_anterior$veces_seleccionada[maquina$palanca]
  
  #Ganancia promedio
  aux$ganancia_promedio[maquina$palanca] <- aux$suma_ganancia[maquina$palanca] /
    aux$veces_seleccionada[maquina$palanca]
  
  #Actualizo las demás entradas con la información previa
  aux[setdiff(1:3,maquina$palanca),3:5] <- aux_anterior[setdiff(1:3,maquina$palanca),3:5]
  
  #Actualizo la matriz M
  M[((1:3)+(3*(i-1))),] <- aux
  
}

#Resultado final SIN-ÉPSILON MIOPE:
View(filter(M, iteracion == 100))


#Además observa...
#¿Cuántas veces se utilizo cada palanca?
filter(M,iteracion == 100) %>% select(veces_seleccionada) 

#Proporción
filter(M,iteracion == 100) %>% select(veces_seleccionada)  / 100



#######################################################################################
#De nueva cuenta, analizamos los resultados:

#Ganancia total de todo el experimento
ganancia_total_SIN_ep <- filter(M, iteracion == 100) %>%
  select(suma_ganancia) %>% 
  unlist() %>% 
  sum()

#En todo el experimento gané:
ganancia_total_SIN_ep

#Con épsilon-miope gané:
ganancia_total_ep

#######################################################################################
#######################################################################################
#######################################################################################























#######################################################################################
#######################################################################################
############   IA Generativa + NLP + Modelos de lenguaje    ###########################
#######################################################################################
#######################################################################################


#Contenido: IA Generativa + NLP + Modelos de lenguaje 

#Paquetes hasta el momento:
library(readr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rpart) #arboles de decisón
library(rpart.plot) #graficos de arbol de decision
library(readxl) #leer archivos de excel
#library(xgboost) #algoritmo para paralelizar arboles de decision
detach("package:xgboost", unload = TRUE) #Quitamos de la sesión a xgoboost pues tiene conflicto con la función slice()
library(tidymodels)
library(caret)
library(lubridate)#Paquete para lidiar con tiempos y fechas de procesamiento
library(rpart)
library(caret)

#Paquete nuevo a instalar
library(tidytext)
library(stringdist) 
library(bayesbio)





#Primero leemos los diversos n-gramas creados a partir de millones de artículos en español
#Datos
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_7_8")

#Caracteres a retirar de los textos
quitar <- read_csv("quitar.csv")

#Muestra de los articulos con los que fueron creados
sample <- read_csv("sample_news.csv")

sample$txt[1]

sample$txt[30]

sample$txt[99]



#######################################################################################
################      Limpieza                     ####################################

#Para mejorar la precision del análisis de texto podemos utilizar funciones para limpiar y organizar texto

normalizar <- function(texto, vocab = NULL){
  # minúsculas
  texto <- tolower(texto)
  # varios ajustes
  texto <- gsub("\\s+", " ", texto)
  texto <- gsub("\\.[^0-9]", " _punto_ ", texto)
  texto <- gsub(" _s_ $", "", texto)
  texto <- gsub("\\.", " _punto_ ", texto)
  texto <- gsub("[«»¡!¿?-]", "", texto) 
  texto <- gsub(";", " _punto_coma_ ", texto) 
  texto <- gsub("\\:", " _dos_puntos_ ", texto) 
  texto <- gsub("\\,[^0-9]", " _coma_ ", texto)
  texto <- gsub("\\s+", " ", texto)
  texto
}

#Ejemplo limpieza
corpus <- c("Lista de las comidas: pollo en chile, frijoles, hummus; 
                 no es posible incluir comida japones. No es verdad, 
                 todo lo que estás dicienciendo .
                 El máximo de participantes de las competencias es 10, 
                 no es posible iniciar con menos de 5 jugadores",
            "Finalmente; este es otro texto en donde; 
                 se usa indecuadamente punto y coma " )

corpus

#Aplicamos normalización de texto
normalizar(corpus)


#Para crear algunos n-gramas con el corpus podemos utilizar el siguiente proceso

ejemplo <- tibble(txt = corpus) %>%
  mutate(id = row_number()) %>%
  mutate(txt = normalizar(txt)) 

#Bi-gramas
bigrams_ejemplo <- ejemplo %>% 
  unnest_tokens(bigramas, txt, token = "ngrams", 
                n = 2) %>%
  group_by(bigramas) %>% tally() %>% 
  arrange(desc(n)) %>% View()

#¿En donde aparece "no es"?

#¿En donde aparece "de las"?

#¿En donde aparece "es posible"?

#¿Qué sucede con el resto de los bigramas?...




#La función "normalizar" es aplicada a la base de datos del periodico y creamos los siguientes conjuntos:

#######################################################################################
################      n-gramas                     ####################################

#Unigrama
mod_uni <- read_csv("unigramas.csv")

#Bigrama
mod_bi <- read_csv("bigramas.csv")

#Trigrama
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2025/Clase6") #Demasiado pesado, lo dejo en carpeta anterior
mod_tri <- read_csv("trigramas.csv")


#Observa el tamaño de nuestros n-gramas:

dim(mod_uni)

dim(mod_bi)

dim(mod_tri)


#Observa algunos de los registros

View(mod_uni)

View(mod_bi)

View(mod_tri)


#¿Cómo podemos hacer que una máquina produzca texto?...

#######################################################################################
################      Modelos de lenguaje                     #########################


#Los resultados de este modelo se basan en los resultados de los bi-gramas

#Palabra inicial
palabra <- "abogados"
#Frase construida a partir de la suceción de diversas palabras
frase <- palabra #Al inicio la frase y palabra son la misma (solo tenemos una)

#¿Cómmo funciona nuestra función para construir frases?...

#En el modelo de bi-grama buscamos en donde ocurrió la palabra...
#y ordenamos para encontrar la palabra más probable que le sigue..

#Nota: sobre los bi-gramas que acabamos de crear (entrenar), se buscan estas coincidencias:

mod_bi %>%  
  filter(!w_n_1 %in% quitar$quitar) %>%
  filter(!w_n_0 %in% quitar$quitar) %>% 
  filter(w_n_1 == palabra) %>% 
  arrange(desc(num)) %>% View()
#Nuestro modelo va a asignar la siguiente palabra con más observaciones...


#Nota: Utilizamos una función para construir frases:

pred_chat <- function(palabra,frase){
  sig <- mod_bi %>%  #En el modelo de bi-grama buscamos en donde ocurrió la palabra...
    filter(!w_n_1 %in% quitar$quitar) %>%
    filter(!w_n_0 %in% quitar$quitar) %>%
    filter(w_n_1 == palabra) %>% 
    arrange(desc(num)) %>% 
    select(w_n_0) %>% slice(1)
  #Pegamos para la frase
  frase <- paste(frase,sig$w_n_0,sep=' ')
  return(list(frase,sig))
}

#Al inicio palabra y frase son las mismas
# ¿Por qué?
palabra

frase

pal <- pred_chat(palabra,frase)

pal #lista con 2 salidas

#Frase
pal[[1]][1]

#sig
pal[[2]][1]

#Pregunta:
#¿Qué está pasando realmente?
#¿Existe un fundamento matemático para constuir frases de esta forma?...


#De todas las posibles frases, está eligiendo la más probable...


















#Regresamos a las notas...

#Creador de frases
#Utilizamos iterativamente esta función para crear frase con longitud 3
palabra <- "abogados"
frase <- palabra

#Construcción inicial
#Longitud 3
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:1){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]

#Ahora toma la última palabra generada y entra de nuevo en el algoritmo:
pal[[2]][1]


#Longitud 4
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:2){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]

#Ahora toma la última palabra generada y entra de nuevo en el algoritmo:
pal[[2]][1]



#Longitud 5
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:3){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]

#Ahora toma la última palabra generada y entra de nuevo en el algoritmo:
pal[[2]][1]


#Longitud 6
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:4){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]

#Ahora toma la última palabra generada y entra de nuevo en el algoritmo:
pal[[2]][1]



#Longitud 7
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:5){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]



#Longitud 8
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:6){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#Longitud 9
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:7){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#Longitud 10
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:8){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#¿Que va a pasar con la siguiente iteración?....









#Longitud 11
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:9){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#Longitud 12
pal <- pred_chat("abogados","abogados") #Primero va la palabra y luego la frase
for(i in 1:10){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#...
#...
#Además, ¿cómo funciona con otras palabras?

pal <- pred_chat("hola","hola") #Primero va la palabra y luego la frase
for(i in 1:8){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]



#Además, ¿cómo funciona con otras palabras?
pal <- pred_chat("griegos","griegos") #Primero va la palabra y luego la frase
for(i in 1:8){
  pal <- pred_chat(as.character(pal[[2]][1]),as.character(pal[[1]][1]))
}
#Frase
pal[[1]][1]


#Preguntas:
#¿Qué esta sucediendo con nuestro modelo?
#Gran pregunta: ¿Cómo podrías mejorarlo?...





















#######################################################################################
################      Modelos con temperatura (random)      ###########################


#Modelo random
palabra <- "deporte"
frase <- palabra
temperatura <-0.8 #valor entre 0 y 1 (sin tocar los extremos)

#Ajustamos la funcion pred_chat para asignar aleatoriamente entre cierto número de registros
pred_chat_temperatura <- function(palabra,frase,temperatura){
  sig <- mod_bi %>%
    filter(!w_n_1 %in% quitar$quitar) %>%
    filter(!w_n_0 %in% quitar$quitar) %>%
    filter(w_n_1 == palabra) %>% 
    arrange(desc(num)) 
  #Matriz auxiliar para muestreo
  sig_bis <- sig %>% arrange((num))
  
  #Nota el ajuste en TEMPERATURA
  
  #Siguiente palabra con Temperatura
  sig_bis <- sig_bis[1:floor(nrow(sig)*(1-temperatura)),"w_n_0"] %>% 
    sample_n(size = 1)
  #Pegamos para la frase
  frase <- paste(frase,sig_bis$w_n_0,sep=' ')
  return(list(frase,sig_bis$w_n_0))
}



palabra <- "abogados"
frase <- palabra

#Longitud 3
pal <- pred_chat_temperatura("abogados","abogados",temperatura) #Primero va la palabra y luego la frase
for(i in 1:1){
  pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
}
#Frase
pal[[1]][1]



###Además cada vez que llamamos la función pred_chat_temperatura()
#Entrega un resultado distintos y con mayor coherencia...

#Longitud 2
for(k in 1:10){
  pal <- pred_chat_temperatura("abogados","abogados",temperatura) #Primero va la palabra y luego la frase
  for(i in 1:1){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
    print(pal[[1]][1])
  }
}


#Aumentamos la longitud
#Longitud 6
for(k in 1:10){
  pal <- pred_chat_temperatura("abogados","abogados",temperatura) #Primero va la palabra y luego la frase
  for(i in 1:5){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
  }
  print(pal[[1]][1])
}



#Aumentamos aún más la longitud
for(k in 1:15){
  pal <- pred_chat_temperatura("abogados","abogados",temperatura) #Primero va la palabra y luego la frase
  for(i in 1:5){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
  }
  print(pal[[1]][1])
}




#Cambiamos la temperatura

palabra <- "abogados"
frase <- palabra
temperatura <-0.8 #valor entre 0 y 1 (sin tocar los extremos)

for(k in 1:10){
  pal <- pred_chat_temperatura(palabra,frase,temperatura) #Primero va la palabra y luego la frase
  for(i in 1:5){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
  }
  print(pal[[1]][1])
}


#Temperatura 0.3
palabra <- "abogados"
frase <- palabra
temperatura <-0.3 #valor entre 0 y 1 (sin tocar los extremos)

for(k in 1:10){
  pal <- pred_chat_temperatura(palabra,frase,temperatura) #Primero va la palabra y luego la frase
  for(i in 1:5){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
  }
  print(pal[[1]][1])
}



#Temperatura 0.9
palabra <- "abogados"
frase <- palabra
temperatura <-0.9 #valor entre 0 y 1 (sin tocar los extremos)

for(k in 1:10){
  pal <- pred_chat_temperatura(palabra,frase,temperatura) #Primero va la palabra y luego la frase
  for(i in 1:5){
    pal <- pred_chat_temperatura(as.character(pal[[2]][1]),as.character(pal[[1]][1]),temperatura)
  }
  print(pal[[1]][1])
}

#Pregunta:
  #¿Alguien utilizó las primeras versión de ChatGPT?... 
      #producía un texto con un nivel de coherencia similar a este ejemplo...

#Notemos que:
#Entre más longitud, menos coherencia (¿por qué?)
#¿Cómo usarías la noción de conjunto de entrenamiento para explicar este hecho?

#El modelo con temperatura (random) funciona mejor mediante una decisión que no parecía evidente

#Importancia de lo aleatorio
#Ejemplo: descenso en gradiente
#Por ejmplo, para optimizar funciones (PARÁMETROS ÓPTIMOS EN ML)...
#https://es.wikipedia.org/wiki/Descenso_del_gradiente









#######################################################################################
#######################################################################################
################                    NLP              ##################################
#######################################################################################
#######################################################################################

library(tm)
library(topicmodels)


#######################################################################################
################      Índice de Jaccard       #########################################

#Oraciones del ejemplo
String_A <- c("Hola","lo","siento","pero","no","puedo","asistir","hoy")

String_B <- c("Hola","lo","entiendo","pero","no","puedo","escuchar","hoy")

#Cadenas de texto
String_A

String_B


#Intersección - como notación de conjuntos
intersect(String_A,String_B) 

intersect(String_A,String_B) %>% length() 


#Union - como notación de conjuntos
union(String_A,String_B) 

union(String_A,String_B) %>% length()


#Índice de Jaccard
(intersect(String_A,String_B) %>% length() )  /  (union(String_A,String_B) %>% length())


#Alternativa
#También podemos utilizar una función del paquete "bayesbio"
jaccardSets(String_A, String_B) 













#######################################################################################
#######################################################################################
################    #Latent Dirichlet Allocation      #################################
#######################################################################################
#######################################################################################

#Leemos una base original de artículos
# ag <- read_csv(
#   "https://raw.githubusercontent.com/mhjabreel/CharCnn_Keras/master/data/ag_news_csv/train.csv",
#   col_names = c("class","title","description")
# )
# 
# #Observa los datos en el enlace:
# #https://raw.githubusercontent.com/mhjabreel/CharCnn_Keras/master/data/ag_news_csv/train.csv
# 
# acq <- sample_n(tbl = ag,size = 1000) %>%
#   select(description)

#Mejor cargamos una base para que la muestra no cambie
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_7_8")
acq <- read_csv("muestra.csv")

#Observamos los datos
View(acq)

#Únicamente tomamos una m.a.
dim(acq)



#Formato:
#Convertir a funcion corpus
acq <- VCorpus(VectorSource(acq$description))

inspect(acq[[1]])

inspect(acq[[2]])

inspect(acq[[20]])

inspect(acq[[24]])

inspect(acq[[40]])

inspect(acq[[44]])

inspect(acq[[49]])


#Guardamos todos estos datos en la variable corpus
corpus <- acq

#Mismos datos
inspect(corpus[[1]])


#######################################################################################
#Transformación de datos
#Raecuerda: el análisis de texto funciona mejor cuando la información esta "limpia y estandarizada"

#Convierto a minúsculas
corpus <- tm_map(corpus, content_transformer(tolower))
inspect(corpus[[1]])

#Remuevo todos los signos de puntuacion:
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[[1]])

#Remuevo los números
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[[1]])

#Remuevo las "stop words"
#¿Qué es una stop word?... palabras atípicamente frecuentes que pueden sesgar los resultados
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[[1]])

#Acomodo el texto
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[[1]])

#Creo una matriz de "términos" para el modelo LDA
dtm <- DocumentTermMatrix(corpus)

dtm


#Tamaño de la matriz
dim(dtm)


#Revisamos las entradas de la matriz:
inspect(dtm[1:20, 1:200]) %>% View()
#¿Qué está pasando?...

#Por cada palabra única, indica con 0 ó 1 si esa noticia contiene dicha palabra o no...


#Convertimos la matriz de términos a formato matriz
m <- as.matrix(dtm)

#Observa la matriz y ordena algunas columnas
View(m)

#Palabras de la primer noticia
m[1, m[1, ] > 0] %>% sort()


#Palabras de la segunda noticia
m[2, m[2, ] > 0] %>% sort()

    #Pregunta, con la evidencia empírica, ¿de qué crees que trata esta segunda noticia?...


#######################################################################################
################          Algoritmo LDA               #################################
#######################################################################################


#Semilla aleatoria
set.seed(1035)

#Corremos el algoritmo Latent Dirichlet Allocation
#y pedimos que agrupe las noticias en 3 conglomerados:
lda <- LDA(dtm, k = 3,
           control = list(seed = 1035))

#Objeto "poco" informativo
lda

#Temas y términos frecuentes en las noticias de cada grupo:
terms(lda, 10)

#Distribuciones posterior de los términos:
theta <- posterior(lda)$topics
posterior(lda)$topics %>% View()

#Nota: ver en Excel...
#Heatmap matricial...

#Asignamos el tema o topic al que pertenece cada noticia:
tema <- apply(theta, 1, which.max)

tema 
    #Observa con atención el paso anterior: de acuerdo a las distribuciones posteriores, se asigna el tema del que trata...

#Convertimos nuevamente a data.frame el objeto de noticias:
acq_df <- data.frame(
  description = sapply(acq, function(x) x$content),
  stringsAsFactors = FALSE
)

#Asignamos cada noticia a un topic o tema
acq_df$tema <- tema

View(acq_df)

#Número de noticias por topic o tema:
table(acq_df$tema) #Grupos equilibrados...


#Algunas noticias del grupo 1:
acq_df %>% 
  filter(tema == "1") %>% 
  sample_n(10) %>% 
  select(description)


#Algunas noticias del grupo 2:
acq_df %>% 
  filter(tema == "2") %>% 
  sample_n(10) %>% 
  select(description)


#Algunas noticias del grupo 3:
acq_df %>% 
  filter(tema == "3") %>% 
  sample_n(10) %>% 
  select(description) 

#¿Te hace sentido la agrupacion de los temas?
#Probablemente 


#Pregunta adicional:
#¿Observas alguna falla en la aplicación del algoritmo LDA?
#¿Qué podríamos agregar?

#¿Qué puedes observar en los parámetros de la función LDA?

#Pregunta importante: ¿Qué tipo de aprendizaje utiliza el algoritmo LDA?
lda <- LDA(dtm, k = 3,
           control = list(seed = 1035))

#Pregunta:
  #1. ¿Observas alguna similitud respecto a clustering?
  #2. ¿Consideras que estamos haciendo algo como one-hot-encoding y distancia euclidiana?  :((((

      #Para responder: lee el siguiente paper: 
        #https://web.stanford.edu/class/linguist289/lda.pdf
        
    


#Análisis alternativo:

#Dimensión de la matriz:
dim(m)

View(m) #Es una matriz con muchos ceros...

#Registros totales igual a cero
sum(m == 0)

#Porcentaje
sum(m == 0) / (nrow(m)*ncol(m)) #Matriz rala...

#Ya lo sabíamos desde que convertirmos los datos originales a una matriz de términos:
dtm


#¿Qué otro algoritmo de ML podrías utilizar para agrupar las noticias en temas?...







#######################################################################################################
#######################################################################################################
### Supongamos que después de un minucioso análisis... alguien decide usar k-means (ML no supervisado)

  #Por supuesto, despúes de leer los paper sobre DLA (hay que estar seguro y entender antes de aplicar cualquier herramienta...)


set.seed(1035)
#Clustering por k-medias con 3 grupos
cluster_3 <- kmeans(m,3)

#Asignamos cada noticia a un grupo
cluster_3$cluster

#Withinss por grupo
cluster_3$withinss

#Whithinss total
cluster_3$tot.withinss

#Ahora, renombramos para reciclar el código de la clase anterior:
datos <- m

#Hacemos distintos cluster para obtener gráfica de codo:

#Cluster con 1 grupos
model_1=kmeans(datos,1)

#Cluster con 2 grupos
model_2=kmeans(datos,2)

#Cluster con 3 grupos
model_3=kmeans(datos,3)

#Cluster con 4 grupos
model_4=kmeans(datos,4)

#Cluster con 5 grupos
model_5=kmeans(datos,5)

#Cluster con 6 grupos
model_6=kmeans(datos,6)

#Cluster con 7 grupos
model_7=kmeans(datos,7)

#Matriz vacía para almacenar los resultados
mat <- matrix(0,7,2) %>% data.frame()
colnames(mat) <- c("cluster","withinss")

#Simplemente lleno mi matriz con los el withinss total de cada modelo
mat$cluster <- c(1,2,3,4,5,6,7)
mat$withinss <- c(model_1$tot.withinss,model_2$tot.withinss,model_3$tot.withinss,model_4$tot.withinss,
                  model_5$tot.withinss,model_6$tot.withinss,model_7$tot.withinss)

#Revisamos la matriz
View(mat)

#Grafica de codo
plot(mat$withinss)
lines(mat$withinss)

#¿Cuál es el número de clusteres o grupos óptimo?


### Pregunta final:
#¿Qué efecto tiene aumentar el número de clústeres o grupos?

table(model_1$cluster)

table(model_2$cluster)

table(model_3$cluster)

table(model_4$cluster)

table(model_5$cluster)

table(model_6$cluster)

#¿Crees que el clustering sea el modelo adecuado para enfrentar este tipo de problemas?
#¿Por qué?
  #Meditalo desde el punto de vista geométrico...


#Desde el punto de vista geométrico - matemático, ¿qué está ocurriendo?...
colnames(m)


#Tomemos 3 variables (justo cuando tomamos el ejemplo de iris)...
#chains,captain,baltimore
data <- m[,c("chains", "captain", "baltimore")] %>% 
  data.frame()

View(data)

dim(data)

#Gráficamos en 3D:
plot_ly( 
  data = data,
  x = ~chains, y = ~captain, z = ~baltimore, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = "royalblue", size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "chains"),
    yaxis = list(title = "captain"),
    zaxis = list(title = "baltimore")))

#¿Cuántos registros tienen estas palabras?...

#Pregunta:
#¿En dónde se concentran la mayoría de los registros?

#Registros con chains
which((data[,"chains"] == 1) == "TRUE")

which((data[,"chains"] == 1) == "TRUE") %>%
  length()

#Registros con captain
which((data[,"captain"] == 1) == "TRUE")

which((data[,"captain"] == 1) == "TRUE") %>%
  length()

#Registros con baltimore
which((data[,"baltimore"] == 1) == "TRUE")

which((data[,"baltimore"] == 1) == "TRUE") %>%
  length()


#Los datos quedan sumamente esparcidos (espacialmente hablando)...
  #Porque hay muchas palabras UNICAS, que aparecen POCAS veces en los registros...

#IMPORTANTE:
#Además empieza a crear grupos de una observacion 

#Y en algunos cluster, crear grupo de menos observaciones, por ejemplo 10,8, entre otros:

table(model_3$cluster)

table(model_4$cluster)

table(model_5$cluster)

table(model_6$cluster)

table(model_7$cluster)

#El model_7 crea el grupo 5  con 6 observaciones...
table(model_7$cluster)

lista <- which(model_7$cluster == "5") %>% unique()
lista

acq_df[lista,1]

#¿Qué tienen en común estos datos?
#Observemos....

#¿No queda claro? Quizás preguntando cuál es la característica común a un LLM...
#que tema o palabra en comun tienen los sig noticias:

acq_df[lista[3],]

acq_df[lista[6],]

acq_df[lista[4],]

#Observa que la clasificación anterior, la tenía que realizar un humano descartando temas....
#Aunque para esos casos, LDA funciona mejor...

#Lo demás puede ser ruido... aunque podríamos quitar algunos conceptos o stopwords adicionales...


#Nota: por esta razón, en este curos analizamos el fundamento geométrico-matemático de los algoritmos...


#Pregunta final:

  #Al parecer aplicando k-means podríamos ir identificando textos con TEMAS SIMILARES, pero...
    #¿Es correcto que los grupos sean tan pequeños?
    #Desde el punto de vista práctico (Cluster para tipos de clientes), ¿tiene sentido tener muchos grupos muy pequeños? ¿Por qué?
    #¿Cómo podrías resolver un problema de estos en la actualidad?...






