#######################################################################################
#######################################################################################
################      CLASE 2                     ####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Contenido: Modelos ML clasificación y métricas

#Paquetes hasta el momento:
library(readr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)

#Paquetes necesarios para correr un árbol de decisión y graficarlo
  #Si no los tienes instalados, lo tienes que hacer antes de llamarlos con library()
library(rpart) #arboles de decisón
library(rpart.plot) #graficos de arbol de decision
library(readxl) #leer archivos de excel
library(xgboost) #algoeritmo para paralelizar arboles de decision
library(tidymodels)
library(caret)

#######################################################################################
################    Arból de clasificación          ###################################
#######################################################################################

#Colocamos la ruta en donde tengamos descargado el archivo de los depas, clase 2
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")

#Abrimos la base sobre deptos 
A <- read_excel("nyc_sfo_deptos.xlsx",1) #ya la cargamos en la memoria de R
  #Observa el ligero cambio respecto a read_csv()...

View(A)


#Nos quedamos sólo con ciertas variables
A <- A %>% 
  select(in_sf:elevation)

View(A)

#Con el comando colnames() puedes ver el nombre de las columnas de la matriz que acabas de cargar
colnames(A)

#¿Cómo se llama la columna que queremos clasificar?...
#...
A$in_sf

#¿Qué tipo de variable es?
A$in_sf %>% class()



#Convertimos la etiqueta a predecir en factor 
  #Nota: (esto lo requiere la función de árbol que vamos a utilizar)
A$in_sf <- factor(A$in_sf)

A$in_sf

A$in_sf %>% class()

#Nota imporanta: Si quieres correr un árbol con la función rpart, 
  #SIEMPRE debes convertir la etiqueta a pronosticar de esta forma...factor()


#Corremos un árbol de decisión
tree <- rpart(in_sf ~ ., data = A) #Listo, esto ya es CORRER un algoritmo de machine learning
#Los resultados quedan guardados en la variable "tree"

tree

#¿Qué decisiones está tomando en cada rama?...
tree

#Observamos como se ve un árbol de decisión graficado...
rpart.plot(tree)


#Tu algoritmo de clasificación (árbol)...
#¿Qué etiqueta asigna a cada registro?

#Con el modelo tree que acabas de crear (object), le das unos datos (newdata) para obtener el pronostico de clase
#que resulta de tu modelo de ML
predict(object = tree,newdata = A,type = "class")

#¿Qué quieren decir los resultados anteriores?...
#Observa nuevamente
predict(object = tree,newdata = A,type = "class")

#Podemos pegar estos resultados en la tabla original A
A$prediccion <- predict(object = tree,newdata = A,type = "class")

#Observa los resultados...
View(A)

#¿Cuantos son de cada clase?
table(A$prediccion)

#¿Cómo se comparan estas predicciones vs los datos reales?
A <- A %>% 
  select(beds:elevation,in_sf,prediccion) 

View(A)

B <- A %>% select(beds,price)

#Puedes guardar esta matriz en tu computadora con la instruccion write_csv

#x: especificias el objeto de R que quieres exportar (en este caso la matriz A)
#file: escribes el nombre que quieres darle al archivo
write_csv(x = A,file = "modelo_arbol.csv")


#Pregunta en clase:

  #¿Cuál es el accuracy que resulta de este modelo tree?
  #Abre el archivo en Excel en tu computadora y responde...


#Diapositivas...





















################################################################################
################################################################################
##########      Algoritmos ML Clasificación              #######################
################################################################################
################################################################################

#Paquetes adicionales
library(glmnet)  #Modelos ML
library(caret)  #Modelos ML
library(tidymodels)  #Modelos ML
library(xgboost) #Modelo paralelizado de ML



################################################################################
### Regresión Logística


#Abrimos la base sobre deptos NY vs SFO

setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Corremos el modelo de regresión logística, ¿qué obtienes?

model <- logistic_reg(penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(in_sf ~ ., data = A)

#Pregunta: ¿qué sucedió al correr la función?...












#Al igual que con el árbol, debemos convertir la etiqueta en factor
A$in_sf<- as.factor(A$in_sf)

#Corremos nuevamente el modelo...
model <- logistic_reg(penalty = double(1)) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(in_sf ~ ., data = A)

#Para obtener la predicciones utilizamos predict()
pred_class <- predict(model,
                      new_data = A,
                      type = "class")

#Observamos los resultados
pred_class

print(pred_class,n=400)

#Agregamos los resultados a la matriz
A <- A %>% 
  mutate(prediccion = pred_class$.pred_class) #Nota que el resultado de predict() es un data.frame

View(A)

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_logistica <- correctos / nrow(A)
accuracy_logistica

#¿Qué funciona mejor considerando accuracy? ¿El árbol de decisión, el filtro en Excel o esta regresión logística?

0.91
0.67
0.90

#Alternativa:
#Tambien puedes correr una regresión logística con la funcion glm()...
mod_1 <- glm(in_sf ~ ., data = A, family = 'binomial')


mod_1 














################################################################################
### Árbol de decisión

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Convertimos en factor la etiqueta
A$in_sf <- as.factor(A$in_sf)

#Corremos el árbol
tree <- rpart(in_sf ~ ., data = A)

#Podemos los pronosticos en la tabla original A
A$prediccion <- predict(object = tree,newdata = A,type = "class")

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_arbol <- correctos / nrow(A)
accuracy_arbol











################################################################################
### Bosque Aleatorio

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Corremos un bosque aleatorio
model_rf <- train(
  in_sf ~ .,
  data = A,
  method = "rf",
  ntree = 5,#Número de árboles) # opcional
)

#Predicciones del bosque
A$prediccion <- predict(object = model_rf,newdata = A,type = "raw")

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Pregunta: ¿Qué está sucediendo? ¿Por qué crees que ocurre?


















set.seed(1923434)

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Convertimos en factor la etiqueta
A$in_sf <- as.factor(A$in_sf)

#Corremos un bosque aleatorio
model_rf <- train(
  in_sf ~ .,
  data = A,
  method = "rf",
  ntree = 5,#Número de árboles) # opcional
)

#Predicciones del bosque
A$prediccion <- predict(object = model_rf,newdata = A,type = "raw")

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_bosque_5 <- correctos / nrow(A)
accuracy_bosque_5



################################################################################

#Pregunta: ¿Qué pasa si aumentamos a 25 árboles? 
#¿Qué efecto podría tener nuestro bosque?

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Convertimos en factor la etiqueta
A$in_sf <- as.factor(A$in_sf)

#Corremos un bosque aleatorio
model_rf <- train(
  in_sf ~ .,
  data = A,
  method = "rf",
  ntree = 100,#Número de árboles) # opcional
)

#Predicciones del bosque
A$prediccion <- predict(object = model_rf,newdata = A,type = "raw")

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_bosque_25 <- correctos / nrow(A)
accuracy_bosque_25


#Comparación:
accuracy_bosque_5

accuracy_bosque_25

  #¿Crees que siempre mejoramos al aumentar el número de árboles?...






####################################################################
##################### XGBoost ######################################
####################################################################

#Paquete para xgboost
library(xgboost)

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx", 1)
A <- select(A, in_sf:elevation)

#Asegúrate de que la etiqueta es 0/1
A$in_sf <- as.numeric(A$in_sf)

#Matriz para XGBoost
dtrain <- xgb.DMatrix(data = A %>% select(-in_sf) %>% as.matrix(),
                      label = A$in_sf)
    #Revisemos con atención el parámetro "data"


#Entrenamos el modelo con algunos parámetros:

#nrounds iteraciones del algoritmo boosting
#max.depth profundida máxima que puede tener el árbol
bst <- xgboost(data = dtrain,
               max.depth = 4,
               nrounds = 5,
               objective = "binary:logistic",
               verbose = 0)

# xgb.train(params = params, data = dtrain, nrounds = nrounds, 
#           watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
#           early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
#           save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
#           callbacks = callbacks, max.depth = 3, objective = "binary:logistic")

#Todos los parámetros del Xgboost
#https://xgboost.readthedocs.io/en/stable/parameter.html

#Lo mismo pasa con la mayoría de los algoritmos de ML...
#Son funciones con parámetros que entregan distintas predicciones según los valores...

#https://www.researchgate.net/figure/3D-visualization-of-error-function_fig2_343763325


#Evaluamos las Predicciones 
A$prob <- predict(bst, newdata = A %>% select(-in_sf) %>% as.matrix())

#Convertimos a clases (0/1)
A$prediccion <- ifelse(A$prob > 0.5, 1, 0)

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_xgboost_5 <- correctos / nrow(A)
accuracy_xgboost_5


#Observa lo siguiente... ¿Qué significa que A$prob > 0.5?
#¿Qué impacto tendría si lo modificamos?...

ifelse(A$prob > 0.5, 1, 0)

#Clasificación con 0.5
ifelse(A$prob > 0.5, 1, 0) %>%
  table()

#Clasificación con 0.2
ifelse(A$prob > 0.2, 1, 0) %>%
  table()

#Clasificación con 0.9
ifelse(A$prob > 0.9, 1, 0) %>%
  table()

### GRAN Pregunta:
# ¿Qué valor te garantiza un accuracy más alto?...

#En el fondo, esto se trata de un problema de Optimización...
# ¿Cuál es la combinación de parámetros de mi modelo de ML que me ayuda a maximizar mi métrica?
#De momento conocemos sólo accuracy... pero hay muchísimas más métricas para clasificación

#Referencia: https://www.ibm.com/mx-es/think/topics/hyperparameter-tuning


#Además...

#¿Qué pasa si aumentamos el número de iteraciones?
#Ahora son 100 en lugar de 5...

#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx", 1)
A <- select(A, in_sf:elevation)

#Asegúrate de que la etiqueta es 0/1
A$in_sf <- as.numeric(A$in_sf)

#Matriz para XGBoost
dtrain <- xgb.DMatrix(data = A %>% select(-in_sf) %>% as.matrix(),
                      label = A$in_sf)


#Entrenamos el modelo con algunos parámetros:

#nrounds iteraciones del algoritmo boosting
#max.depth profundida máxima que puede tener el árbol
bst <- xgboost(data = dtrain,
               max.depth = 4,
               nrounds = 100,
               objective = "binary:logistic",
               verbose = 0)

#Evaluamos las Predicciones 
A$prob <- predict(bst, newdata = A %>% select(-in_sf) %>% as.matrix())

#Convertimos a clases (0/1)
A$prediccion <- ifelse(A$prob > 0.5, 1, 0)

#Observamos datos y etiqueta v.s. predicciones
A %>% 
  select(beds:price_per_sqft,in_sf,prediccion) %>% 
  View()

#Registros clasificados correctamente, independientemente de la clase...
correctos <- filter(A,in_sf == prediccion) %>% nrow()
correctos

#Accuracy
accuracy_xgboost_100 <- correctos / nrow(A)
accuracy_xgboost_100

#100% de precisión (considerando accuracy)...


#Comparando con los demás modelos:

accuracy_xgboost_100

accuracy_bosque_5

accuracy_bosque_25

accuracy_xgboost_5


#Pero... ¿qué otras métricas para clasificación tenemos?...














####################################################################
####################################################################
####################################################################
# ¿Cómo medir el tiempo?...

library(lubridate)#Paquete para lidiar con tiempos y fechas de procesamiento

ahora <- now()

#Deja pasar unos segundos...

despues <-  now()


#Diferencia tiempo

despues - ahora

as.numeric(despues - ahora)



####################################################################

#Tiempo procesamiento

ahora <- now()

#Ponemos algun algoritmo - random rofest 100:


#Cargamos los datos nuevamente...
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2025/Clase2")
A <- read_excel("nyc_sfo_deptos.xlsx",1) 

#Conservamos algunas variables y la etiqueta:
A <- select(A,in_sf:elevation)

#Convertimos en factor la etiqueta
A$in_sf <- as.factor(A$in_sf)

#Corremos un bosque aleatorio
model_rf <- train(
  in_sf ~ .,
  data = A,
  method = "rf",
  ntree = 100,#Número de árboles) # opcional
)

#Predicciones del bosque
A$prediccion <- predict(object = model_rf,newdata = A,type = "raw")

#Medimos al termino
despues <-  now()

#Tiempo total en correr el algoritmo:
despues - ahora



