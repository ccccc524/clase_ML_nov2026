#######################################################################################
#######################################################################################
################      CLASE 1                     ####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Código ejemplo e introducción
#Contenido: mapitas y heatmap

#Cursos y Diplomados iTAM


####Presentación de R


####Instalación de paquetes
library("readr") #en un inicio R, no encontrará el paquete

#¿Qué son los paquetes? - idea: calculadora científica...

install.packages("readr")
#Comprueba que está instalado...


#Forma alternativa de instalar... ventana inferior derecha...

#Instala el resto
install.packages("leaflet") #Paquetes para hacer mapas
install.packages("leaflet.extras")
install.packages("tidyverse") #Paquetes para manipular datos de forma eficiente


#Activa los paquetes instalados
library(readr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
#Puedes observar que ya no señala ningún error...

#¿Comprobación de que ya se cargo el paquete?



#Datos

#Escuelas

#Paso IMPORTANTE, coloca la ruta correcta en tu computadora
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase1") #CAMBIALOOOOO POR FAVOR


#Opción 2: Hacerlo con el menu Session...
#Por favor, ten cuidado al cambiarlo

#Al correr la siguiente instrucción, podrás ver los archivos que están dentro de tu directorio:
list.files()

#En lugar de abrirlo en Excel, lo podemos incorporar en R
esc <- read_csv("escuelas.csv") #Observa que es un csv, si fuera un Excel necesitas otra intrucción/función (read_excel)


#Formato: Corregimos la codificación (acentos mal escritos y caracteres raros)
esc <- as.data.frame(
  lapply(esc, function(x) {
    if (is.character(x)) {
      iconv(x, from = "latin1", to = "UTF-8", sub = "")
    } else {
      x
    }
  }),
  stringsAsFactors = FALSE
)


#Veamos el archivo
View(esc) 
#Forma alternativa ventana superior derecha de RStudio
#Si lo abrse en Excel, podrás ver la misma pantalla

#Observa que hasta ahora, solo incorporamos nuestra base de datos en R; aún no hacemos nada más...





######################################################################################################
######################################################################################################
########################     Mapa           ##########################################################
######################################################################################################
######################################################################################################

#Nuestro primer ejercicio será hacer un mapa de las escuelas

#Primero observamos los datos sobre las escuelas
leaflet(data = esc) %>% addTiles() %>%
  addMarkers(~longitud, ~latitud)
#Nota que en el parámetros data = debes escribir el nombre de la BD
#nota que en addMarkers debes poner cómo están escritas las variables de Latitud, Longitud

#Existe una forma más eficiente para visualizar puntos, en lugar de addMarkers
leaflet(data = esc) %>% addTiles() %>%
  addCircles(~longitud, ~latitud) #Además es un formato más "ligero" 
#Nota que en lugar de addMarkers dice addCircles
#¿Cuál es el efecto de cambiar esta línea del código?






######################################################################################################
######################################################################################################
##########        Introducción a R              ######################################################
######################################################################################################
######################################################################################################

###### Sesión Introductoria R - ITAM

#En este código vamos a revisar algunas de las funciones básicas de R y 
#sus componentes: consola, editor, paquetes y variables


#Paquetes que no utilizaremos de momento pero que son muy utiles tener para mas adelante

#Recuerda instalar los que aún te hacen falta...
library(jsonlite)
library(foreign)
library(tidyverse)
library(lubridate)
library(readr)
library(sp)
library(leaflet)
library(readxl)
library(RCurl)
library(stringr)


#Asignamos un vector
a <- c(1,2,3)
#Nota importante, a diferencia de Python en R, usaras funciones para obtener caracterisitcas de los elementos

#Por ejemplo
#En lugar de a.mean(), vas a hacer
mean(a)



#Correr primeras lineas

iris #Dataset ya cargado en R
View(iris)

summary(iris) #medidas resumen

#Podemos probar algunas operaciones básicas y comandos

2+2

#Asignamos variable
hola <- 2

hola + 4

#Tambien se puede asignar de esta forma
hola = 3

hola + 4

#Declaramos un vector de números
vector <- c(1,2,3,4,5,6,7,8,9,10)

#Podemos hacer una operacion sobre todo el vector
vector + 10

#y lo podemos capturar en una nueva variable
vector_bis <- vector + 10

vector_bis


#Tambien podemos llamar elementos en una posición específica
vector[5]

#Para llamar posiciones seguidas
vector[5:7]

#Para llamar numeross en posiciones salteadas utilizamos el vector de números (para las posiciones)
vector[c(1,3,5)]



################################################################################
## Operaciones

## Suma
sum(vector)

## Máximo
max(vector)

## Mínimo
min(vector)

## Promedio
mean(vector)

## Media
median(vector)

## Raiz cuadrada
sqrt(vector)

# Logaritmo
log(vector)


## Secuencia

seq(2, 10, by=1)

seq(2, 10, by=2)


######################################################################
###### Tipos de datos

#Siempre podemos utilizar la funcion class()

x <- 10
x

y <- "hola Jonas"
y

p <- TRUE
p

#De esta forma podemos conocer la clase de una variable declarada anteriormente
#class() sera muy util para usar otros paquetes o funciones no programadas por el usuario
class(x)

class(y)

class(p)



#Vectores
#De igual forma, se pueden declarar vectores con distintos tipos de variables

vec1 <- c(1:10)#Con los : quiere decir "seguido"
vec1

#¿De qué otra forma podríamos crear vec1?


vec2 <- c(1,5,7,8,10,1,5,7,8,10)
vec2

#De hecho se pueden sumar directamente
vec1+vec2

#Tambien nos podemos quedar con un pedazo del vector
vec1[1:5]


#Operadores lógicos (mayor que, menor que, etc)
#son muy intuitivos

vec1[1]

vec1[5]

vec1[1] < vec1[5]#Nos regresa un boolean

vec1[1] == vec1[5]#Observa que se pone == no =, pues = es para asignar variables
#otra alternativa para asignar variables es con <- 

vec1[1] != vec1[5]

#Podemos preguntar a los miembros de un vector
vec1 < 3 #Y nos regresara una cadena con booleans

#Para saber la longitud de nuestro vector podemos utilizar length()
length(vec1)




######################################################################
###### Matrices

#Ademas de los vectores, las matrices nos pueden ayudar a diseñar algoritmo 
#y manipular los datos en R
#aunque, mas adelante tambien revisaremos los data.frame

#Para crear una matriz de ceros utilizamos la siguiente linea
#Primero se especifica el numero de rengloes y luego las filas

mat <- matrix(0,10,5)

View(mat)

#También la podemos llenar con otro número
mat <- matrix(3.14,10,5)

View(mat)

#Para saber las dimensiones de la matriz podemos utilizar el siguiente comando
#dim()
dim(mat)

#Tambien podemos acomodar un arreglo de numeros, 
mat1 <- matrix(1:10, byrow = TRUE, nrow = 5)
mat1

#Tambien puedes llenar la matriz con otros vectores ya calculados
mat <- matrix(0,10,5)
vec1

mat[,1] <- vec1[1:5]

mat

#Para referenciar elemento de la matriz se pueden utilizar los siguientes comandos
#Slice matrix

mat[5,2] #Renglon 5, columna 2

#Tambien te puedes quedar con ciertos "pedazos de la matriz"
mat[4:7,2:3]
#Con el vector de posiciones
mat[4:5,c(1,3,5)]



###Union de matrices con cbind() y rbind()
#Concatenar

aux1 <- matrix(1,5,10)
aux1

aux2 <- matrix(0,10,5)
aux2

#cbind() - union por columnas
#Nota: las matrices deben tener el mismo numero de filas

mat <- cbind(aux1,aux1)
mat

dim(mat)

cbind(aux1,aux2)#Error
#¿Por qué crees que marca error?

dim(aux1)

dim(aux2)


#rbind() - union por filas
#Nota: las matrices deben tener el mismo numero de columnas

mat <- rbind(aux1,aux1)
dim(mat)

rbind(aux1,aux2)#Error
#¿Por qué crees que marca error?




######################################################################
###### #Estructuras básicas de programación

vec <- c(1,2,3,4,5,6,7,8,9,10)

###############
#If (condicional)

#Imprimimos el primer valor
vec[1]

if(vec[1] < 5) {
  print("hola")
}


###############
#For (ciclo)

#Imprimimos el valor
var <-  1:15
var

#Imprime los valores de cada posicion del vector
for (i in 1:length(var)){
  print(var[i])
}
#Observa que imprimio el valor paso a paso (entrada por entrada)... 

#Ahora podríamos imprimir cada número del vector, al cuadrado
for (i in 1:length(var)){
  print(var[i]*var[i])
}



######################################################################
###### Dataframe y matrices

#Creamos una matriz de ejemplo
mat <- matrix(0,10,5)

View(mat)#revisar las columnas

dim(mat)

#¿Cómo puedes referenciar los datos de cada columna de la matriz?
mat[,1]

mat[,3]

#¿Cuál es la desventaja si tienes 1,000 columnas?...


################################################################
#Dataframe
mat <- data.frame(mat)

View(mat)#revisar las columnas

#Una de las características más importantes de los dataframe está relacionado con sus columnas
colnames(mat)


####Append
#Podemos referenciar fácilmente cada columna
mat$X1

mat$X2

#Escribe el signo de "$" y después utiliza la tecla TAB:
mat$
  
  #Importante: Cuando utilizas un dataframe no necesitas recordar el nombre de las variables
  #tampoco necesitas recordar el número de variable para referenciarla: mat[,1], etc..
  
  #Otra de las ventajas más relavantes de los data.frame es que puede agrupar conjuntos de datos 
  #de distinta naturaleza en un mismo conjunto de datos.
  
  #Esta caracteristica, lo hace particularmente util porque se puede utilizar ¡como una BD!
  #En otras palabras, en una BD puedes almacenar texto, números, etc.
  
  #Una BD (base de datos) es uno de los principales fundamentos para hacer análisis defdatos



################################################################
#Tipos de variables

#Podemos crear un vector de numeros, otro de caracteres y otro boolean
a <- c(10,20)
b <- c('book','pen','textbook','pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)

#Los podemos acomodar en el mismo data.frame aunque sean de distinta clase
df <- data.frame(a,b,c)

df

#Nota importante: esta caracteristica de juntar distintos tipos de variables en una misma BD
#solo puede hacerse en un dataframe. 

#Por ejemplo, al intentar hacerlo en un vector obtenemos lo siguiente

c('a','w') #Caracteres

c(2,4) #Numeros

c(2,'w') #Observar que el número se tranasforma en caracter...

c(2,w) #Pero...un caracter NO se puede convertir en número...


df

#Para revisar los nombres de las columnas de un data.frame podemos hacer la siguiente instruccion
colnames(df)
#Recuerda: en un dataframe NO necesitas recordar los nombres...


#Recordando la instruccion class
class(df)

#Ademas, podemos obtener caracteristicas claras sobre los componentes de cada data.frame
str(df)

#Tambien aplica el comando dim que teniamos para matrices
dim(df)

#Y el resumen estadistico summary
summary(df)

#Resumen estadístico por variable
#Una forma alternativa podría ser llamar la columna con las funciones individuales
mean(df$a)

median(df$a)

min(df$a)

max(df$a)




################################################################
####Slice

#Al igual que las matrices vistas anteriormente, tambien es posible llamar secciones de la matriz
# a traves de los indices

df[2:4,1]

df[c(3,1),2]

#Observa la diferencia y posibilidad de cambiar el orden dentro del mismo conjunto de indices
df[c(1,3),2]

df[c(1,3),2] == df[c(3,1),2]




#Recodatorio sobre Append() *****

#Una de las caracteristicas mas interesantes y utiles de los data.frame es que se pueden
#referencias a las columnas a traves del nombre

#Por ejemplo, imagina que tienes una matriz con 150 columnas, y quieres obtener el promedio
#de aquella que tiene información sobre la edad. ¿Podrías recordar fácilmente el número de
#columna en la que están los datos? --- NOTA: es más fácil mediante el nombre (a esto se le llama Append)

colnames(df)

df

#Asignamos nuevos nombres
colnames(df) <- c('ID','concept','store')

#Si dos columnas podemos utilizar esta instruccion (en lugar de llamar el nombre)
df[, c('ID', 'store')]


#Finalmente, la mayor bondad de append un data.frame es utilizar el simbolo $ para llamar variables

df$ID

df$concept

#Al poner esto y el tab, te dara el nombre de todas las variables disponibles en tu data.frame
#df$

#NOTA: Es mucho más facil analizar una BD con este tipo de recursos $ para el nombre de las variables


#Asignacion con append: tambien es posible asignar nuevos valores dentro de la matriz
#referenciando el nombre de la columna
df

df$ID <- c(1,2,3,4)

df

df$ID[c(1,3)] <- c(5,60000)

df$ID[c(1,3)]

#Sin embargo, observa que el vector debe ser de la misma longitud
df$ID <- c(1,2,3,4,5)





################################################################
###Funciones subset en Dataframe

#Los dataframe tienen una funcionalidad interesante y útil al momento de procesar una BD

#Imagina que necesitas quedarte con los datos de aquellas tiendas cuyo ID es mayor que 2
df$ID

#Puedes utilizar la funcion SUBSET
subset(df, subset = ID > 2)

#Esto tambien funciona en tydiverse...
df %>% 
  filter(ID > 2)





####################################################################################
#Listas en R

#Una de las estructuras de datos mas interesantes que tiene R (además de las matrices y dataframe)
#son las listas

#Esta estructura es muy util porque permite guardar datos de distinta naturaleza en la misma variable
#Ejemplo

#vector con numeros
a <- c(1:10)
a

#dataframe
df

#Matriz
mat <- matrix(0,10,5)

#Creamos una lista  
lista_1 <- list(a,df,mat)

#La puedes considerar como una especie de "bolsa" en la que puedas trasladar todos los datos que
#necesites" sin importar la clase y la dimension de cada conjunto.
lista_1

#Observa con atención, todos los datos y formatos almacenados

#Para revisar el numero de elemento que contiene una lista puedes utilizar la funcion length
length(lista_1)

#Por otro lado, puedes "obtener" los elementos de la lista a través de una sencilla notacion de corchetes
lista_1[[1]]

lista_1[[2]]

lista_1[[3]]

#Nota: Observa que los elementos obtenidos de la lista siguen conservando sus caracteristicas de datos
class(lista_1[[1]])

class(lista_1[[2]])

class(lista_1[[3]])

#Almacenar objetos en una lista, no cambia su naturaleza (todas sus características se pueden conservar)...


