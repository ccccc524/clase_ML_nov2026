#######################################################################################
#######################################################################################
################      CLASE 3-4                     ###################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Contenido: Modelos ML clasificación, métricas, Algoritmo Shazam, Definiciones, Modelos ML regresión

#Paquetes hasta el momento:
library(readr)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(rpart) #arboles de decisón
library(rpart.plot) #graficos de arbol de decision
library(readxl) #leer archivos de excel
library(xgboost) #algoeritmo para paralelizar arboles de decision
library(tidymodels)
library(caret)
library(lubridate)#Paquete para lidiar con tiempos y fechas de procesamiento

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

#Medimos al termino
despues <-  now()

#Tiempo total en correr el algoritmo:
despues - ahora










################################################################################
##########      Algoritmos ML Regresión                  #######################
################################################################################

#Datos rendimiento de alumnos
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_3_4")
performance <- read_csv("Student_Performance.csv")

#Veamos qué contiene la base de datos
performance %>% 
  View()


####################################################################
#Regresión lineal
#El modelo clásico e inicial de siempre...
reg_model <- lm(Performance_Index  ~ ., data=performance)

#Parámetros de la recta (modelo base de la regresión)
reg_model

#Predicciones
pred <- predict(object = reg_model,newdata = performance)

#Observamos las estimaciones del modelo:
pred

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred

#Observamos los datos
View(performance)



####################################################################
#Para calcular el ECM de nuestros modelos de regresión
# vamos a programar una función en R que me permita evaluar todos los modelos

#ECM
A <- performance$Performance_Index
B <- pred #Resutaldos del modelo de predicción

#Con esta notación podemos crear una función para utilizarla siempre
ecm <- function(A,B){
  error <- 0
  for (i in 1:length(B)){
    error <- error + ((A[i] - B[i])^2)
  }
  error <- error/length(B)
  return(error)
}

#Por supuesto, hay funciones de R que también lo calculan pero 
#el objetivo es crear el código para entender muy bien la fórmula

#Finalmente, utilizamos la fórmula para calcular el ECM regresión
ecm_regresion <- ecm(A,B)
ecm_regresion



#Originalmente, estudiamos problemas de clasificación y sus modelos,
#pero recuerda... hay algunos algoritmos que funcionan para ambos tipos de problema...



####################################################################
####################################################################
#Árbol de decisión

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_3_4")
performance <- read_csv("Student_Performance.csv")

#Observa que es la MISMA fórmula que para problemas de clasificación
tree <- rpart(Performance_Index ~ ., data = performance)

#Imprimimos el modelo de ML
tree

#¿Qué variables y rangos está tomando nuestro arbol?
rpart.plot(tree)

#Predicciones con el árbol de regresion
pred_arbol <- predict(object = tree,newdata = performance)

#Imprime el modelo ML:
pred_arbol

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_arbol

#Observamos los datos
View(performance)

#Pregunta Importante:
#¿Qué paso adicional tendríamos que hacer en un árbol para clasificación?...

#ECM árbol
ecm_arbol <- ecm(A,pred_arbol)
ecm_arbol

#Recordemos ECM de la regresión lineal
ecm_regresion

#Pregunta:
#Intuitivamente, ¿se te ocurre alguna razón por la que el modelo de regresión funciona mejor que un árbol de decisión?





####################################################################
####################################################################
#Bosque aleatorio

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_3_4")
performance <- read_csv("Student_Performance.csv")

#Corremos el modelo bosque aleatorio para problemas de regresión
model_rf <- caret::train( #Estoy espeficicando que quiero usar la funcion train() del paquete caret...
  Performance_Index ~ .,
  data = performance,
  method = "rf",
  ntree = 5,#Comenzamos simulando con 5 árboles
)

#Observemos el modelo:
model_rf

#Predicciones sobre el mismo conjunto de datos
pred_bosque <- predict(object = model_rf,newdata = performance)

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_bosque

#Observamos los datos
View(performance)

#ECM árbol
ecm_bosque_5 <- ecm(A,pred_bosque)
ecm_bosque_5



#Pregunta: ¿Qué pasaría si aumentamos a 10 árboles?...

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_3_4")
performance <- read_csv("Student_Performance.csv")

#Corremos el modelo bosque aleatorio para problemas de regresión
model_rf <- caret::train(
  Performance_Index ~ .,
  data = performance,
  method = "rf",
  ntree = 10,#Aumentamos a 25 árboles
)

#Observamos el modelo 
model_rf

#Predicciones
pred_bosque <- predict(object = model_rf,newdata = performance)

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_bosque

#Observamos los datos
View(performance)

#ECM árbol
ecm_bosque_10 <- ecm(A,pred_bosque)
ecm_bosque_10

#¿Cuál fue mejor regresión lineal, bosque o árbol?
ecm_regresion

ecm_bosque_5

ecm_bosque_10


####################################################################
#Preguntas:

#1. ¿Cuál corrió más lento? ¿por qué?

#2. ¿Cómo toda la decisión del ECM un bosque aleatorio? ¿por qué?... #Regresar a Diapositivas


















#######################################################################################
#######################################################################################
################      Algoritmo Shazam            #####################################
#######################################################################################
#######################################################################################

#Contenido: ¿Cómo funciona Shazam? Recreamos el algoritmo en R
#Basado en: Wang, A. (2003). An Industrial-Strength Audio Search Algorithm.

#El código sigue el mismo orden que la presentación:
#0. Preparación
#1. Ver el sonido (leer MP3, 8 kHz, Fourier, espectrograma)
#2. La constelación (nos quedamos con los picos)
#3. Las huellas (parejas de estrellas -> hash)
#4. El índice (biblioteca de canciones)
#5. La votación (reconocer las muestras)

#Tiempo aproximado: procesar 50 canciones de ~3 min tarda unos 4 minutos
#Reconocer una muestra tarda ~1 segundo

#Directorio de mi computadora...
#Yo haré el ejercicio con estas canciones
setwd("~/Desktop/Carlos/cursos/ITAM/Taller_MCD_agosto2026/Clase9/musica")

#Les dejo estás canciones libres...
#Modifica la ruta en tu computadora
#setwd("~/Desktop/Carlos/cursos/ITAM/Taller_MCD_agosto2026/Clase9/musica_libre_prueba")

#canciones/ -> canciones completas en mp3 (la biblioteca)
#muestras/  -> muestras a "shazamear" en mp3 (algunas están en la biblioteca y otras no)



################################################################################
################################################################################
##########                0. Preparación                 #######################
################################################################################
################################################################################


################################################################################
################################################################################
################################################################################
#Carga todo el siguiente bloque de parámetros y datos...

#Paquetes hasta el momento:
library(tuneR)       #leer archivos mp3 o wav
library(signal)      #filtro para bajar la frecuencia de muestreo
library(data.table)  #tablas rápidas, para el índice de huellas
library(ggplot2)     #gráficas
library(tidyverse)

#Carpetas
CARPETA_CANCIONES <- "canciones"
CARPETA_MUESTRAS  <- "muestras"
ARCHIVO_INDICE    <- "indice_huellas.rds"   #aquí guardamos la biblioteca ya procesada

#Parámetros del algoritmo (los podemos mover para experimentar)
SR          <- 8000   #muestras por segundo con las que trabajamos (igual que el paper)
N_VENTANA   <- 1024   #tamaño de cada pedacito de audio para Fourier (1024/8000 = 128 ms)
SALTO       <- 256    #cada cuánto empieza un pedacito nuevo (256/8000 = 32 ms)
VECINDAD_F  <- 8      #un pico debe ganarle a sus vecinos ±8 renglones de frecuencia
VECINDAD_T  <- 8      #y ±8 columnas de tiempo
PERCENTIL   <- 0.85   #y además estar en el 15% más fuerte de toda la canción
FAN_OUT     <- 10     #con cuántas estrellas de la zona se empareja cada ancla
DT_MAX      <- 63     #la zona llega hasta 63 pasos adelante (~2 s)
DF_MAX      <- 120    #y ±120 renglones de frecuencia (~940 Hz)

#Canción que usamos para los ejemplos y gráficas de los pasos 1 a 3
CANCION_EJEMPLO <- 1      #número de canción (en orden alfabético) dentro de la carpeta
MUESTRA_EJEMPLO <- 1      #número de muestra que analizamos paso a paso en la sección 5.1

#Para guardar las gráficas en una carpeta
GUARDAR_GRAFICAS <- FALSE
ver <- function(p, nombre) {
  print(p)
  if (GUARDAR_GRAFICAS) {
    dir.create("graficas", showWarnings = FALSE)
    ggsave(file.path("graficas", paste0(nombre, ".png")), p, width = 9, height = 5, dpi = 120, bg = "white")
  }
  invisible(p)
}
tema <- theme_minimal(base_size = 13)

#Funciones de apoyo para pasar de renglón o columna a Hz y segundos
renglon_a_hz  <- function(f) f * SR / N_VENTANA   #cada renglón mide 8000/1024 = 7.8 Hz
paso_a_seg    <- function(t) t * SALTO / SR       #cada paso de tiempo mide 32 ms

################################################################################
################################################################################
################################################################################




################################################################################
################################################################################
##########                1. Ver el sonido               #######################
################################################################################
################################################################################

#Diapositivas: "Así ve la computadora el audio", "El truco: separar el sonido en notas"
#y "El espectrograma: la partitura de la canción"

####################################################################
######  1.1 Leer un mp3

#Un mp3 es audio comprimido. readMP3() lo descomprime y nos da los números de la onda
archivos_canciones <- list.files(CARPETA_CANCIONES, pattern = "\\.mp3$", full.names = TRUE)
cat("Canciones encontradas:", length(archivos_canciones), "\n")

#Utilizamos esta canción:
archivos_canciones[CANCION_EJEMPLO]

w <- readMP3(archivos_canciones[CANCION_EJEMPLO])
w            #observamos: 44,100 muestras por segundo y estéreo (canal izquierdo y derecho)


####################################################################
######  1.2 Pasar a mono

#¿Por qué?
#Estéreo son dos canales casi iguales (izquierdo y derecho). La huella sólo necesita
#saber QUÉ notas suenan y CUÁNDO, no si vienen de la izquierda o de la derecha
#Además el micrófono del celular graba prácticamente en mono, así comparamos peras con peras
#Y de paso trabajamos con la mitad de los datos
#Para pasar a mono promediamos los dos canales
a_mono <- function(w) {
  if (w@stereo) (w@left + w@right) / 2 else as.numeric(w@left)
}

####################################################################
######  1.3 Bajar a 8,000 muestras por segundo

#El mp3 trae 44,100 números por segundo, pero las notas que nos importan están por debajo de ~4,000 Hz
#Con 8,000 muestras por segundo representamos sonidos de hasta 4,000 Hz (la mitad: frecuencia de Nyquist)
#Son 5.5 veces menos datos
#Ojo: antes de tirar muestras quitamos lo que está arriba de 4,000 Hz con un filtro
#Si no lo hacemos, esos agudos se "disfrazan" de graves (aliasing)
a_8khz <- function(x, sr_original) {
  if (sr_original == SR) return(x)
  filtro <- butter(6, 3800 / (sr_original / 2))        #filtro pasa-bajas: deja pasar < 3,800 Hz
  x <- filtfilt(filtro, x)
  x[round(seq(1, length(x), by = sr_original / SR))]   #nos quedamos con 1 de cada 5.5 muestras
}

#Juntamos todo en una función: archivo -> vector de números (mono, 8 kHz, volumen normalizado)
leer_audio <- function(archivo) {
  ext <- tolower(tools::file_ext(archivo))
  w <- if (ext == "mp3") readMP3(archivo) else if (ext == "wav") readWave(archivo) else
    stop("Formato no soportado: convierte a .mp3 o .wav")
  x <- a_mono(w)
  x <- a_8khz(x, w@samp.rate)
  x / max(abs(x))                                        #volumen entre -1 y 1
}

x <- leer_audio(archivos_canciones[CANCION_EJEMPLO])
cat("Duración:", round(length(x) / SR, 1), "s  ·  números en el vector:", length(x), "\n")

#Imprimimos onda o señal
x

#Para las gráficas usamos una ventana de 10 s a la mitad de la canción
INICIO_DEMO <- max(0, floor(length(x) / SR / 2) - 5)

#Gráfica: 3 segundos de la onda
seg <- INICIO_DEMO * SR + 1:(3 * SR)
ver(ggplot(data.frame(t = seg / SR, y = x[seg]), aes(t, y)) +
      geom_line(linewidth = 0.2) +
      labs(title = "Así ve la computadora el audio", x = "segundos", y = NULL) + tema,
    "1_onda")














####################################################################
######  1.4 Fourier: separar un sonido en sus notas

#Mezclamos 3 tonos puros (grave, medio y agudo) y le pedimos a Fourier que los encuentre
# t  <- (0:(SR - 1)) / SR                                  # 1 segundo
# mezcla <- 1.0 * sin(2 * pi * 110 * t) + 0.7 * sin(2 * pi * 330 * t) + 0.5 * sin(2 * pi * 880 * t)
# espectro <- Mod(fft(mezcla))[1:(SR / 2)]                 # fft() hace la Transformada de Fourier
# ver(ggplot(data.frame(hz = 0:(SR / 2 - 1), fuerza = espectro), aes(hz, fuerza)) +
#       geom_line() + coord_cartesian(xlim = c(0, 1200)) +
#       labs(title = "Fourier encuentra las 3 notas: 110, 330 y 880 Hz", x = "Hz (vibraciones por segundo)", y = "fuerza") + tema,
#     "1_fourier")

####################################################################
######  1.5 Espectrograma: Fourier a cada pedacito

#Cortamos el audio en pedacitos de 1024 muestras que se enciman (uno nuevo cada 256)
#A cada uno le aplicamos Fourier y los ponemos lado a lado
#Resultado: una matriz -> renglones = frecuencias (grave a agudo), columnas = tiempo
espectrograma <- function(x) {
  n_cols <- floor((length(x) - N_VENTANA) / SALTO) + 1
  idx    <- outer(1:N_VENTANA, (0:(n_cols - 1)) * SALTO, "+")    #posiciones de cada pedacito
  M      <- matrix(x[idx], nrow = N_VENTANA)                     #un pedacito por columna
  hann   <- 0.5 - 0.5 * cos(2 * pi * (0:(N_VENTANA - 1)) / (N_VENTANA - 1))
  M      <- M * hann                                             #suaviza las orillas del pedacito
  FT     <- mvfft(M)[1:(N_VENTANA / 2 + 1), , drop = FALSE]      #Fourier a todas las columnas
  20 * log10(Mod(FT) + 1e-6)                                     #fuerza en decibeles
}

S <- espectrograma(x)
dim(S)   #513 renglones de frecuencia * miles de columnas de tiempo

#Para dibujar pasamos un pedazo de 10 s de la matriz a una tabla larga
matriz_a_tabla <- function(S, cols, max_hz = 3000) {
  filas <- which(renglon_a_hz(0:(nrow(S) - 1)) <= max_hz)
  data.table(hz  = renglon_a_hz(rep(filas, times = length(cols)) - 1),
             seg = paso_a_seg(rep(cols, each = length(filas)) - 1),
             db  = as.vector(S[filas, cols]))
}
cols10 <- round(INICIO_DEMO / paso_a_seg(1)) + 1:min(round(10 / paso_a_seg(1)), ncol(S) - round(INICIO_DEMO / paso_a_seg(1)))   #10 s
d_spec <- matriz_a_tabla(S, cols10)


#IMPORTANTE:
#Tabla con todas las mediciones de Hz
View(d_spec)



lim    <- quantile(d_spec$db, c(0.35, 1))

#Gráfica del espectograma...
ver(ggplot(d_spec, aes(seg, hz, fill = pmax(db, lim[1]))) + geom_raster() +
      scale_fill_viridis_c(option = "magma", guide = "none") +
      labs(title = "Espectrograma: la partitura de la canción", x = "segundos", y = "Hz (grave -> agudo)") + tema,
    "1_espectrograma")













################################################################################
################################################################################
##########                2. La constelación             #######################
################################################################################
################################################################################

#Diapositivas: "Paso 2: Quedarnos con lo más fuerte" y "Las estrellas sobreviven al ruido"

#Un pico (estrella) es un punto del espectrograma que:
#a) es el más fuerte de su vecindario (un rectángulo de ±8 x ±8 a su alrededor)
#b) está entre el 15% más fuerte de toda la canción
#Al final sólo guardamos DÓNDE está cada estrella: (columna de tiempo, renglón de frecuencia)

#Máximo del vecindario: recorremos la matriz un lugar a la vez en cada dirección
#y nos quedamos con el mayor. Primero por renglones y luego por columnas
maximo_vecindario <- function(S, rf = VECINDAD_F, rt = VECINDAD_T) {
  M <- S; nr <- nrow(S); nc <- ncol(S)
  for (k in 1:rf) {   #vecinos arriba y abajo
    M <- pmax(M, rbind(S[-(1:k), , drop = FALSE], matrix(-Inf, k, nc)),
              rbind(matrix(-Inf, k, nc), S[1:(nr - k), , drop = FALSE]))
  }
  M2 <- M
  for (k in 1:rt) {   #vecinos izquierda y derecha
    M2 <- pmax(M2, cbind(M[, -(1:k), drop = FALSE], matrix(-Inf, nr, k)),
               cbind(matrix(-Inf, nr, k), M[, 1:(nc - k), drop = FALSE]))
  }
  M2
}

encontrar_picos <- function(S) {
  es_max <- S == maximo_vecindario(S)
  fuerte <- S > quantile(S, PERCENTIL)
  pos    <- which(es_max & fuerte, arr.ind = TRUE)
  #Usamos índices desde 0 (como en el paper): f = renglón de frecuencia, t = paso de tiempo
  data.table(t = pos[, 2] - 1L, f = pos[, 1] - 1L)[order(t, f)]
}

picos <- encontrar_picos(S)
cat("Estrellas en la canción:", nrow(picos), " (≈", round(nrow(picos) / (length(x) / SR), 1), "por segundo)\n")

#Gráfica: estrellas sobre el espectrograma y la constelación sola
p10 <- picos[t %between% (range(cols10) - 1) & renglon_a_hz(f) <= 3000]
# ver(ggplot(d_spec, aes(seg, hz)) + geom_raster(aes(fill = pmax(db, lim[1])), alpha = 0.4) +
#       geom_point(data = p10, aes(paso_a_seg(t), renglon_a_hz(f)), shape = 8, color = "gold", size = 2) +
#       scale_fill_viridis_c(option = "magma", guide = "none") +
#       labs(title = "Nos quedamos sólo con los picos", x = "segundos", y = "Hz") + tema +
#       theme(panel.background = element_rect(fill = "#0B0B14"), panel.grid = element_blank()),
#     "2_picos")
ver(ggplot(p10, aes(paso_a_seg(t), renglon_a_hz(f))) +
      geom_point(shape = 8, color = "gold", size = 2.2) +
      labs(title = "La constelación de la canción", x = "segundos", y = "Hz") +
      theme_void() + theme(plot.background = element_rect(fill = "#0B0B14"),
                           plot.title = element_text(color = "white")),
    "2_constelacion")













################################################################################
################################################################################
##########                3. Las huellas (hash)          #######################
################################################################################
################################################################################

#Diapositivas: "Elegimos una estrella: el ancla", "Miramos una zona…",
#"Cada pareja es una huella" y "Así se arma el hash"

#Para cada estrella (ancla) buscamos las estrellas de su zona: entre 1 y 63 pasos adelante
#y a no más de ±120 renglones de frecuencia. Nos quedamos con las 10 primeras
#Cada pareja nos da 3 números: (f1, f2, dt) = (tono del ancla, tono de la otra, tiempo entre ellas)

armar_parejas <- function(picos) {
  zona <- picos[, .(t1 = t, f1 = f, t_lo = t + 1L, t_hi = t + DT_MAX, f_lo = f - DF_MAX, f_hi = f + DF_MAX)]
  #Join por rangos: para cada ancla, todas las estrellas que caen dentro de su zona
  par <- picos[zona, on = .(t >= t_lo, t <= t_hi, f >= f_lo, f <= f_hi),
               .(t1 = i.t1, f1 = i.f1, t2 = x.t, f2 = x.f), nomatch = 0L, allow.cartesian = TRUE]
  setorder(par, t1, f1, t2, f2)
  par[rowid(t1, f1) <= FAN_OUT]            #sólo las primeras FAN_OUT de cada ancla
}

#Hash: pegamos los 3 números en binario, uno tras otro (10 dígitos cada uno)
#f1 se recorre 20 lugares, f2 se recorre 10 lugares y dt se queda al final
#hash = f1 * 2^20 + f2 * 2^10 + dt (cabe en un entero de 32 bits, como en el paper)
calcular_hash <- function(f1, f2, dt) {
  bitwOr(bitwShiftL(as.integer(f1), 20L), bitwOr(bitwShiftL(as.integer(f2), 10L), as.integer(dt)))
}

#De picos a huellas: cada huella guarda su hash y en qué paso de tiempo apareció el ancla
picos_a_huellas <- function(picos) {
  par <- armar_parejas(picos)
  par[, .(hash = calcular_hash(f1, f2, t2 - t1), t1)]
}

#Veamos una pareja de cerca (como en la diapositiva "Así se arma el hash")
par <- armar_parejas(picos)
ej  <- par[t1 >= round((INICIO_DEMO + 3) / paso_a_seg(1))][1]   #una pareja 3 s después del inicio de la ventana
a_binario <- function(n, digitos = 10) paste(rev(as.integer(intToBits(n))[1:digitos]), collapse = "")
cat("\nUna pareja:\n",
    " tono A  =", round(renglon_a_hz(ej$f1)), "Hz  -> renglón", ej$f1, "-> binario", a_binario(ej$f1), "\n",
    " tono B  =", round(renglon_a_hz(ej$f2)), "Hz  -> renglón", ej$f2, "-> binario", a_binario(ej$f2), "\n",
    " tiempo  =", round(paso_a_seg(ej$t2 - ej$t1), 2), "s  -> pasos  ", ej$t2 - ej$t1, "-> binario", a_binario(ej$t2 - ej$t1), "\n",
    " HASH    =", calcular_hash(ej$f1, ej$f2, ej$t2 - ej$t1),
    "  (apareció en el segundo", round(paso_a_seg(ej$t1), 1), "de la canción)\n")

#Gráfica: un ancla y su zona
anc <- par[t1 == ej$t1 & f1 == ej$f1]
ver(ggplot(p10, aes(paso_a_seg(t), renglon_a_hz(f))) +
      geom_point(shape = 8, color = "gold", size = 2, alpha = 0.3) +
      annotate("rect", xmin = paso_a_seg(ej$t1 + 1), xmax = paso_a_seg(ej$t1 + DT_MAX),
               ymin = renglon_a_hz(ej$f1 - DF_MAX), ymax = renglon_a_hz(ej$f1 + DF_MAX),
               fill = NA, color = "white", linetype = 2) +
      geom_segment(data = anc, aes(x = paso_a_seg(t1), y = renglon_a_hz(f1), xend = paso_a_seg(t2), yend = renglon_a_hz(f2)),
                   color = "white", inherit.aes = FALSE) +
      geom_point(data = anc, aes(paso_a_seg(t2), renglon_a_hz(f2)), shape = 8, color = "gold", size = 3.5, inherit.aes = FALSE) +
      geom_point(data = anc[1], aes(paso_a_seg(t1), renglon_a_hz(f1)), shape = 8, color = "#FF3B6B", size = 6, inherit.aes = FALSE) +
      labs(title = "Un ancla (rosa), su zona y sus parejas", x = NULL, y = NULL) +
      theme_void() + theme(plot.background = element_rect(fill = "#0B0B14"), plot.title = element_text(color = "white")),
    "3_parejas")


#Todo el proceso en una función: de un archivo mp3 a sus huellas
huellas_de_archivo <- function(archivo) {
  x <- leer_audio(archivo)
  picos_a_huellas(encontrar_picos(espectrograma(x)))
}
#CCC: Continua sin regresar a la ppt

################################################################################
################################################################################
##########                4. El índice                   #######################
################################################################################
################################################################################

################################################################################
#IMPORTANTE: CREAMOS LA TABLA DE HASHES

#Diapositivas: "Paso 4: Guardar en un índice" y "Nuestro índice de huellas"

#Obtenemos TODAS las huellas de TODAS LAS CANCIONES DE LA BIBLIOTECA 
#y LAS ALMACENAMOS...

#Procesamos todas las canciones UNA sola vez y juntamos sus huellas en una tabla:
#hash | cancion | t1 (en qué paso de la canción apareció)
#setkey() la ordena por hash: buscar un hash ahí es como buscar una palabra en el índice de un libro
#Al final la guardamos en un .rds para no recalcularla en cada clase

# if (file.exists(ARCHIVO_INDICE)) {
#   indice <- readRDS(ARCHIVO_INDICE)
#   cat("Índice cargado de", ARCHIVO_INDICE, "\n")
# } else {
inicio <- Sys.time()
indice <- rbindlist(lapply(seq_along(archivos_canciones), function(i) {
  a <- archivos_canciones[i]
  h <- huellas_de_archivo(a)
  h[, cancion := tools::file_path_sans_ext(basename(a))]
  cat(sprintf("  [%2d/%d] %-30s %7d huellas\n", i, length(archivos_canciones), basename(a), nrow(h)))
  h
}))
indice[, cancion := factor(cancion)]   #guardar el nombre como factor ocupa mucho menos
setkey(indice, hash)
saveRDS(indice, ARCHIVO_INDICE)
cat("Índice listo en", round(difftime(Sys.time(), inicio, units = "mins"), 1), "min\n")
#}

cat("Huellas en la biblioteca:", format(nrow(indice), big.mark = ","),
    " · hashes distintos:", format(uniqueN(indice$hash), big.mark = ","), "\n")

#Observamos la tabla de hashes, segundos y canción en donde aparece
View(indice)





#Hashes que aparecen más veces
tabla_hashes <- table(indice$hash) %>%
  data.frame() %>%
  arrange(desc(Freq))

#Hashes más populares (aparecen en varias canciones o varias veces en una misma cancion)
head(tabla_hashes,30) %>%
  View()


#LO MÁS INTERESANTE: es que puedo volver a leer la biblioteca...
#algo así hace Shazam con millones de canciones "precalculadas"
biblioteca <- readRDS("indice_huellas.rds")

View(biblioteca) #algo así hace Shazam con millones de canciones "precalculadas"











################################################################################
################################################################################
##########                5. La votación                 #######################
################################################################################
################################################################################

#Diapositivas: "Contar coincidencias no basta", "el desfase", "los testigos",
#"Dibujemos a los testigos", "¿Cuántos testigos dicen cada número?" y "Gana la torre más alta"

#1. Sacamos las huellas de la muestra y las buscamos en el índice
#2. Cada huella que coincide es un TESTIGO que dice:
#   desfase = segundo en la canción - segundo en la muestra
#   ("yo creo que empezaste a grabar en el segundo ___")
#3. Contamos, para cada canción, cuántos testigos dicen el MISMO desfase
#4. Gana la canción con la torre más alta

buscar_testigos <- function(huellas_muestra, indice) {
  #Join por hash: cada huella de la muestra con todas sus apariciones en la biblioteca
  m <- indice[huellas_muestra, on = "hash", nomatch = 0L, allow.cartesian = TRUE,
              .(cancion, t_cancion = x.t1, t_muestra = i.t1)]
  #Desfase en segundos, redondeado a décimas: la muestra casi nunca empieza justo en el
  #mismo paso de 32 ms que la canción, así que agrupamos los que caen muy cerca
  m[, desfase := round(paso_a_seg(t_cancion - t_muestra), 1)]
  m
}

contar_votos <- function(testigos) {
  #Para cada canción: cuántos testigos coinciden en total y cuál es su torre más alta
  torres <- testigos[, .N, by = .(cancion, desfase)]
  res <- torres[order(-N), .(votos = N[1], desfase = desfase[1]), by = cancion]
  res <- merge(res, testigos[, .(coincidencias = .N), by = cancion], by = "cancion")
  res[order(-votos)]
}

#Regla para decidir "sí la conozco" o "no la conozco":
#El ganador necesita un mínimo de votos (una torre de verdad, no pasto)
#Y debe ganarle con claridad al segundo lugar
#Ajustamos estos dos números viendo los votos de las muestras que sabemos que NO están
#Ojo: sólo con el mínimo de votos no basta, una canción que no está puede juntar
#bastantes votos por casualidad. La VENTAJA sobre el segundo lugar es la que más ayuda
MIN_VOTOS <- 10
VENTAJA   <- 3      #el ganador debe tener al menos 3 veces los votos del segundo lugar

reconocer <- function(archivo, indice) {
  h <- huellas_de_archivo(archivo)
  testigos <- buscar_testigos(h, indice)
  if (nrow(testigos) == 0) return(list(veredicto = "no la conozco", ranking = NULL, testigos = testigos))
  ranking <- contar_votos(testigos)
  v1 <- ranking$votos[1]; v2 <- if (nrow(ranking) > 1) ranking$votos[2] else 0
  ok <- v1 >= MIN_VOTOS && v1 >= VENTAJA * max(v2, 1)
  list(veredicto = if (ok) as.character(ranking$cancion[1]) else "no la conozco",
       ranking = ranking, testigos = testigos, huellas = nrow(h))
}

####################################################################
######  5.1 Una muestra paso a paso
archivos_muestras <- list.files(CARPETA_MUESTRAS, pattern = "\\.mp3$", full.names = TRUE)
r <- reconocer(archivos_muestras[MUESTRA_EJEMPLO], indice)
cat("\nMuestra:", basename(archivos_muestras[MUESTRA_EJEMPLO]), "->", r$veredicto, "\n")

#Veamos las votaciones:
head(r$ranking, 5)






#Analizamos los resultados

#Contar coincidencias no basta: comparamos la columna coincidencias contra votos
#Una canción equivocada puede tener muchas coincidencias, pero sus testigos no se ponen de acuerdo

ganadora <- r$ranking$cancion[1]
otra     <- r$ranking[order(-coincidencias)][cancion != ganadora]$cancion[1]   #la equivocada con más coincidencias

#Canción ganadora
ganadora


#Segunda mejor opción
otra

#CCC: Sigue sin regresar a las notas

####################################################################
######  5.2 Dibujemos a los testigos (gráfica de puntos)

#Testigos de la cancion correcta vs un incorrecta

#Eje horizontal: segundo en la muestra. Eje vertical: segundo en la canción
#Canción correcta: los testigos verdaderos forman una LÍNEA (canción = muestra + desfase)
#Canción equivocada: puntos regados
d_scatter <- r$testigos[cancion %in% c(ganadora, otra)]
d_scatter[, tipo := factor(ifelse(cancion == ganadora, paste("Ganadora:", cancion), paste("Equivocada:", cancion)))]
d_scatter[, tipo := relevel(tipo, grep("^Equivocada", levels(tipo), value = TRUE))]
#Pintamos de rosa a los testigos que dicen el desfase ganador (los que dicen la verdad)
d_scatter[, dice_verdad := cancion == ganadora & desfase == r$ranking$desfase[1]]
ver(ggplot(d_scatter, aes(paso_a_seg(t_muestra), paso_a_seg(t_cancion), color = dice_verdad)) +
      geom_point(alpha = 0.7, size = 1.3) + facet_wrap(~tipo) +
      scale_color_manual(values = c(`FALSE` = "grey65", `TRUE` = "#FF3B6B"), guide = "none") +
      labs(title = "Cada punto es un testigo: en la ganadora aparece una línea", x = "segundo en la muestra", y = "segundo en la canción") + tema,
    "5_testigos")





#Testigos y números de votos
####################################################################
######  5.3 ¿Cuántos testigos dicen cada número? (histograma de desfases)

#Contamos cuántos testigos dicen cada desfase (con barras gruesas para que se vean)
d_hist <- d_scatter[, .N, by = .(tipo, desfase, dice_verdad)]
ver(ggplot(d_hist, aes(desfase, ymin = 0, ymax = N, color = dice_verdad)) + geom_linerange(linewidth = 0.9) +
      facet_wrap(~tipo) +
      scale_color_manual(values = c(`FALSE` = "grey30", `TRUE` = "#FF3B6B"), guide = "none") +
      labs(title = "Correcta: una torre · Equivocada: pasto bajito", x = "desfase (segundos)", y = "testigos (votos)") + tema,
    "5_histograma")



####################################################################
######  5.4 Gana la torre más popular y con cierta diferencia respecto al segundo lugar
top <- head(r$ranking, 10)
ver(ggplot(top, aes(reorder(cancion, votos), votos, fill = cancion == ganadora)) + geom_col() + coord_flip() +
      scale_fill_manual(values = c(`TRUE` = "#FF3B6B", `FALSE` = "grey70"), guide = "none") +
      labs(title = "Votos en la torre más alta", x = NULL, y = "votos") + tema,
    "5_votos")


#Ahora analizarmos todas las muestras vs la biblioteca de canciones
####################################################################
######  5.5 Analizamos todas las muestras vs la biblioteca de canciones
resultados <- rbindlist(lapply(archivos_muestras, function(a) {
  r <- reconocer(a, indice)
  rk <- r$ranking
  data.table(muestra      = basename(a),
             veredicto    = r$veredicto,
             mejor        = if (is.null(rk)) NA else as.character(rk$cancion[1]),
             votos        = if (is.null(rk)) 0L else rk$votos[1],
             segundo_lugar= if (is.null(rk) || nrow(rk) < 2) 0L else rk$votos[2],
             empieza_seg  = if (is.null(rk)) NA else rk$desfase[1])
}))

#Cancion asignada a cada muestra, de acuerdo a nuestra biblioteca
View(resultados)

#Observa que la cancion con ruido tiene menos huellas pero aun así, una cantidad IMPORTANTE
#que permite ASIGNARLA...

#Observa la diferencia respecto al primer y segundo lugar
resultados$votos

#Segundo lugar en votos...
resultados$segundo_lugar

















################################################################################
################################################################################
##########                Netflix                        #######################
################################################################################
################################################################################

####################################################################
######  Singular Value Descomposition
#https://en.wikipedia.org/wiki/Singular_value_decomposition
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_3_4")
#Leemos una BD con películas y usuarios

netflix <- read_excel("peli_netflix.xlsx")

#Observamos los datos
View(netflix)

#Conservamos solo los datos

#Peliculas: columnas
#Usuarios: registros
netflix <- netflix[2:nrow(netflix),2:ncol(netflix)]

#Cambiar nombres de columnas para apreciar mejor los registros
colnames(netflix) <- paste0("peli_", seq_len(ncol(netflix)))
#Cambiar nombres de filas para apreciar mejor los registros
rownames(netflix) <- paste0("user_", seq_len(nrow(netflix)))

#Observamos de nueva cuenta los datos
View(netflix)

#Número de peliculas y usuarios
dim(netflix)
#348 usuarios
#30 películas

#Alpicamos SDV a la matriz de datos
svd <- svd(netflix)

#Obtenemos las tres matrices de la descomposición
svd$d

svd$u

svd$v

#Matrices para cálculos Netflix
p <-  svd$u * sqrt(svd$d)
q <-  sqrt(svd$d) * t(svd$v)

#Observamos
View(p)

View(q)

#Con estos datos y algunas operaciones matriciales sería posible estimar 
#calificaciones de películas de usuarios que nos las han visto... (con cierto error...ECM)...
dim(p)# 348*30
dim(q)# 30*30



















