#######################################################################################
#######################################################################################
################      CLASE 5-6                     ###################################
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

#IMPORTANTE

#Directorio de mi computadora...
#Yo haré el ejercicio con estas canciones
setwd("~/Desktop/Carlos/cursos/ITAM/Taller_MCD_agosto2026/Clase9/musica")

#IMPORTANTE

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

#Quito el paquete signal para que no haga conflicto
detach("package:signal", unload = TRUE)














#######################################################################################
#######################################################################################
############# Aprendizaje no supervisado (Clustering)   ###############################
#######################################################################################
#######################################################################################

#Nuevos paquetes
library(cluster) #Paquetes para apredizaje no supervisado
library(plotly) #Lo usaremos para gráficas 3D interactivas (se giran con el mouse) sin OpenGL/XQuartz

#Quito el paquete signal para que no haga conflicto (ejemplo Shazam)
detach("package:signal", unload = TRUE)

#################################################################################
######## Visualización de variables 

#Primero cargamos la bd de iris que está contenida en R

#Nota que como estos datos ya están integrados en R, no es necesario leerlos desde un archivo csv o xlsx
data(iris)

#Esta conjunto de datos contiene información sobre distintas flores y sus características
#Para más información consulta:
#https://www.statology.org/iris-dataset-r/

#Setosa: https://es.wikipedia.org/wiki/Echeveria_setosa
#Virginica: https://es.wikipedia.org/wiki/Iris_virginica

#Observamos la información
View(iris)

#Asignamos los datos en una variable llamada data
data <- iris

#Tenemos datos sobre 150 plantas
nrow(data)

#Y 4 variables que describen a nuestras plantas
ncol(data) - 1

#Varibles
colnames(data)[1:4]


#Visualizamos las plantas con 3 variables en una gráfica en 3D

#Ahora Coloco una variable con un color uniforme...
data$color_unif <- 'royalblue1'

#Gráfica en 3 dimensiones
plot_ly( 
  data = data,
  x = ~`Sepal.Length`, y = ~`Sepal.Width`, z = ~`Petal.Length`, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = "royalblue", size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "Sepal Length"),
    yaxis = list(title = "Sepal Width"),
    zaxis = list(title = "Petal Length")))

###TOMA UN MINUTO Y Gira la gráfica y observa...
### De forma intuitiva... ¿Cuántos GRUPOS distintos de flores crees que hay?
#¿Por qué?





#Ahora repetimos la gráfica pero les asigno un color de acuerdo a la etiqueta (el tipo de planta...)
mycolors <- c("blue", "darkcyan", "red")
#A cada tipo de flor, le asigno un color
data$color <- mycolors[ as.numeric(data$Species) ]

#Esta es la etiqueta "real"... de acuerdo a la clase verdadera de planta:

#Gráfica en 3 dimensiones
plot_ly( 
  data = data,
  x = ~`Sepal.Length`, y = ~`Sepal.Width`, z = ~`Petal.Length`, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = data$color, size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "Sepal Length"),
    yaxis = list(title = "Sepal Width"),
    zaxis = list(title = "Petal Length")))

#Observa que si acomodamos de cierta forma la gráfica anterior, nos podría dar una de 2 dimensiones
#...



#Nota tambien que podrías cambiar las variables que se están graficando

#Ahora podemos incluir Petal.Width:

plot_ly( 
  data = data,
  x = ~`Sepal.Length`, y = ~`Sepal.Width`, z = ~Petal.Width, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = "royalblue", size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "Sepal Length"),
    yaxis = list(title = "Sepal Width"),
    zaxis = list(title = "Petal Width")))

#Observa que para cualquier conjunto de datos, podrías gráficas las variables que decidas...

#Nota que la última columna de la BD iris, te dice qué tipo de flor es:

iris$Species %>% table()#Tenemos 50 flores de cada tipo
















#######################################################################################
######## ML No Supervisado: Clustering ################################################
#######################################################################################

#Utilizaremos los mismos datos de IRIS
original <- iris

#Quitamos la columna con la etiqueta
#Creamos dos conjuntos de datos (original y uno sin la información sobre el tipo de flor)
datos <- select(iris,-Species)

#OBSERVA QUE ESTOY QUITANDO LA ETIQUETA CON LA CLASE "VERDADERA"...
#VAMOS A TRATAR DE APROXIMARLA CON CLUSTERING...


#Estamos solicitando un análisis de conglomerados (clustering) de 3 grupos
model=kmeans(datos,3)

#SÍ, ASÍ DE FÁCIL SE CORRE EL ALGORITMO DE CLUSTERING...


#¿Que información nos entrega?

#Nos dice a qué cluster asignó cada registro...
model$cluster

#Podemos pegar esta información a nuestra base de datos
datos <- cbind(datos,c_3 = model$cluster)
View(datos)

#¿Cuantos registros hay por cada cluster?
table(datos$c_3)


##############################
#¿Qué pasa si ahora utilizamos 2 grupos?
datos <- select(iris,-Species)

#Mismas instrucciones que antes:
model=kmeans(datos,2)

#Pegamos los resultaos
datos <- cbind(datos,c_2 = model$cluster)
View(datos)

#¿Cuantos registros hay por cada cluster?
table(datos$c_2)



##############################
#¿Qué pasa si ahora utilizamos 4 grupos?
datos <- select(iris,-Species)

model=kmeans(datos,4)


#Pegamos los resultaos
datos <- cbind(datos,c_4 = model$cluster)
View(datos)

#¿Cuantos registros hay por cada cluster?
table(datos$c_4)


#La GRAN pregunta es...

#¿Cuál es el número óptimo de grupos? ¿3,4,5, o cuántos?...














#######################################################################################
### Withinss y métricas adicionales

#Utilizamos un cluster de 3 grupos
datos <- select(iris,-Species)
model=kmeans(datos,3)

#La información sobre los withinss está asociada en el modelo

#Cada conglomerado tiene su propio withinss
model$withinss

#Y también se puede estimar uno total 
#NOTA: CON EL WITHINSS TOTAL SE TOMA LA DECISIÓN DEL NÚMERO DE GRUPOS A TOMAR...
model$tot.withinss



### Centroides
#También puedes obtener las coordenadas de los centroides de cada grupo/cluster
model$centers


#######################################################################################
### Gráfica de codo
datos <- select(iris,-Species)

#Podemos correr el modelo de clustering con distintos grupos y 
#almacenar los distintos withinss totales

#Nota: esto también se podría hacer con un for...

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

#¿Cuál es el número de clusteres o grupos óptimo?...


#Asignamos el modelo
datos$cluster <- model_3$cluster

#¿Cuántos miembros tenemos por cada cluster?
table(datos$cluster)


### Pregunta final:
#¿Qué efecto tiene aumentar el número de clústeres o grupos?

table(model_1$cluster)

table(model_2$cluster)

table(model_3$cluster)

table(model_4$cluster)

table(model_5$cluster)

table(model_6$cluster)

### 1. ¿Qué está sucediendo con nuestros grupos y miembros?
### 2. ¿Qué efectos prácticos tiene en un caso de uso real?













#######################################################################################
#######################################################################################
######## Aclaración clustering y conversión de variables  #############################
#######################################################################################
#######################################################################################

setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")

#Leemos los datos
datos <- read_excel("ejemplo_categoricas_onehot.xlsx") %>% 
  select(1:4)

#Matriz de datos sobre clientes y el tipo de auto que compraron a la agencia
View(datos)

#Observa que son datos mixtos...
str(datos)

#Por esta razón no podemos aplicar distancia euclidiana...
#AUTO es una variable catagórica...

#¿Qué pasa si haces one-hot encodingo para convertir AUTO en numérica?
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
datos <- read_excel("ejemplo_categoricas_onehot.xlsx")
#Nota: se puede convertir también en R...

View(datos)

#Tipos de auto
table(datos$Auto)
# BMW CHEVROLET    NISSAN 
# 11         9         9 

#Haciendo one hot enconding parece que ahora tenemos "datos numéricos"...
datos <- datos %>% 
  select(-Auto)

#Observamos
View(datos)

#Incluso R lo reconoce como un número...
str(datos)


#######################################################################################
#Pregunta: ¿Por qué no tenemos una columna con CHEVROLET...?





#Pero veamos la siguiente gráfica en 3D:
datos$color_unif <- 'royalblue1'

plot_ly( 
  data = datos,
  x = ~`Salario`, y = ~`Monto Deuda`, z = ~`BMW`, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = "royalblue", size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "Salario"),
    yaxis = list(title = "Monto Deuda"),
    zaxis = list(title = "BMW")))



#Pregunta: ¿Observas algo extraño?...

#Probemos otras variables...
datos$color_unif <- 'royalblue1'

plot_ly( 
  data = datos,
  x = ~`NISSAN`, y = ~`Monto Deuda`, z = ~`BMW`, 
  type = "scatter3d",
  mode = "markers",
  marker = list(color = "royalblue", size = 4)
) %>%
  layout(scene = list(
    xaxis = list(title = "NISSAN"),
    yaxis = list(title = "Monto Deuda"),
    zaxis = list(title = "BMW")))

#Pregunta: 
#Espacialmente, ¿consideras que los CENTROIDES multidimensionales tienen sentido en este clase de problema?...


#Incluso veamos la distancia entre el registro 1 y 2:
datos[1:2,] %>% 
  select(2:5)

#Recuerda que la distancia NOS DA UNA NOCIÓN DE AGRUPAMIENTO O CLUSTER...


#Considerando las variables BMW y NISSAN, ¿cuál es la distancia entre el registro 1 y 2?

#Registro 1
datos[1,4:5]
#Registro 2
datos[2,4:5]

#Distancia Euclidiana
sqrt(sum((datos[1,4:5] - datos[2,4:5])^2))
#O sea, raíz cuadrada de 2

#Pregunta crucial:
#¿Qué significa que la distancia entre el usuario 1 y 2, 
#considerando sus variables NISSAN Y BMW sea igual a 1.414214?

















#ABSOLUTAMENTE NADA...

#Lo peligroso es que si hicieras clustering con las variables one-hot encoding...
#el algoritmo sí correría...

setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
datos <- read_excel("ejemplo_categoricas_onehot.xlsx") %>% 
  select(c(2,3,5,6))

#Observamos los datos:
View(datos)

#Aplicamos clustering con el número de grupos que sea...
mal <- kmeans(datos,4)

mal$cluster

mal$centers #Observa las coordenadas de sus centroides... ????

#Incluso cambiando el número de grupos...
kmeans(datos,5)

kmeans(datos,6)

#Error común y bastante cuestionable...



#Metodológicamente es un error... peroo, tiene solución


#######################################################################################
# Clustering con distancia de Gower (métrica cuando tenemos variables mixtas: numéricas + categóricas)

#Cargamos nuevamente los datos:

setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
#Variables original sin transformación one-hot encoding:
datos <- read_excel("ejemplo_categoricas_onehot.xlsx") %>% 
  select(Salario:Auto)

View(datos)

#Revisamos el tipo de datos
str(datos)


#######################################################################################
#Opción 1: Clustering con variables mixtas

# Distancia Gower
d <- daisy(datos,
           metric = "gower") #Indicamos que es Gower distance...

#¿Qué crees que está sucediendo?...










#La conversión para este tipo de datos como factor es Obligatoria:
datos$Auto <- as.factor(datos$Auto)

#Revisamos el tipo de datos
str(datos)

# Distancia Gower
d <- daisy(datos,
           metric = "gower") #Distancia Gower...

# Clustering
pam_fit <- pam(d,
               k = 3) #Pedimos 3 grupos...

#Resultados
pam_fit$clustering





#######################################################################################
#Opción 2: Podemos utilizar otro algoritmo diseñado especialmente para variables mixtas...

library(clustMixType)

#Cargamos nuevamente los datos:
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")

#Variables original sin transformación one-hot encoding:
datos <- read_excel("ejemplo_categoricas_onehot.xlsx") %>% 
  select(Salario:Auto)

#La conversión para este tipo de datos como factor es Obligatoria:
datos$Auto <- as.factor(datos$Auto)

#Clustering con 3 grupos
kp <- kproto(datos, k = 3)

#De nueva cuenta...
datos$Auto <- as.factor(datos$Auto)


#Clustering con 3 grupos
kp <- kproto(datos, k = 3)

#Resultados
kp$cluster



#Investiga como se mide Gower y analiza sus diferencias respecto al enfoque one-hot enconding


#######################################################################################
#Importante: Novedad
#######################################################################################

https://x.com/swann_bessa/status/2120200714630594911

https://www.alphaxiv.org/abs/2611.09501



















#######################################################################################
#######################################################################################
######## Error Entrenamiento + Prueba            ######################################
#######################################################################################
#######################################################################################

#Semilla
set.seed(1035)


setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
#Entrenamiento
train <- read_excel("deptos_train.xlsx")
View(train) #Ya conocemos estos datos
#Tamaño
dim(train)


#Con estos datos de entrenamiento vamos a construir un modelo de ML
#y luego lo probaremos con otros datos



#ML: Árbol decisión con conjunto de entrenamiento

#Convertimos a factor la etiqueta
train$in_sf <- factor(train$in_sf)

#Corremos un árbol de decisión
tree_train <- rpart(in_sf ~ ., data = train)

#Predicción sobre la etiqueta
train$pred <- predict(object = tree_train,newdata = train,type = "class")

#Matriz de confusion

#Comando nuevo para obtener la matriz de confusión
conf_train <- confusionMatrix(data=train$pred, reference = train$in_sf)

#Nota todas las métricas que obtienes
conf_train$table

#Supongamos que Accuracy es la métrica más importante para nosotros
conf_train$overall[1]
# Accuracy 
# 0.9238329


#### ¿Qué pasa si ahora medimos el Accuracy de las predicciones de este modelo contra
#   observaciones que "no ha visto el algoritmo"? Es decir, contra el conjunto de Prueba...

#  ¿Qué podrías esperar, el accuray es más alto o más bajo?...

#Cargamos datos que no ha visto el algoritmo de los datos de entrenamiento...
#Datos de Prueba
test <- read_excel("deptos_test.xlsx")

View(test)

#Tamaño
dim(test)

#Convertimos a factor la etiqueta
test$in_sf <- factor(test$in_sf) #Recuerda que este paso no se realiza con los algoritmos de regresión

#Aplicamos el modelo de ML (calibrado con los datos de entrenamiento) sobre
#los datos del conjunto de prueba
test$pred <- predict(object = tree_train,newdata = test,type = "class")
#Este paso es CRUCIAL: por primera vez, hacemos predicciones sobre nuevos datos "test" (no vs train...)

#Matriz de confusion
#Comando nuevo para obtener la matriz de confusión
conf_test <- confusionMatrix(data=test$pred, reference = test$in_sf)
#Accuracy con el conjunto de prueba
conf_test$overall[1]

#Comparacion Accuracy train v.s. Accuracy test
conf_train$overall[1]

conf_test$overall[1]

#El "error" es menor en el conjunto de entrenamiento (un accuracy más alto)...
#en comparación con el "error" en el conjunto de prueba...

#¿Qué crees que pase en las demás métricas?

#Reflexiona: ¿Este fenómeno se repite siempre? ¿Por qué?
#¿Este fenómeno también ocurre con otros algoritmos de ML o solo con los árboles?...
















#######################################################################################
######## Separación Ent + Prueba             ##########################################
#######################################################################################

setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")

accion <- read_excel("acciones_Twitter.xlsx") %>%
  select(-Fecha) #Quitamos, por ahora, el campo de Fecha

dim(accion)

View(accion)

#Enfoque: Separación aleatoria...

#Entrenamiento
ent <- sample_frac(tbl = accion,size = 0.8)
dim(ent)

#ID seleccionamos como entrenamiento
ent$ID

#Prueba
#El resto de los registros es prueba
pru <- accion %>% filter(!ID %in% ent$ID)

dim(pru)

#Comprobamos que son conjuntos disjuntos
intersect(ent$ID,pru$ID)


#¿Identificas algún posible error metodológico por la forma en que separas estos datos?...

















#######################################################################################
######## Separación Temporal Ent + Prueba            ##################################
#######################################################################################

#Separamos el conjunto entrenamiento y prueba de forma secuencial, no aleatorio...

#Leemos el archivo
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_noviembre2026/Clase_5_6")
accion <- read_excel("acciones_Twitter.xlsx")

#Ordenamos por fecha
accion <- arrange(accion,Fecha)
#Observa que el conjunto debe ser ordenado de la fecha mas antigua a la más reciente...
View(accion)

#limite
aux <- (nrow(accion)*.8) %>%
  floor()#Hacemos explícito el registro que concentra el 80% de los datos

aux

#Entrenamiento
ent <- accion[1:aux,]
dim(ent)

#Prueba
pru <- accion[(aux+1):nrow(accion),]
dim(pru)

#Esta es la forma correcta para separar cuando hay riesgo de filtracion por datos con fecha...












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








