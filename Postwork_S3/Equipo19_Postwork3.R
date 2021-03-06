
#Postwork 2, Equipo 19

# Ahora vamos a generar un c�mulo de datos mayor al que se ten�a, esta es una situaci�n 
#habitual que se puede presentar para complementar un an�lisis, siempre es importante 
#estar revisando las caracter�sticas o tipos de datos que tenemos, por si es necesario 
#realizar alguna transformaci�n en las variables y poder hacer operaciones aritm�ticas 
#si es el caso, adem�s de s�lo tener presente algunas de las variables, 
#no siempre se requiere el uso de todas para ciertos procesamientos.

library(dplyr)
library(ggplot2)

# 1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la 
#primera divisi�n de la liga espa�ola a R, los datos los puedes encontrar en el siguiente enlace: 
#https://www.football-data.co.uk/spainm.php

lista <- lapply(dir(), read.csv)

# 2. Obten una mejor idea de las caracter�sticas de los data frames al usar las funciones: str, head, View y summary

str(lista) 
head(lista)
View(lista)
summary(lista)

# 3. Con la funci�n select del paquete dplyr selecciona �nicamente las columnas Date, 
#     HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: tambi�n puedes usar lapply).


lista <- lapply(lista, select, Date, HomeTeam:FTR)

# 4. Aseg�rate de que los elementos de las columnas correspondientes de los nuevos data frames
#   sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas).
#   Con ayuda de la funci�n rbind forma un �nico data frame que contenga las seis columnas mencionadas en el punto 3
#   (Hint 2: la funci�n do.call podr�a ser utilizada).

df_equipos <- do.call(rbind, lista)
df_equipos <- mutate(df_equipos, Date = as.Date(Date, "%d/%m/%y"))

# 1.Con el �ltimo data frame obtenido en el postwork de la sesi�n 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

#extraemos las columnas solicitadas
golesCasa <- df_equipos$FTHG
golesVisitante <- df_equipos$FTAG

#calculamos el n�mero de partidos para calcular las probabilidades
totalPartidos <- length(golesCasa)

#hacemos las tablas de frecuencia
frecGC <- table(golesCasa)
frecGV <- table(golesVisitante)
frecConjunta <- table(golesCasa, golesVisitante)

#hacemos las tablas de probabilidades
probGC <- frecGC/totalPartidos
probGV <- frecGV/totalPartidos
probConjunta <- frecConjunta/totalPartidos

#   La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

df_GC <- data.frame(Goles = names(probGC), probabilidad = (probGC))
df_GC <- df_GC[-c(2)]
df_GC <- rename(df_GC, Frecuencia = probabilidad.Freq)

#   La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

df_GV <- data.frame(Goles = names(probGV), probabilidad = (probGV))
df_GV <- df_GV[-c(2)]
df_GV <- rename(df_GV, Frecuencia = probabilidad.Freq)

#   La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)

df_probConjunta <- data.frame(probConjunta)


# Realiza lo siguiente:

#   Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de goles que anota el equipo de casa

df_GC %>%
  ggplot() +
  aes(x = Goles, y = Frecuencia) +
  geom_col(col ="black", fill = "yellow") +
  ggtitle("Probabilidades Marginales: Equipos de casa") +
  theme_gray()
  
# Un gr�fico de barras para las probabilidades marginales estimadas del n�mero de goles que anota el equipo visitante.

df_GV %>%
  ggplot() +
  aes(x = Goles, y = Frecuencia) +
  geom_col(col ="black", fill = "blue") +
  ggtitle("Probabilidades Marginales: Equipos visitantes") +
  theme_gray()

# Un HeatMap para las probabilidades conjuntas estimadas de los n�meros de goles que anotan el equipo de casa y el equipo visitante en un partido.
          
df_probConjunta %>%
  ggplot() +
  aes(x = golesCasa, y = golesVisitante, fill = Freq) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Goles equipo de Casa", y = "Goles equipo visitante", fill = "Frecuencia") +
  ggtitle("Heatmap de probabilidad conjunta") +
  theme_gray()
  