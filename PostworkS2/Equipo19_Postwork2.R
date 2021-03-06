
#Postwork 2, Equipo 19

# Ahora vamos a generar un c�mulo de datos mayor al que se ten�a, esta es una situaci�n 
#habitual que se puede presentar para complementar un an�lisis, siempre es importante 
#estar revisando las caracter�sticas o tipos de datos que tenemos, por si es necesario 
#realizar alguna transformaci�n en las variables y poder hacer operaciones aritm�ticas 
#si es el caso, adem�s de s�lo tener presente algunas de las variables, 
#no siempre se requiere el uso de todas para ciertos procesamientos.

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

library(dplyr)

lista <- lapply(lista, select, Date, HomeTeam:FTR)

# 4. Aseg�rate de que los elementos de las columnas correspondientes de los nuevos data frames
#   sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas).
#   Con ayuda de la funci�n rbind forma un �nico data frame que contenga las seis columnas mencionadas en el punto 3
#   (Hint 2: la funci�n do.call podr�a ser utilizada).

df_equipos <- do.call(rbind, lista)
df_equipos <- mutate(df_equipos, Date = as.Date(Date, "%d/%m/%y"))

          
