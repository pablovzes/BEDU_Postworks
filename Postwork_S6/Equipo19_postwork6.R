#POSTWORK 6

#Importa el conjunto de datos match.data.csv a R y realiza lo siguiente:
match<-read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-con-R-Santander/master/Sesion-06/Postwork/match.data.csv")
head(match)
attach(match)

#Agrega una nueva columna sumagoles que contenga la suma de goles por partido.
for (i in 1:nrow(match)){
  match$sumagoles[i]<-sum(home.score[i]+away.score[i])
}

#Obtén el promedio por mes de la suma de goles.
library("dplyr")
library(lubridate)

promedio<- match %>%
  mutate(Fecha = as.Date.character(date, "%Y-%m-%d"),
                    Año = year(Fecha),
                    Mes = month(Fecha)) %>%
  group_by(Año, Mes) %>%
  summarise(promediomensual=mean(sumagoles, na.rm = TRUE))


promedio<-as.data.frame(promedio)
promedio

#Crea la serie de tiempo del promedio por mes de la suma de goles hasta diciembre de 2019.
serie<-ts(promedio$promediomensual,start=c(2010,8),end=c(2019,12),fr=12)
serie

#Grafica la serie de tiempo.
ts.plot(serie, xlab = "Tiempo", ylab = "Promedio por mes", 
     main = "Promedio por mes de la suma de goles",
     sub = "hasta diciembre de 2019")
