

library(dplyr)

#LEEMOS LOS DATASETS
games_2017 <- read.csv("B1.1718.csv")
games_2018 <- read.csv("B1.1819.csv")
games_2019 <- read.csv("B1.1920.csv")

games_list = list(games_2017 = games_2017, games_2018 = games_2018,  games_2019 = games_2019)

#LO QUE HICIMOS EN EL POSTWORK 2 y 3 QUE NECESITAMOS PARA EL POSTWORK 4: 


#creamos la funci?n para pasarla a lapply
select_columns <- function(data_frame){
  df <- data_frame %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR) 
  return(mutate(df, Date = as.Date(Date, "%d/%m/%y")))
}

#aplicamos la selecci?n de columnas y la transformaci?n de fechas a todos los 
#dataframes 
games_list <- lapply(games_list, FUN = select_columns)

futbol <- do.call(rbind, games_list)

#extraemos las columnas solicitadas
golesCasa <- futbol$FTHG
golesVisitante <- futbol$FTAG

#calculamos el n?mero de partidos para calcular las probabilidades
totalPartidos <- length(golesCasa)

#hacemos las tablas de frecuencia
frecGC <- table(golesCasa)
frecGV <- table(golesVisitante)
frecConjunta <- table(golesCasa, golesVisitante)

#hacemos las tablas de probabilidades
probGC <- frecGC/totalPartidos
probGV <- frecGV/totalPartidos
probConjunta <- frecConjunta/totalPartidos

prodsGC_GV <- probGC %o% probGV

dependencia <- probConjunta/prodsGC_GV


#A PARTIR DE AQUÍ EMPIEZA EL POSTWORK 4


#bootstraping
sample_size = totalPartidos
repetition_size = 100


dependencias <- list()
#encontramos tablas similares de frecuencias (como dataframes)
for(i in 1:repetition_size)
{
  golesCasa2 <- sample(golesCasa, size = sample_size, replace=TRUE)
  golesCasa2 <- factor(golesCasa2, levels = min(golesCasa):max(golesCasa))
  golesVisitante2 <- sample(golesVisitante, size = sample_size, replace=TRUE)
  golesVisitante2 <- factor(golesVisitante2, levels = min(golesVisitante):max(golesVisitante))
  frecGC2 <- table(golesCasa2)
  frecGV2 <- table(golesVisitante2)
  frecConjunta2 <- table(golesCasa2, golesVisitante2)
  probGC2 <- frecGC2/sample_size
  probGV2 <- frecGV2/sample_size
  probConjunta2 <- frecConjunta2/sample_size
  
  prodsGC_GV2 <- probGC2 %o% probGV2
  dependencias[[i]] <- as.data.frame(probConjunta2/prodsGC_GV2)[,"Freq"]
}

#Concatenamos todas por columnas
df <- do.call("cbind", dependencias)

#Hallamos la media de nuestros estimadores  \hat P(X, Y) (omitiendo valores nulos que pueden haber por divisiones 0/0)
means_df <- rowMeans(df, na.rm = TRUE)
means_df

#Podemos observar que para casi todas las entradas obtenemos un cociente cercano a 1.
#Por lo que podr?amos decir que las variables s son independientes, pero para tener un s?lo estimador 
#calculamos la media ?de todos los \overline P(X, Y) y vemos si es cercano a 1. 

meanMeans <- mean(means_df)
meanMeans

#En este run se obtuvo 0.9302, lo cual es un buen indicador de que las variables son independientes 
