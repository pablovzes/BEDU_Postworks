
library(dplyr)
library(fbRanks)

#LEEMOS LOS DATASETS
games_2017 <- read.csv("SP1_1718.csv")
games_2018 <- read.csv("SP1_1819.csv")
games_2019 <- read.csv("SP1_1920.csv")

games_list = list(games_2017 = games_2017, games_2018 = games_2018,  games_2019 = games_2019)

str(games_2017)

#EXTRAEMOS LAS COLUMNAS SOLICITADAS


#creamos la función para pasarla a lapply
select_columns <- function(data_frame){
  df <- data_frame %>% select(Date, HomeTeam, AwayTeam, FTHG, FTAG) 
  return(mutate(df, Date = as.Date(Date, "%d/%m/%y")))
}


#aplicamos la selección de columnas y la transformación de fechas a todos los 
#dataframes 
games_list <- lapply(games_list, FUN = select_columns)

futbol <- do.call(rbind, games_list)
futbol <- futbol %>%
  rename(date=Date, home.team=HomeTeam, home.score=FTHG, away.team=AwayTeam, away.score=FTAG)
head(futbol)

#guardamos el dataframe en un directorio
filename="soccer.csv"
write.csv(futbol, filename, row.names = FALSE)

listasoccer <- create.fbRanks.dataframes(filename)
equipos <- listasoccer$teams
anotaciones <- listasoccer$scores

fecha <- unique(anotaciones$date) 
n <- length(fecha)

ranking <- rank.teams(anotaciones, equipos, min.date=min(fecha), max.date=max(fecha))

predict(ranking, date=fecha[n])
