#install.packages("mongolite")
library(mongolite)

data <- read.csv("data1.csv")
conn <- mongo(collection='match', db='match_games')


conn$insert(data)

count <- conn$count()
games <- conn$find('{"date" : "2015-12-20", "home_team":"Real Madrid"}')

conn$disconnect()

print("El número de registros en la base de datos es: ")
print(count)
print("\n")
print("Los registros que cumplen con las condiciones son:")
print(games)
print("El Real Madrid ganó por goleada al Vallecano")

