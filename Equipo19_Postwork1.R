#Leemos el dataset
futbol <- read.csv("SP1.csv")

#extraemos las columnas solicitadas
golesCasa <- futbol$FTHG
golesVisitante <- futbol$FTAG

#calculamos el número de partidos para calcular las probabilidades
totalPartidos <- length(golesCasa)

#revisamos la documentación de la función table
?table

#hacemos las tablas de frecuencia
frecGC <- table(golesCasa)
frecGV <- table(golesVisitante)
frecConjunta <- table(golesCasa, golesVisitante)

#hacemos las tablas de probabilidades
probGC <- frecGC/totalPartidos
probGV <- frecGV/totalPartidos
probConjunta <- frecConjunta/totalPartidos
print(probGC)
print(probGV)
print(probConjunta)



