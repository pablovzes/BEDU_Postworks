#PROYECTO BEDU 
#ANÁLISIS PREDICTIVO PARA UNA EMPRESA DE TELECOMUNICACIONES
library("dplyr")
library(ggplot2)
customer<-read.csv("Telco-Customer-Churn.csv")
str(customer)
attach(customer)

## Tipo de contrato de los clientes que rescinden de su contrato
Contratos <- table(Churn, Contract)
Contratos

Terminados <- customer %>% filter(Churn == "Yes") %>% select(Churn, Contract)
Terminados %>% 
  ggplot() +
  aes(Churn, fill = Contract)+
  geom_bar(position = "dodge") +
  ggtitle("Clientes con contrato terminado") +
  xlab("Rescidieron del contrato")+
  ylab("Total de Clientes")

#Proporción por contratos, terminados vs actuales

prop.contratos <- prop.table(Contratos, margin = 2)
prop.contratos

dprop.contratos <- as.data.frame(prop.contratos)
dprop.contratos %>% 
  ggplot() +
  aes(fill = Churn, x = Contract, y = Freq) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Clientes por tipo de contrato") +
  xlab("Tipo de Contrato") +
  ylab("Frecuencia")

# De las personas que rescinden de su contrato: 
# ¿qué proporción contrató línea telefónica y que proporción internet?
telefono<-table(Churn,PhoneService)
telefono

#proporción de clientes con el servicio telefónico
prop.tel<-prop.table(telefono,margin=2)
prop.tel

#gráfica de la proporción clientes con el servicio telefónico
prop.tel<-as.data.frame(prop.tel)
prop.tel
prop.tel%>% 
  ggplot() +
  aes(fill = Churn, x = PhoneService, y = Freq) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Proporción de clientes con el servicio de teléfono") +
  xlab("Servicio de teléfono") +
  ylab("Proporción")

#gráfica de clientes con el servicio telefónico
customer %>% 
  ggplot() +
  aes(Churn, fill = PhoneService) +
  geom_bar(position = "dodge") +
  ggtitle("Clientes con el servicio de teléfono") +
  xlab("Abandono de contrato") +
  ylab("Número de clientes")


internet<-table(Churn,ifelse(InternetService!="No","Sí","No"))
internet

#proporción de clientes con el servicio de internet
prop.int<-prop.table(internet,margin=2)
prop.int

#gráfica dela proporción de clientes con el servicio de internet
prop.int<-as.data.frame(prop.int)
prop.int
prop.int%>% 
  ggplot() +
  aes(fill = Churn, x = Var2, y = Freq) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Proporción de clientes con el servicio de internet") +
  xlab("Servicio de internet") +
  ylab("Proporción")

#gráfica de clientes con el servicio de internet
customer %>% 
  ggplot() +
  aes(Churn, fill = InternetService) +
  geom_bar(position = "dodge") +
  ggtitle("Clientes con el servicio de internet") +
  xlab("Abandono de contrato") +
  ylab("Número de clientes")


#-- Análisis de clientes principales --

# Sacamos el valor promedio de un cliente para la compañía, multiplicando el
# promedio de pago mensual por el promedio de duración de contrato.

prom_duracion <- mean(customer$tenure)
prom_pagoMensual <- mean(customer$MonthlyCharges)
valorCliente <- prom_duracion * prom_pagoMensual

# Filtramos aquellos clientes que hayan hecho un pago por encima del promedio
# para analizar que variables tienen en común esos clientes.

clientes_principales <- filter(customer, TotalCharges >= valorCliente)

# Analizamos graficamente las variables de los clientes principales

clientes_principales %>%
  ggplot() +
  aes(PhoneService, fill = MultipleLines) +
  geom_bar() +
  ggtitle("Contrato de línea telefónica") +
  xlab("Línea telefónica") +
  ylab("Cantidad de clientes") +
  scale_fill_discrete(name = "Multiples líneas telefónicas")

# La gran mayoría de nuestros clientes principales no solo tienen línea telefónica,
# tienen múltiples


# La mayoría de nuestros clientes principales se conectan por fibra óptica. 
# Analizaremos la cantidad de clientes que contratan servicios adicionales.

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = OnlineSecurity) +
  geom_bar() +
  ggtitle("Internet + Seguridad Online") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes") +
  scale_fill_discrete(name = "Seguridad Online")


clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = OnlineBackup) +
  geom_bar() +
  ggtitle("Internet + Online Backup") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes") +
  scale_fill_discrete(name = "Online Backup")

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = DeviceProtection) +
  geom_bar() +
  ggtitle("Internet + Porteccion de Dispositivo") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Protección de Dispositivo")

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = TechSupport) +
  geom_bar() +
  ggtitle("Internet + Apoyo técnico") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Apoyo Técnico")

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = StreamingTV) +
  geom_bar() +
  ggtitle("Internet + Streaming de Televisi?n") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Streaming TV")

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = StreamingMovies) +
  geom_bar() +
  ggtitle("Internet + Streaming de Películas") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Streaming Películas")

# Tipo de contrato de nuestros clientes principales 

clientes_principales %>%
  ggplot() +
  aes(Contract) +
  geom_bar(fill = "blue") +
  ggtitle("Tipos de contrato") +
  labs(x = "Contrato", y = "Cantidad de clientes")

# Tipo de recibo

clientes_principales %>%
  ggplot() +
  aes(PaperlessBilling) +
  geom_bar(fill = "dark green") +
  ggtitle("Recibo digital") +
  labs(x = "Recibo digital", y = "Cantidad de clientes")

# Método de pago de preferencia

clientes_principales %>%
  ggplot() +
  aes(PaymentMethod) +
  geom_bar(fill = "dark blue") +
  ggtitle("Método de pago") +
  labs(x = "", y = "Cantidad de clientes")


#Correlaciones
chisq.test(customer$gender, customer$Churn, correct=FALSE)
chisq.test(customer$Partner, customer$Churn, correct=FALSE)
chisq.test(customer$Dependents, customer$Churn, correct=FALSE)
chisq.test(customer$PhoneService, customer$Churn, correct=FALSE)
chisq.test(customer$InternetService, customer$Churn, correct=FALSE)
chisq.test(customer$Contract, customer$Churn, correct=FALSE)
chisq.test(customer$PaymentMethod, customer$Churn, correct=FALSE)