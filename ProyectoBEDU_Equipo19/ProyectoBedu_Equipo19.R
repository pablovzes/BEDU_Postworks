#PROYECTO BEDU 
#AN�LISIS PREDICTIVO PARA UNA EMPRESA DE TELECOMUNICACIONES
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

#Proporci�n por contratos, terminados vs actuales

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
# �qu� proporci�n contrat� l�nea telef�nica y que proporci�n internet?
telefono<-table(Churn,PhoneService)
telefono

#proporci�n de clientes con el servicio telef�nico
prop.tel<-prop.table(telefono,margin=2)
prop.tel

#gr�fica de la proporci�n clientes con el servicio telef�nico
prop.tel<-as.data.frame(prop.tel)
prop.tel
prop.tel%>% 
  ggplot() +
  aes(fill = Churn, x = PhoneService, y = Freq) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Proporci�n de clientes con el servicio de tel�fono") +
  xlab("Servicio de tel�fono") +
  ylab("Proporci�n")

#gr�fica de clientes con el servicio telef�nico
customer %>% 
  ggplot() +
  aes(Churn, fill = PhoneService) +
  geom_bar(position = "dodge") +
  ggtitle("Clientes con el servicio de tel�fono") +
  xlab("Abandono de contrato") +
  ylab("N�mero de clientes")


internet<-table(Churn,ifelse(InternetService!="No","S�","No"))
internet

#proporci�n de clientes con el servicio de internet
prop.int<-prop.table(internet,margin=2)
prop.int

#gr�fica dela proporci�n de clientes con el servicio de internet
prop.int<-as.data.frame(prop.int)
prop.int
prop.int%>% 
  ggplot() +
  aes(fill = Churn, x = Var2, y = Freq) +
  geom_bar(position = "fill", stat = "identity") +
  ggtitle("Proporci�n de clientes con el servicio de internet") +
  xlab("Servicio de internet") +
  ylab("Proporci�n")

#gr�fica de clientes con el servicio de internet
customer %>% 
  ggplot() +
  aes(Churn, fill = InternetService) +
  geom_bar(position = "dodge") +
  ggtitle("Clientes con el servicio de internet") +
  xlab("Abandono de contrato") +
  ylab("N�mero de clientes")


#-- An�lisis de clientes principales --

# Sacamos el valor promedio de un cliente para la compa��a, multiplicando el
# promedio de pago mensual por el promedio de duraci�n de contrato.

prom_duracion <- mean(customer$tenure)
prom_pagoMensual <- mean(customer$MonthlyCharges)
valorCliente <- prom_duracion * prom_pagoMensual

# Filtramos aquellos clientes que hayan hecho un pago por encima del promedio
# para analizar que variables tienen en com�n esos clientes.

clientes_principales <- filter(customer, TotalCharges >= valorCliente)

# Analizamos graficamente las variables de los clientes principales

clientes_principales %>%
  ggplot() +
  aes(PhoneService, fill = MultipleLines) +
  geom_bar() +
  ggtitle("Contrato de l�nea telef�nica") +
  xlab("L�nea telef�nica") +
  ylab("Cantidad de clientes") +
  scale_fill_discrete(name = "Multiples l�neas telef�nicas")

# La gran mayor�a de nuestros clientes principales no solo tienen l�nea telef�nica,
# tienen m�ltiples


# La mayor�a de nuestros clientes principales se conectan por fibra �ptica. 
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
  scale_fill_discrete(name = "Protecci�n de Dispositivo")

clientes_principales %>%
  ggplot() +
  aes(InternetService, fill = TechSupport) +
  geom_bar() +
  ggtitle("Internet + Apoyo t�cnico") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Apoyo T�cnico")

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
  ggtitle("Internet + Streaming de Pel�culas") +
  labs(x = "Servicio de Internet", y = "Cantidad de clientes")  +
  scale_fill_discrete(name = "Streaming Pel�culas")

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

# M�todo de pago de preferencia

clientes_principales %>%
  ggplot() +
  aes(PaymentMethod) +
  geom_bar(fill = "dark blue") +
  ggtitle("M�todo de pago") +
  labs(x = "", y = "Cantidad de clientes")


#Correlaciones
chisq.test(customer$gender, customer$Churn, correct=FALSE)
chisq.test(customer$Partner, customer$Churn, correct=FALSE)
chisq.test(customer$Dependents, customer$Churn, correct=FALSE)
chisq.test(customer$PhoneService, customer$Churn, correct=FALSE)
chisq.test(customer$InternetService, customer$Churn, correct=FALSE)
chisq.test(customer$Contract, customer$Churn, correct=FALSE)
chisq.test(customer$PaymentMethod, customer$Churn, correct=FALSE)