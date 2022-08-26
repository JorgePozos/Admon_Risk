## Actividad 2.2
library(dplyr)
library(tidyverse)
library(fitdistrplus) #Ajuste de curvas de Probabilidad
library(goftest) #Pruebas de bondad de ajuste 

# Importamos los datos 
datos_og <- read.csv("Downloads/SBUX.csv")
datos_og$Date <- as.Date(datos_og$Date) #formato de fecha

# Filtramos año actual 
datos <- datos_og %>% 
filter(Date >= "2022-01-01")
# Por variable, encontrar:
# Filtrar año actual 
# Histograma
# Densidades
# Boxplot
# Cuartiles 


# Obtenemos estadistica descriptiva 
est_des<- function(var = "Date"){
  switch (var,
    "Date" = summary(datos$Date),
    "Open"= summary(datos$Open),
    'High'=summary(datos$High),
    'Low'= summary(datos$Low),
    'Close' = summary(datos$Close),
    'Adj.close' = summary(datos$Adj.Close),
    'Volume' = summary(datos$Volume)
  )
}

# Histograma con Densidad 
datos %>%
  ggplot(aes(Open)) + 
  geom_histogram(bins= 10, aes(y=..density..)) + 
  geom_density(color= 'blue') + 
  geom_vline(aes(xintercept=  est_des('Open')[4] ),#Media
             color="blue", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des('Open')[2] ),#Primer Cuartil
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des('Open')[3] ), #Mediana
             color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=  est_des('Open')[5] ),#Tercer Cuartil
             color="red", linetype="dashed", size=1)
# Boxplot 
datos %>%
  ggplot(aes(Open)) + 
  geom_boxplot()

est_des('Open')
