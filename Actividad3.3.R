# Actividad 3.3
# Jose Manuel Hernandez Gomez 
# Jorge Uriel Barragan Pozos 

# parametros 
n <- 100
lambda <- 1

#carga de datos
datos <- rexp(n,lambda)

#1. Análisis Descriptivo de los Datos 
summary(datos) 
hist(datos, freq = FALSE)
plot(ecdf(datos))

#2. Ajuste de Curvas de Probabilidad

mod1 <- fitdist(datos, "exp", method = c("mle"))
mod1
summary(mod1)

par(mfrow = c(2,2))
denscomp(mod1) # Función de Densidad
cdfcomp(mod1) #Función de Distribución 
qqcomp(mod1) #cuantiles Teóricos vs Empiricos 
ppcomp(mod1) #Probabilidades Teóricas vs Empiricas 
