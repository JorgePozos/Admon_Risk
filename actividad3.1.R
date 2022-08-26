# actividad 3.1
library(knitr)
# INPUTS
n <- 4 #<- n tiempo de poliza 
x <- 45 # x <- edad de la persona
k <- 99 # k <- numero de v.a. Ti's
alpha <- .4 #  alpha 
lambda <- .5 # lambda 
delta <- .4 # fuerza de interes 

# simulaciones de tk distribucion gamma con alpha y lambda 
simulaciones <- rgamma(k,alpha, lambda)
datos<- array()
for (i in 1:k) {
  datos[i] <- exp(-delta * simulaciones[i])
}
datos

# Calcular Ax, temporal, dotal puro , dotal 
# Seguro de vida entera continuo
datos<- array()
for (i in 1:k) {
  datos[i] <- exp(-delta * simulaciones[i])
}
datos
A_x <- mean(datos)
A_x

# seguro de vida dotal/ temporal
Y <- array()
for (i in 1:k){
  if (datos[i] < .7 ){
    Y[i] <- datos[i]
  }
}
Y
(A_xDotal <- sum(Y,na.rm = TRUE)/k)

# Seguro de vida dotal puro
datos<- array()
for (i in 1:k) {
  if (simulaciones[i]< n) {
    datos[i] <- exp(-delta * simulaciones[i])
  } else {
    datos[i] <- exp(-delta * n)  
  }
}
(A_xDotalpuro <- mean(datos))

# SEguro de vida diferido 

A_xdiferido <- A_xDotal + A_xDotalpuro * A_x

kable(cbind(A_x ,A_xDotal, A_xDotalpuro, A_xdiferido))
