# Actividad 2 
# Jorge U. Barragan Pozos 
#########################################
library(knitr)
library(dplyr)

#Variable Aleatoria Exponencial
miexp <- function(lamda, n = 1){
  Y <- array()
  
  for (k in 1:n){
    u <- runif(1)
    x <- -log(1 - u)/lamda
    Y[k] <- x
  }
  
  Y
}
#Ejemplo
miexp(5,3)
#########################################
# Varible Poisson
mipois <- function(lambda = 1, n = 1){
  Y <- array()
  
  for (q in 1:n){
    U <- runif(1)
    i <- 0
    p <- exp(-lambda)
    P <- p
    while(U >= P){
      p <- lambda * p / (i + 1)
      P <- P + p
      i <- i + 1
    }
    Y[q]<- i
  }
  Y
}
# Ejemplo 
mipois(4,300)

#########################################
# Variable a. binomial 
mibinom <- function(n,p, r=1){
  Y <- array()
  
  for (s in 1:r) {
    ne <- 0
    i <- 0
    while (i < n) {
      u <- runif(1)
      if (u < p) {
        ne <- ne + 1 
      }
      i <- i + 1 
    }
    Y[s] <- ne
  }
  Y
}
#ejemplo
datos <- mibinom(3,.5,500)
hist(datos)

# Algoritmo para simular el modelo de riesgo individual

riesgo_ind <- function(n,     #<- numero de pólizas 
                       q_j,   #<- prob de tener una reclamacion
                       mu_j){ #<- parametro de monto de reclamacion
  Reclamacion <- array()

  Poliza <- 1:n
  for (s in 1:n) {
    u <- runif(1)
    ne <- 0
    if (u < q_j ) {
      ne <- ne + 1 
    }
    Reclamacion[s] <- ne * miexp(mu_j)
  }
  kable(tabla <- data.frame(Poliza,Reclamacion))
  kable(tabla %>%
          mutate(Valor_esperado = mean(Reclamacion)))
} 
riesgo_ind(100 , .5, .00083)

