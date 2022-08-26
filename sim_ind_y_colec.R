# Algoritmo para simular el modelo de riesgo individual y colectivo 
library(knitr)
library(dplyr)

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

# Simulacion de v.a. exponencial 
miexp <- function(mu, n = 1){
  Y <- array()
  
  for (k in 1:n){
    u <- runif(1)
    x <- -log(1 - u)/mu
    Y[k] <- x
  }
  Y
}
miexp(4)
# Simulacion del modelo de riesgo individual
riesgo_ind <- function(n,     #<- numero de pólizas 
                       q_j,   #<- prob de tener una reclamacion
                       mu_j,  #<- parametro de monto de reclamacion
                       r=1){    #<- numero de simulaciones 
  
  Reclamaciones <- 0
  Poliza <- 1:n
  Simulacion <- 1:r
  
  S_i <- array()
  
  for(h in Simulacion){
    Reclamaciones <- 0
    for (s in Poliza) {
      u <- runif(1)
      ne <- 0
      if (u < q_j ) {
        ne <- ne + 1 
      }
      Reclamaciones <- Reclamaciones + ne * miexp(mu_j)
    }
    S_i[h] <- Reclamaciones
  }
  data.frame(Simulacion, S_i)
} 

# Se asignan valores a los paremetros 
n <- 20 
q_j <- .1
mu_j <- 1/3000
r <- 100

# Se corre la función y la asignamos a la variable individual 
individual <- riesgo_ind(n , q_j, mu_j,r)

# Se muestran las simulaciones 
kable(individual)

# Se muestra la aproximacion del valor esperado 
kable(individual %>%
        summarize( VE_aproximado = mean(S_i)))

# Se muestra el valor esperado teórico 
kable(individual %>%
        summarize( VE_teorico = n * q_j * 1/mu_j ))


# Simulacion del modelo colectivo 
riesgo_col <- function(lambda, mu, k){
  n <- mipois(lambda)
  Poliza <- 1:n
  Reclamaciones <- 0
  Simulacion <- 1:k
  
  S_i <- array()
  
  for(h in Simulacion){
    Reclamaciones <- 0
    
    for (s in Poliza) {
      Reclamaciones <- Reclamaciones + miexp(mu)
    }
    
    S_i[h] <- Reclamaciones
  }
  data.frame(Simulacion, S_i)
}
 
 # Se corre la función y la asignamos a la variable individual 
lambda <- 2
mu <- 1/300
k <- 200
colectivo <- riesgo_col(lambda,mu,k)
 
 # Se muestran las simulaciones 
 kable(colectivo)
 
 # Se muestra la aproximacion del valor esperado 
 kable(colectivo %>%
         summarize( VE_aproximado = mean(S_i)))
 
 # Se muestra el valor esperado teórico 
 kable(colectivo %>%
         summarize( VE_teorico = lambda * 1/mu )
       )
 
 