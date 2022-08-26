# Eventos de Riesgo
library(knitr)
library(dplyr)


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


# Definimos funcion
simulacion_unif <- function(n = 1, k){
  p <- array()

  for (i in 1:k) { # <- se generan numeros aleatorios
    p[i]<- runif(1)
  }
  
  X <- data.frame()# <- Se define una matriz de binom
  
  for (s in 1:k) {
    for (i in 1:k) {
      X[i,s] <- mibinom(n , p[i]) 
    }
  }
  
  X$mm <- rowMeans(X[, c(1:k)]) # Se obtiene media muestral por fila 
  X$pgorro <- (X[, c(k+1)] - 1)/(k*n + 2) # Se obtienen pgorros
  hist(X$pgorro , main = 'Histograma de P_gorro con p uniforme', freq = FALSE)
  X$pgorro
}

simulacion_beta <- function(n = 1, k, alpha = 1 , beta = 3){
  p <- array()
  
  for (i in 1:k) { # <- se generan numeros aleatorios
    p[i]<- rbeta(1, alpha, beta)
  }
  
  X <- data.frame()# <- Se define una matriz de binom
  
  for (s in 1:k) {
    for (i in 1:k) {
      X[i,s] <- mibinom(n , p[i]) 
    }
  }
  
  X$mm <- rowMeans(X[, c(1:k)]) # Se obtiene media muestral por fila 
  X$pgorro <- (X[, c(k+1)] - 1)/(k*n + 2) # Se obtienen pgorros
  hist(X$pgorro , main = 'Histograma de P_gorro con p beta', freq = FALSE)
  X$pgorro
}

# algoritmo binomial
simulacion_unif(100, 300)

# algoritmo beta
simulacion_beta(100, 300, 5,1)

