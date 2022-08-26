# Ejercicio:
# librereria para simular Weibull
library(actuar)
library(stats)
library(tidyverse)
library(ggplot2)
help(distributions)
mu<- 4 
sig <- 3 

sim_weibull<- data.frame(x = exp(rweibull(n=142, mu , sig)))

hist(sim_weibull$x, n=50)

ggplot(data= sim_weibull,
       mapping = aes(x = x))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# libreria para simular Frechét
library(extraDistr)
sim_frechet<- data.frame(x = exp(rfrechet(n=142, mu , sig)))

hist(sim_frechet$x, n=50)

ggplot(data= sim_frechet,
       mapping = aes(x = x))+
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
