#ACTIVIDAD 2.5 
Base <- read.csv("~/Downloads/Base.csv")
attach(Base)
datos <- as.data.frame(Base)

#Asignar valores de monto
w1 <- 10 #monto en millones
w2 <- 8

#Asignar los acciones de las empresas
x <- as.data.frame(datos$NETFLIX)
y <- as.data.frame(datos$AMAZON)

#volatilidades diarias Netflix
# Para el primer valor
datos$RendN[1] = 0

for(i in 2:253){
  
  datos$RendN[i] = (datos$NETFLIX[i]-datos$NETFLIX[i-1])/datos$NETFLIX[i-1]
}

#volatilidades diarias Amazon
# Para el primer valor
datos$RendA[1] = 0
for(i in 2:253){
  
  datos$RendA[i] = (datos$AMAZON[i]-datos$AMAZON[i-1])/datos$AMAZON[i-1]
}

#caclulando las desviaciones 
sigma1 <- sd(datos$RendN)
sigma2 <- sd(datos$RendA)

#calculando el coeficiente de correlación
rho <- cor(datos$RendN,datos$RendA)
rho

#calculando la volatilidad del portafolio
Vp <- w1^2 * sigma1^2 + w2^2 * sigma2^2+2*w1*rho*w2*sigma1*sigma2
Vp

sigmap <- sqrt(Vp)
sigmap

#calculando el VAR a un día 
VaR <- qnorm(.01,0,sigmap)
VaR

#calculando el VaR a N días 
N <- 5 # <- inserta el numero de días 
VaR_N <- VaR * sqrt(N)
VaR_N
