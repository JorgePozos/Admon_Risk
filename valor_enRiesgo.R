# practica de admon de riesgo para nuestra acción 
# Jorge U. Barragan Pozos 
library( tidyverse )

ADBE <- read.csv("~/Downloads/ADBE.csv")
attach( ADBE )
# Limpiamos datos 
ADBE <-  ADBE %>% 
  mutate( Activo = Adj.Close )
ADBE <- ADBE %>% 
  select(Date, Activo)
# graficamos el precio 
ADBE %>%
  ggplot( aes(Activo) ) + geom_density()

# Hacemos una transformacion 
ADBE$ActivoF[1] = ADBE$Activo[505]* ADBE$Activo[2]/ADBE$Activo[1]

for(i in 2:505){
  ADBE$ActivoF[i] = ADBE$Activo[505]* ADBE$Activo[i]/ADBE$Activo[i-1]
}

# graficamos la transformacion 
ADBE %>%
  ggplot( aes(ActivoF) ) + geom_density()

# Encontramos VaR
VaR_diario <-quantile(ADBE$ActivoF, .01)
VaR_diario

# practica con el precio del dolar 
mxn <- read.csv("~/Downloads/MXN=X.csv")
attach( mxn )

# Limpiamos datos 
mxn <-  mxn %>% 
  mutate( Activo = Adj.Close )
mxn <- mxn %>% 
  select(Date, Activo)
# graficamos el precio 
mxn %>%
  ggplot( aes(Activo) ) + geom_density()

# Hacemos una transformacion 
mxn$ActivoF[1] = mxn$Activo[505]* mxn$Activo[2]/mxn$Activo[1]

for(i in 2:505){
  mxn$ActivoF[i] = mxn$Activo[505]* mxn$Activo[i]/mxn$Activo[i-1]
}

# graficamos la transformacion 
mxn %>%
  ggplot( aes(ActivoF) ) + geom_density()

# Encontramos VaR
VaR_diario <-quantile(mxn$ActivoF, .01)
VaR_diario
