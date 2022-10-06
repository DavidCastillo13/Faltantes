
# Imputación por pmm
# Regresión lineal + selección del donante más cercano

datos = airquality[,1:4]
summary(datos)

with(datos, plot(Solar.R, Ozone, pch = 20))

miss = data.frame(is.na(datos)) ; miss

imp = datos

# Iteración 1. 
# Imputación simple con media incondicional
imp$Ozone[miss$Ozone] = mean(datos$Ozone, na.rm = T)
imp$Solar.R[miss$Solar.R] = mean(datos$Solar.R, na.rm = T)

summary(imp)

with(imp, plot(Solar.R, Ozone, pch = 20, col = miss$Ozone*1+1))

# Iteración 2, 3, ...
# Imputación por regresión + pmm

# Modelos de regresión
mod.oz = lm(Ozone ~ . , data=imp) ; mod.oz
mod.sol = lm(Solar.R ~ . , data=imp) ; mod.sol

# Valores predichos
oz = predict(mod.oz) ; oz
sol = predict(mod.sol) ; sol

# Valores predichos para faltantes
miss.oz = oz[miss$Ozone] ; miss.oz
miss.sol = sol[miss$Solar.R] ; miss.sol

# Valores predichos para observados
obs.oz = oz[!miss$Ozone] ; obs.oz
obs.sol = sol[!miss$Solar.R] ; obs.sol

# Valores reales observados (donantes)
real.oz = datos$Ozone[!miss$Ozone] ; real.oz
real.sol = datos$Solar.R[!miss$Solar.R] ; real.sol

miss.oz = as.numeric(miss.oz)
miss.sol = as.numeric(miss.sol)
obs.oz = as.numeric(obs.oz)
obs.sol = as.numeric(obs.sol)
real.oz = as.numeric(real.oz)
real.sol = as.numeric(real.sol)

# Donantes

# Para Ozone
donante.oz = numeric(0)
for(i in 1:length(miss.oz)){
  donante.oz[i] = which.min(abs(miss.oz[i] - obs.oz))
}

# Para Solar.R
donante.sol = numeric(0)
for(i in 1:length(miss.sol)){
  donante.sol[i] = which.min(abs(miss.sol[i] - obs.sol))
}

# Imputación
imp$Ozone[miss$Ozone] = real.oz[donante.oz]
imp$Solar.R[miss$Solar.R] = real.sol[donante.sol]

imp
summary(imp)
with(imp, plot(Solar.R, Ozone, pch = 20, col = miss$Ozone*1+1))

# ... Repetir hasta convergencia

######  Tarea de imputar como lo hace mice con los 3 menores uwu

minimos=function(x=vector, y=vector, posicion){
  a=abs(x[posicion] - y)
  donante1=numeric(0)
  for (j in 1:3) {
    donante1[j]=which.min(a)
    a[donante1[j]]=max(a)
  }
  donante1
}

minimos(miss.oz,obs.oz,2)

sample(minimos(miss.oz,obs.oz,1),1)


# Para Ozone
donante.oz = numeric(0)
for(i in 1:length(miss.oz)){
  donante.oz[i] = sample(minimos(miss.oz,obs.oz,i),1)
}

# Para Solar.R
donante.sol = numeric(0)
for(i in 1:length(miss.sol)){
  donante.sol[i] = sample(minimos(miss.sol,obs.sol,i),1)
}

donante.oz

# Imputación
imp$Ozone[miss$Ozone] = real.oz[donante.oz]
imp$Solar.R[miss$Solar.R] = real.sol[donante.sol]

imp
summary(imp)

### Grafico de los valores reales en negro con los que 
### estan en rojo siendo los imputados

with(imp, plot(Solar.R, Ozone, pch = 20, col = miss$Ozone*1+1)) 

