 
require(readxl)
require(TSA)
require(forecast)
require(imputeTS)

eldorado = read_xlsx("ElDorado.xlsx")
head(eldorado)

int = ts(eldorado$Internacional, start = c(2012, 1), freq =12)
plot(int)

# Amputando la serie de tiempo
set.seed(123)
nas = sample(1:length(int), 15) ; nas
x = int
x[nas] = NA

ggplot_na_distribution(x) # Distribucion para graficar series con faltantes uwu
#### Ayuda a ver si hay patrones de faltantes, 
#### si ejemplo hay cosas gravez como un año faltante seguido, no habra algo que haga las cosas bien

# Imp. con un valor aleatorio (Toma un valor observado de la serie y remplaza, no bueno uwu)
imp.ran = na_random(x) ; imp.ran
ggplot_na_imputations(x, imp.ran) # permite comparar hasta 3 series, con faltantes, imputada, y ejemplo read
ggplot_na_imputations(x, imp.ran, int)

# Imp. con el promedio
imp.prom = na_mean(x) ; imp.prom # Si la serie no tiene tendecia seria mas adecuado, pero mejor que aleatorio
ggplot_na_imputations(x, imp.prom)
ggplot_na_imputations(x, imp.prom, int)

# Imp. con la mediana
imp.med = na_mean(x, option="median") ; imp.med # Igual mala para esto
ggplot_na_imputations(x, imp.med)
ggplot_na_imputations(x, imp.med, int)

# Imp. con la última observación (observada)
imp.locf = na_locf(x) ; imp.locf # No tiene sentido cuando hay secuencias mas largas de faltantes, ejemplo 3 meses, 4, 
ggplot_na_imputations(x, imp.locf) # Aqui es decente la imputacion
ggplot_na_imputations(x, imp.locf, int)

# Imp. con medias móviles
imp.ma = na_ma(x, k = 2) ; imp.ma # Metodo de suavizamiento (promedio valores antes y despues promedio y listo)
ggplot_na_imputations(x, imp.ma)  # Aqui toma dos por abajo y dos por encima (no suavizar mucho o caga modelo)
ggplot_na_imputations(x, imp.ma, int)

# Imp. con interpolación
imp.int = na_interpolation(x, option = "spline") ; imp.int # Aqui sirven mas los no parametricos que 
ggplot_na_imputations(x, imp.int)                          # como spline u otro, por que son mas
ggplot_na_imputations(x, imp.int, int)                     # precisos que los lineales

# Imp. con Kalman
imp.kal = na_kalman(x, model = "StructTS") ; imp.kal       # Filtros de kalman (construye un modelo)
ggplot_na_imputations(x, imp.kal)                          # arima estacional y pronostica
ggplot_na_imputations(x, imp.kal, int,                     # Generalmente el mas eficiente
    color_points = 1, color_imputations = 2, color_truth = 4)

mod = auto.arima(imp.kal, lambda = "auto")
summary(mod)

pro = forecast(mod, h = 12) ; pro
plot(pro)

##### Ahora revisando domestico

int = ts(eldorado$Doméstico, start = c(2012, 1), freq =12)
plot(int)

# Amputando la serie de tiempo
set.seed(123)
nas = sample(1:length(int), 15) ; nas
x = int
x[nas] = NA

ggplot_na_distribution(x) # Distribucion para graficar series con faltantes uwu
#### Ayuda a ver si hay patrones de faltantes, 
#### si ejemplo hay cosas gravez como un año faltante seguido, no habra algo que haga las cosas bien

# Imp. con un valor aleatorio (Toma un valor observado de la serie y remplaza, no bueno uwu)
imp.ran = na_random(x) ; imp.ran
ggplot_na_imputations(x, imp.ran) # permite comparar hasta 3 series, con faltantes, imputada, y ejemplo read
ggplot_na_imputations(x, imp.ran, int)

# Imp. con el promedio
imp.prom = na_mean(x) ; imp.prom # Si la serie no tiene tendecia seria mas adecuado, pero mejor que aleatorio
ggplot_na_imputations(x, imp.prom)
ggplot_na_imputations(x, imp.prom, int)

# Imp. con la mediana
imp.med = na_mean(x, option="median") ; imp.med # Igual mala para esto
ggplot_na_imputations(x, imp.med)
ggplot_na_imputations(x, imp.med, int)

# Imp. con la última observación (observada)
imp.locf = na_locf(x) ; imp.locf # No tiene sentido cuando hay secuencias mas largas de faltantes, ejemplo 3 meses, 4, 
ggplot_na_imputations(x, imp.locf) # Aqui es decente la imputacion
ggplot_na_imputations(x, imp.locf, int)

# Imp. con medias móviles
imp.ma = na_ma(x, k = 2) ; imp.ma # Metodo de suavizamiento (promedio valores antes y despues promedio y listo)
ggplot_na_imputations(x, imp.ma)  # Aqui toma dos por abajo y dos por encima (no suavizar mucho o caga modelo)
ggplot_na_imputations(x, imp.ma, int)

# Imp. con interpolación
imp.int = na_interpolation(x, option = "spline") ; imp.int # Aqui sirven mas los no parametricos que 
ggplot_na_imputations(x, imp.int)                          # como spline u otro, por que son mas
ggplot_na_imputations(x, imp.int, int)                     # precisos que los lineales

# Imp. con Kalman
imp.kal = na_kalman(x, model = "StructTS") ; imp.kal       # Filtros de kalman (construye un modelo)
ggplot_na_imputations(x, imp.kal)                          # arima estacional y pronostica
ggplot_na_imputations(x, imp.kal, int,                     # Generalmente el mas eficiente
                      color_points = 1, color_imputations = 2, color_truth = 4)

mod = auto.arima(imp.kal, lambda = "auto")
summary(mod)

pro = forecast(mod, h = 12) ; pro
plot(pro)

