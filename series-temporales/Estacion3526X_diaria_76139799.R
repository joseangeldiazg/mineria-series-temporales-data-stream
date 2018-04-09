# José Ángel Díaz García 76139799R
# joseadiazg02@correo.ugr.es
# Ejercicio de trabajo autonomo. Series Temporales. Curso 2017-2018

#***************************************************************
# OBJETIVOS
#***************************************************************

# El objetivo de la práctica es predecir la temperatura máxima que tendremos la primera semana de marzo de 2018 a nivel de dias. 
# Es decir, necesitamos dar 7 predicciones.

#***************************************************************
# LECTURA y PRE PROCESADO DE DATOS
#***************************************************************

#Leemos los datos

datos<-read.csv2("data/3526x.csv", stringsAsFactors = F)
datos$Tmax<-as.numeric(datos$Tmax)

# Antes de eliminar las columnas que no necesitamos para nuestra predicción de series temporales, solventaremos el posible problema
# de los valores perdidos, ya que podemos usar las demas variables para estimar con regresión estos posibles valores.  

sum(is.na(datos$Fecha))
sum(is.na(datos$Tmax))

# Tenemos 83 valores perdidos por lo que tendremos que solventar esto antes de seguir con el procesado. Para ello, nos quedaremos
# con los valores numéricos y con ellos aplicaremos regresion con MICE para solventar el problema.

datosRegresion<-datos[-c(1,2,4,6,9,11)]

# Pasamos todas las columnas a numerico

datosRegresion <- apply(datosRegresion,2,as.numeric)

library(VIM)
aggr_plot <- aggr(datosRegresion, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histograma de valores perdidos","Patron"))

# Vemos que cuando hay valores perdidos en Tmax tendremos bastantes valores perdidos en las demás columnas
# por lo que la regresion no irá del todo bien aun así usaremos el paquete MICE para solventarlo

library(mice)
tempData <- mice(datosRegresion,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

# Por último completamos los datos y los añadimos al modelo

completedData <- complete(tempData,1)
sum(is.na(completedData$Tmax))


#En este punto debemos obtener los datos necesarios para trabajar con nuestra serie, es decir, fecha y Tmax.

datos<-as.data.frame(cbind(Fecha=datos$Fecha, Tmax=completedData$Tmax), stringsAsFactors = F)
datos$Tmax<-as.numeric(datos$Tmax)

#Ya tenemos nuestros datos correctamente, ahora los escribimos a disco para poder recuperarlos.

write(datos$Tmax, file = "data/Estacion3526X_diaria.txt", ncolumns = 365, append = FALSE, sep = " ")

#En este caso no hay que agregar asi que creamos la serie normalmente

seriediaria<-datos$Tmax
seriediaria.ts <- ts(datos$Tmax, f=180)
plot(decompose(seriediaria.ts))

#Parece que las varianzas cambian por lo que deberemos normalizar los datos con un logaritmico

seriediaria.ts <- log(seriediaria.ts)
seriediaria.log <- log(seriediaria)
plot(decompose(seriediaria.ts))

#Tras esto dividiremos en training y test que dejaremos 7 días para predecir y ver como ajusta el modelo

library(tseries)

NPred <- 7
NTest <- 7

serieTr <- seriediaria.log[1:(length(seriediaria.log)-NTest)]
tiempoTr <- 1:length(serieTr)
serieTs <- seriediaria.log[(length(seriediaria.log)-NTest+1):length(seriediaria)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Representamos gráficamente el espacio de training y test
plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs, col="red")

#***************************************************************
# MODELADO DE LA TENDENCIA
#***************************************************************

# Al igual que pasaba con la serie mensual en este caso nos encontramos con un modelado de la tendencia bastante complejo.
# hemos obtenido distintas gráficas en función de la frecuencia variando en periodos de 60 días, es decir, 2 mesesy la tendencia tiene
# un comportamiento ciclico aunque nuevamente parece que esta se inclina hacia la derecha, es decir, desciende con el paso del tiempo. 

# Este hecho de tendencia descendente choca con lo que podriamos pensar de un aumento gradual de las temperaturas debidos a 
# temas de calentamiento climático. Para constatar estos hechos, hemos acudido a ciertas webs de meteorología y hemos podido 
# comprobar que en los años  atras ha habido grandes picos de temeperatura máxima y mínimas alcanzados en Extremadura, algo que
# que ha hecho que que en estos dos ultimos años más estables (puede verse en los picos de la gráfica) pueda aparecer una tendencia
# general descendente pero que muy seguro pueda desaparecer en los próximos meses ya que es muy poco acentuada. 

# Dado esto, utilizaremos una regresion lineal para modelar la tendencia y obtendremos mediante test estadisticos
# una estimacion de su funcionamiento.

parametros.H1 <- lm(serieTr ~ tiempoTr)

# Calculamos la estimación de la tendencia

TendEstimadaTr.H1  <- parametros.H1$coefficients[1]+tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1  <- parametros.H1$coefficients[1]+tiempoTs*parametros.H1$coefficients[2]

# Mostramos el ajuste sobre la serie temporal

plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTr, TendEstimadaTr.H1, col="blue")
lines(tiempoTs, serieTs, col="red")
lines(tiempoTs, TendEstimadaTs.H1, col="green")

# Tal y como habiamos propuesto, la recta de regresiÓn nos dibuja una tendencia descendente. 
# Esta es casi plana, por lo que vamos a comprobar estadísticamente si el ajuste es significativo. 

# Primero comprobamos que los datos son normales, para ello usamos el test de jarque.bera

JB <- jarque.bera.test(parametros.H1$residuals)
JB <- jarque.bera.test((TendEstimadaTs.H1-serieTs))

# Los p-values nos indican que la ditribución es normal y por tanto podemos aplicar un t student de manera apropiada. 

TT <- t.test(c(parametros.H1$residuals, TendEstimadaTs.H1-serieTs))

# El tstudent nos indica que no hay diferencias significativas entre residuos del ajuste y los errores del modelo en tst
# Al ser esta hipótesis factible, asumimos la hipótesis del ajuste lineal y eliminamos la tendencia.

serieTr.SinTend.H1 <- serieTr - TendEstimadaTr.H1
serieTs.SinTend.H1 <- serieTs - TendEstimadaTs.H1


plot.ts(serieTr.SinTend.H1, xlim=c(1, tiempoTs[length((tiempoTs))]))
lines(tiempoTs, serieTs.SinTend.H1, col="red")


#***************************************************************
# MODELADO DE ESTACIONALIDAD
#***************************************************************

# La estacionalidad en este caso, será anual, por lo que deberemos modelarla con 365 muestras y lo dividiremnos en los 5 años de los que
# se tienen muestras. 

k <- 365 
estacionalidad <- decompose(seriediaria.ts)$seasonal[1:k]
aux <- rep(estacionalidad, 5)
serieTr.sinTendEst <- serieTr.SinTend.H1 - aux[1:length(serieTr)]
serieTs.sinTendEst <- serieTs.SinTend.H1 - aux[(length(serieTr)+1):(length(serieTr)+length(serieTs))]

plot.ts(serieTr.SinTendEst.H1, xlim=c(1, tiempoTs[length(tiempoTs)]))


#***************************************************************
# COMPROBACIÓN DE SERIE ESTACIONARIA
#***************************************************************

# Llegamos al punto más relevante del análisis y es que en este punto veremos si es posible modelar la serie en función de su 
# condición estacionaria. Para ello podemos apoyarnos en el gráfico acf o en el AD test.

adtest.h1 <- adf.test(serieTr.SinTendEst.H1)
acf(serieTr.SinTendEst.H1)


# En este caso parece que hay discrepancias. Por un lado el test nos dice que es estacionaria, pero por otro lado, el gráfico nos dice que
# al no descender rápidamente a 0 no lo es. Para curarnos en salud, vamos a diferenciar la serie. 


serieTr.SinTendEstDiff.H1 <- diff(serieTr.SinTendEst.H1)
serieTs.SinTendEstDiff.H1 <- diff(serieTs.SinTendEst.H1)

adtest.h1 <- adf.test(serieTr.SinTendEstDiff.H1)
acf(serieTr.SinTendEstDiff.H1)

# En este caso, tanto el test estadístico como el gráfico (tiende rápido a 0) nos ofrecen resultados aceptables, por lo que se cumpliría
# la condición de ser una serie estacionaria y por tanto podemos modelarla. 

#***************************************************************
# TEST ESTADÍSTICOS Y COMPARATIVAS DE MODELOS
#***************************************************************

# Vamos a visualizar sus gráficos acf y pacf para estimar que modelos pueden ser posibles.

acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)

# En base a los gráficos, podemos estar ante un modelo MA(5) o MA(6). 


modelodiario.H1 <- arima(serieTr.SinTendEst.H1, order=c(0,1,5))
modelodiario.H2 <- arima(serieTr.SinTendEst.H1, order=c(0,1,6))

valoresAjustados.H1 <- serieTr.SinTendEst.H1+modelodiario.H1$residuals
valoresAjustados.H2 <- serieTr.SinTendEst.H1+modelodiario.H2$residuals


#Predecimos para ambos modelos
Predicciones.H1 <- predict(modelodiario.H1, n.ahead = 6);
Predicciones.H2 <- predict(modelodiario.H2, n.ahead = 6);

valoresPredichos.H1 <-Predicciones.H1$pred
valoresPredichos.H2 <-Predicciones.H2$pred

#Calculamos el error cuadrático para ambos modelos
errorTr.H1 <- sum((modelodiario.H1$residuals)^2)
errorTs.H1 <- sum((valoresPredichos.H1- serieTs.SinTendEstDiff.H1)^2)

errorTr.H2 <- sum((modelomensual.H2$residuals)^2)
errorTs.H2 <- sum((valoresPredichos.H2- serieTs.SinTendEstDiff.H1[1:2])^2)


# Una vez tenemos todos los parámetros, aplicamos los test estadísticos, para ver 
# si los modelos son aceptables.

bpxtestM1 <- Box.test(modelodiario.H1$residuals)

#Test de normalidad
jb.M1 <- jarque.bera.test(modelodiario.H1$residuals)
sw.M1 <- shapiro.test(modelodiario.H1$residuals)

bpxtestM2 <- Box.test(modelodiario.H2$residuals)

#Test de normalidad
jb.M2 <- jarque.bera.test(modelodiario.H2$residuals)
sw.M2 <- shapiro.test(modelodiario.H2$residuals)

# En este caso, el test de aleatoriedad nos ofrece resultados peores que en el caso anterior, por lo que no podríamos descartar con
# toda la probabilidad el caso de tener resultados debidos a la aleatoriedad. Aquí hay cierta discordancia ya que tenemos en el caso de 
# los tes de normalidad el ejemplo contrario pasando los test con valores de W en el Saphiro muy altos lo que indica normalidad. 

# Pese a que tenemos dudas, compararemos como hicimos en el caso de la predicción mensual con el test AIC de comparación de modelos.

AIC(modelodiario.H1, modelodiario.H2)

# Acorde a los resultados, nos quedamos con el modelo 1, es decir ARIMA(0,1,6) el cual produce menos errores que el ARIMA(0,1,5)

#***************************************************************
# PREDICCIÓN
#***************************************************************

# Para predecir usando nuestros modelos, nos decantaremos por el uso de la siguiente función implementada en el ejericico de predicción mensual.


#Obtenemos las predicciones para los meses:

predicionArimaMA5<- predictTS(seriemensual.log, MA=5, D=1, k=7)
predicionArimaMA6<- predictTS(seriemensual.log, MA=6, D=1, k=7)


#Por ultimo vamos a comprobar el resultado de las predicciones. 

temperaturas2018<-c(13.6,12.2,12.3,14.5,13.1,13.7,12.9)

ErrorMedio <- sum(abs(temperaturas2018-predicionArimaMA6$`Valores-Predichos`))
ErrorMedio

# El error que sale es bastante elevado en el caso de nuestra predicción, podemos comprobar manualmente que hemos errado en algunos
# casos en casi mas de 9 grados. Pero esto nos lleva a pensar que la seria ha entrado con modelos antiguos y que este mes de marzo, ha 
# sido inusualmente frio y lluvioso en extremadura por lo que comprobaremos los resultados con series de temeperatura de la misma semana
# de marzo en años anteriores.

temperaturas2015 <- c(21.6,19.6,19.2,19.9,17.1,20.6,24.6)
temperaturas2017 <- c(17.1,17.6,11.5,12.6,13.7,20.0,22.3)

ErrorMedio2015 <- sum(abs(temperaturas2015-predicionArimaMA6$`Valores-Predichos`))
ErrorMedio2017 <- sum(abs(temperaturas2017-predicionArimaMA6$`Valores-Predichos`))

ErrorMedio2015
ErrorMedio2017

#Podemos ver por tanto, como en base a años anteirores el error de la predicción desciende drasticamente asimilandose más a la realdiad
#climática de extremadura. 


#***************************************************************
# CONCLUSIONES
#***************************************************************

# Ademas de las conclusiones vistas en el modelo mensual y que pueden asimilarse a este modelo diario, es menester mencionar el caracter
# de las seríes y su aparente dificultad de modelar cambios repentinos por ejemplo en el caso actual que nos compete de la temperatura, el 
# modelo predice en función de datos anteriores y en años anteriores encaja basntate bien, pero en el caso actual de una bajada brusca
# de las temperaturas comienza a fallar. Modelar estos casos, implicaran un nivel muy avanzado en las series temporales.


