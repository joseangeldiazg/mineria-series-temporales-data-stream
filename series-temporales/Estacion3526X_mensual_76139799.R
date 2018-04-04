# José Ángel Díaz García 76139799R
# joseadiazg02@correo.ugr.es
# Ejercicio de trabajo autonomo. Series Temporales. Curso 2017-2018

#***************************************************************
# OBJETIVOS
#***************************************************************

# El objetivo de la práctica es predecir la temperatura máxima que tendremos en los meses de abril y marzo de 2018. 

#***************************************************************
# LECTURA y PRE PROCESADO DE DATOS
#***************************************************************

#Leemos los datos
library(tseries)
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
aggr_plot <- aggr(datosRegresion, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# Vemos que cuando hay valores perdidos en Tmax tendremos bastantes valores perdidos en las demás columnas
# por lo que la regresion no irá del todo bien aun así usaremos el paquete MICE para solventarlo

library(mice)
tempData <- mice(datosRegresion,m=5,maxit=50,meth='norm.predict',seed=500)
summary(tempData)

# Por último completamos los datos y los añadimos al modelo

completedData <- complete(tempData,1)
sum(is.na(completedData$Tmax))

#En este punto debemos obtener los datos necesarios para trabajar con nuestra serie, es decir, fecha y Tmax.

datos<-as.data.frame(cbind(Fecha=datos$Fecha, Tmax=completedData$Tmax), stringsAsFactors = F)
datos$Tmax<-as.numeric(datos$Tmax)
datos$Fecha <-as.Date(datos$Fecha, "%Y-%m-%d")

# Ya tenemos los datos necesarios, en este punto deberemos agregarlos de manera mensual.

# Para ello nos quedamos con una nueva columna que sea el año y el mes

datos$AñoMes <-format(as.Date(datos$Fecha), "%Y-%m")

# Sobre esta columna nos quedaremos con la máxima temperatura de ese mes
# y construiremos los datos de la serie final

datosSerieMensual<-aggregate(datos$Tmax, by = list(datos$AñoMes), max)

# Por último escribimos los datos en disco para poder volver a recuperarlos.

write(datosSerieMensual$x, file = "data/Estacion3526X_mensual.txt", ncolumns = 12, append = FALSE, sep = " ")

# En el paso final de esta parte podemos crear la serie temporal para poder a volver a realizarla

seriemensual<-datosSerieMensual$x
seriemensual.ts <- ts(datosSerieMensual$x, f=12)
plot(decompose(seriemensual.ts))

#Parece que las varianzas cambian por lo que deberemos normalizar los datos con un logaritmico

seriemensual.ts <- log(seriemensual.ts)
seriemensual.log <- log(seriemensual)
plot(decompose(seriemensual.ts))

# Acorde a los gráficos podemos deducir que no hay tendencia al menos a simple vista, aunque
# si que se aprecia que descienden los valores. Por otro lado si que se aprecia una clara
# estacionalidad. En el primer perídoo parece que ocurre de manera distinta pero es porque
# los datos que tenemos comienzan en mayo, es decir, a mediados de la serie ideal. 

#Vamos a comenzar a estudiar y modelar la serie, primero partimos en training y test.
#Como queremos modelar 2 meses, nos quedaremos con 2 meses de test

NPred <- 2
NTest <- 2

serieTr <- seriemensual.log[1:(length(seriemensual.log)-NTest)]
tiempoTr <- 1:length(serieTr)
serieTs <- seriemensual.log[(length(seriemensual.log)-NTest+1):length(seriemensual)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Representamos gráficamente el espacio de training y test
plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs, col="red")


#***************************************************************
# MODELADO DE LA TENDENCIA
#***************************************************************

# Hemos concluido que la tendencia en caso de haberla es muy reducida. Igualmente, 
# la modelaremos para aceptar o refutar esta  hipótesis de manera estadística.
# Partimos con un modelo lineal sencillo de partida.

parametros.H1 <- lm(serieTr ~ tiempoTr)

# Calculamos la estimación de la tendencia

TendEstimadaTr.H1  <- parametros.H1$coefficients[1]+tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1  <- parametros.H1$coefficients[1]+tiempoTs*parametros.H1$coefficients[2]

# Mostramos el ajuste sobre la serie temporal

plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTr, TendEstimadaTr.H1, col="blue")
lines(tiempoTs, serieTs, col="red")
lines(tiempoTs, TendEstimadaTs.H1, col="green")

# Parece que hay una tendencia descentende aunque es dificil de ver a simple vista, 
# la regresion la muestra claramente.

# Vamos a comprobar si este ajuste de la regresión lineal es estadisticamente significativo
# y por tanto podemos usarlo para modelar y eliminar la tendencia.

# Primero comprobamos que los datos son normales, para ello usamos el test de jarque.bera

JB <- jarque.bera.test(parametros.H1$residuals)
JB <- jarque.bera.test((TendEstimadaTs.H1-serieTs))

# Como los p-values son mayores de 0.05 asumimos normalidad y por lo tanto podemos aplicar el t student.

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

# En base al gráfico anterior podemos concluir que tendremos estacionalidad. 
# Vamos a modelar esta, para eliminarla y poder seguir en nuestro análisis.
# Dado que tenemos datos de temperatura, podemos preveer estacionalidad de 6 meses
# Períodos más frios y periodos mas calientesaún así nos basaremos en el gráfico 
# acf para corroborarlo.

acf(serieTr.SinTend.H1)

# Una vez determinado el periodo de estacionalidad para eliminarla lo que hacemos
# es obtener las medias para cada valor de estacionalidad tras tener estos valores
# los restamos a la serie original para calulcar el E(t)

k <- 6
estacionalidad.H1 <- decompose(seriemensual.ts)$seasonal[1:k]
aux <- rep(estacionalidad.H1, length(serieTr)/length(estacionalidad.H1))

serieTr.SinTendEst.H1 <- serieTr.SinTend.H1-aux
serieTs.SinTendEst.H1 <- serieTs.SinTend.H1-estacionalidad.H1
plot.ts(serieTr.SinTendEst.H1, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTendEst.H1[1:2],col="red")

#***************************************************************
# COMPROBACION DE SERIE ESTACIONARIA
#***************************************************************

# Una serie será estacionaria si su media y variabilidad se mantienen a lo largo del tiempo.
# Esto nos será  muy últil ya que nos permitirá hacer predicciones correctamente. 
# Obtendremos por tanto la estacionaridad de la serie.


adtest.h1 <- adf.test(serieTr.SinTendEst.H1)
acf(serieTr.SinTendEst.H1)

# Tanto gráficamente al tender acf a 0 muy rápido coo el test estadistico nos indican
# que tenemos una serie estacionaria por lo que no debemos ni diferenciarla.

#***************************************************************
# TEST ESTADÍSTICOS Y COMPARATIVAS DE MODELOS
#***************************************************************

# Vamos a visualizar sus gráficos acf y pacf para estimar que modelos pueden ser posibles.

acf(serieTr.SinTendEst.H1)
pacf(serieTr.SinTendEst.H1)

# En base a los gráficos, podemos estar ante un modelo MA(5) o AR(4). En base a estos modelos
# Vamos a realizar comparaciones estadísticas para ver cual nos ofrece mejores resultados.


modelomensual.H1 <- arima(serieTr.SinTendEst.H1, order=c(4,0,0))
modelomensual.H2 <- arima(serieTr.SinTendEst.H1, order=c(0,0,5))

valoresAjustados.H1 <- serieTr.SinTendEst.H1+modelomensual.H1$residuals
valoresAjustados.H2 <- serieTr.SinTendEst.H1+modelomensual.H2$residuals


#Predecimos para ambos modelos
Predicciones.H1 <- predict(modelomensual.H1, n.ahead = NPred);
Predicciones.H2 <- predict(modelomensual.H2, n.ahead = NPred);

valoresPredichos.H1 <-Predicciones.H1$pred
valoresPredichos.H2 <-Predicciones.H2$pred

#Calculamos el error cuadrático para ambos modelos
errorTr.H1 <- sum((modelomensual.H1$residuals)^2)
errorTs.H1 <- sum((valoresPredichos.H1- serieTs.SinTendEst.H1[1:2])^2)

errorTr.H2 <- sum((modelomensual.H2$residuals)^2)
errorTs.H2 <- sum((valoresPredichos.H2- serieTs.SinTendEst.H1[1:2])^2)


# Una vez tenemos todos los parámetros, aplicamos los test estadísticos, para ver 
# si los modelos son aceptables.

bpxtestM1 <- Box.test(modelomensual.H1$residuals)
#Test de normalidad
jb.M1 <- jarque.bera.test(modelomensual.H1$residuals)
sw.M1 <- shapiro.test(modelomensual.H1$residuals)

bpxtestM2 <- Box.test(modelomensual.H2$residuals)
#Test de normalidad
jb.M2 <- jarque.bera.test(modelomensual.H2$residuals)
sw.M2 <- shapiro.test(modelomensual.H2$residuals)

# Acorde a los test de aleatoriedad y normalidad los cuales se pasan correctamente.
# Podemos concluir que ambos modelos se comportan de manera aceptable. Si nos fijamos 
# en el test de Saphiro Wilk, valores altos de W nos llevan a aceptar la hipótesis nula
# por lo que podemos concluir con valores de 0.97916 y 0.97217 de w que el modelo se
# comporta bien.

# Por último, vamos a comparar que modelo se comporta mejor con un test comparativo entre modelos.

AIC(modelomensual.H1, modelomensual.H2)

# Acorde a los resultados, nos quedamos con el modelo 1, es decir ARIMA(4,0,0) el cual
# produce menos errores.

#***************************************************************
# PREDICCIÓN
#***************************************************************


# Para predecir usando nuestros modelos, nos decantaremos por el uso de la siguiente funcion.
# implementada con el trabajo guiado y que realiza toda la lógica de prediccion. 

# Cabe decir, que para predecir y obtener los modelos reales, una vez entrenado el modelo
# se deshacen todos los pasos dados en el análisis de la red a la inversa.

predictTS <- function(serieEntera, AR=0, MA=0, DIFF=0, kestacional)
{
  tiempo <- 1:length(serieEntera)
  parametros <- lm(serieEntera ~ tiempo)     
  TendEstimada <- parametros$coefficients[1]+tiempo*parametros$coefficients[2]
  serieSinTend <- serieEntera-TendEstimada
  aux <- ts(serieEntera, f=kestacional)
  aux <- decompose(aux)$seasonal
  estacionalidad <- as.numeric(aux[1:kestacional])
  aux<-rep(estacionalidad, length(serieSinTend)/length(estacionalidad))
  serieSinTendEst <- serieSinTend-aux
  modelo <- arima(serieSinTendEst, order=c(AR,DIFF,MA))
  valoresAjustatos <- serieSinTendEst+modelo$residuals
  Predicciones <- predict(modelo, n.ahead = kestacional)
  valoresPredichos <- Predicciones$pred
  #Deshacemos pasos
  valoresAjustados <- valoresAjustatos+aux
  valoresPredichos <- valoresPredichos+estacionalidad
  
  valoresAjustados <- valoresAjustados+TendEstimada
  tiempoPred <- (tiempo[length(tiempo)]+(1:kestacional))
  TendEstimadaPred <- parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
  valoresPredichos <- valoresPredichos+TendEstimadaPred
  
  valoresAjustados <- exp(valoresAjustados)
  valoresPredichos <- exp(valoresPredichos)
  
  return(list("Valores-Predichos"=valoresPredichos,
              "Valores-Ajustados"=valoresAjustados))
}

#Obtenemos las predicciones para los meses:

predicionArimaAR4<- predictTS(seriemensual.log, AR=4, k=2)
predicionArimaMA5<- predictTS(seriemensual.log, MA=5, k=2)

#Vamos a comprobar el error o acierto, como solo es un mes, podemos hacerlo manual:

# Accediendo a la web de AEMET podemos ver que la temperatura más alta obtenida en la 
# estación en marzo ha sido de 24.4 el dia 27-03-2018

#El modelo basado en AR 4 nos ofrece 27.77537º lo que es un error de 3 grados.
#El modelo basado en MA 5 nos ofrece algo mas de 30º por lo que el error es mas elevado.

#***************************************************************
# CONCLUSIONES
#***************************************************************

# Vemos como el modelo AR, es bastante aceptable y además cumple con lo que el test AIC
# predijo estadísticamente sobre el comportamiento del modelo.

# Si nos centramos en las connotaciones de las series y lo aprendido y asimilado en la 
# asignatura, no queda otra que constatar su potencia y la de los recursos para su 
# minado que R ofrece ya que éstos nos ayudan y permiten dar predicciones bastante fiables,
# con apenas unas horas de práctica y trabajo con ellas. 



