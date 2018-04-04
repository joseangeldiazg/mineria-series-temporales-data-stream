# José Ángel Díaz García
# joseadiazg02@correo.ugr.es
# Ejercicio Guiado. Curso 2017-2018

library(tseries)

# Vamos a predecir datos anuales, uno por mes por lo que deberemos declarar las variables globales

NPred <- 12
NTest <- 12

#***************************************************************
# LECTURA y PRE PROCESADO DE DATOS
#***************************************************************


# Leemos la seríe y la descomponemos para analizarla graficamente
serie <- scan("data/pasajeros_1949_1959.dat")
serie.ts <- ts(serie, f=12)
plot(decompose(serie.ts))

# Pregunta 1. Análisis inicial de la serie

# Vemos que la varianza de los datos varia notablemente en ciertos puntos, siendo diferencias muy significativas
# a lo largo del tiempo, y siguiendo patrones pero algo erráticos. Esto puede verse en gráfico Random y también se 
# puede ver en observed donde vemos como los valores crecen con el tiempo. 

# Pregunta 2. Justificar si hay necesidad de preprocesar los datos iniciales, e indicar qué transformación se realiza y porqué.

# Debido a la variación de la varianza que hemos comentado anteriormente, deberemos "normalizar" los datos.
# La transformación seguida será una transformacion logaritmica.

serie.ts <- log(serie.ts)
serie.log <- log(serie)
plot(decompose(serie.ts))

# Pregunta 1.2. Análisis inicial de la serie:
# Claramente hay una tendencia en alza según pasan los años y que tenemos cierta estacionalidad anual.

# Hecho esto, dividiremos nuestros datos en train y test para poder crear nuestro modelo predictivo.


serieTr <- serie.log[1:(length(serie.log)-NTest)]
tiempoTr <- 1:length(serieTr)
serieTs <- serie.log[(length(serie.log)-NTest+1):length(serie)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs, col="red")

# 3. Justificar, en caso de haber tendencia o estacionalidad, cuál de las dos se debe eliminar antes.

# El primer paso a modelar será la tendencia debido a que si eliminamos esta de la serie temporal, tendremos todos los 
# datos de la estacionalidad en el mismo rango por decirlo de alguna manera, lo que nos ayudará en futuros pasos. 

#***************************************************************
# MODELADO DE TENDENCIA
#***************************************************************

# Ejercicio 4. En el caso de existir tendencia, justificar qué modelo de tendencia se utiliza para eliminarla (filtros, aproximación funcional, diferenciación).

# Dado que la tendencia parece ser lineal, nos decantaremos por esta función que a priori parecerá ajustar apropiadamente.
# Para comprobar que es así totalmente, nos basaremos en test estadisticos de diferencias entre residuos. 

# Nos decantamos por una funcion lineal como hipótesis de partida

parametros.H1 <- lm(serieTr ~ tiempoTr)

# Calculamos la estimación de la tendencia

TendEstimadaTr.H1  <- parametros.H1$coefficients[1]+tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1  <- parametros.H1$coefficients[1]+tiempoTs*parametros.H1$coefficients[2]

# Mostramos el ajuste sobre la serie temporal

plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTr, TendEstimadaTr.H1, col="blue")
lines(tiempoTs, serieTs, col="red")
lines(tiempoTs, TendEstimadaTs.H1, col="green")


# Ahora comprobamos que la hipótesis de tendencia lineal es válida para ello aplicamos test estadisticos

# Primero comprobamos que los datos son normales, para ello usamos el test de jarque.bera

JB <- jarque.bera.test(parametros.H1$residuals)
JB <- jarque.bera.test((TendEstimadaTs.H1-serieTs))

# Como los p-values son mayores de 0.05 asumimos normalidad y por lo tanto podemos aplicar el t student.

TT <- t.test(c(parametros.H1$residuals, TendEstimadaTs.H1-serieTs))

# El tstudent nos indica que no hay diferencias significativas entre residuos del ajuste y los errores del modelo en tst
# Al ser esta hipótesis factible, asumimos la hipótesis del ajuste lineal y eliminamos la tendencia usando esta premisa

serieTr.SinTend.H1 <- serieTr - TendEstimadaTr.H1
serieTs.SinTend.H1 <- serieTs - TendEstimadaTs.H1

plot.ts(serieTr.SinTend.H1, xlim=c(1, tiempoTs[length((tiempoTs))]))
lines(tiempoTs, serieTs.SinTend.H1, col="red")

#***************************************************************
# MODELADO DE ESTACIONALIDAD
#***************************************************************

# Ejercicio 5. En el caso de existir estacionalidad, justificar qué modelo se utiliza para eliminarla.

# Hay claramente estacionalidad, podemos usar acf para ver el periodo de estacionalidad que claramente será 12 segun predegimos. 
acf(serieTr.SinTend.H1)

# Una vez determinado el periodo de estacionalidad para eliminarla lo que hacmeos es obtener las medias para cada valor de estacionalidad
# Tras tener estos valores los restamos a la serie original para calulcar el E(t)

k <- 12 
estacionalidad.H1 <- decompose(serie.ts)$seasonal[1:k]
aux <- rep(estacionalidad.H1, length(serieTr)/length(estacionalidad.H1))

serieTr.SinTendEst.H1 <- serieTr.SinTend.H1-aux
serieTs.SinTendEst.H1 <- serieTs.SinTend.H1-estacionalidad.H1
plot.ts(serieTr.SinTendEst.H1, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs.SinTendEst.H1,col="red")

#***************************************************************
# COMPROBACION DE ESTACIONARIA
#***************************************************************

# Una serie será estacionaria si su media y variabilidad se mantienen a lo largo del tiempo. Esto nos será  muy últil 
# ya que nos permitirá hacer predicciones correctamente. Obtendremos por tanto la estacionalida de la serie.

# 6. Explicación del procedimiento seguido para comprobar y conseguir la estacionaridad, en base a los ADF, ACF, PACF.

# Para comprobar la estacionariedad vemos que adtest nos da p-value muy elevado con hipótesis alernativa
# de ser estacionaria por lo que tendremos que aceptar que no es estacionaria. Acf tiende a 0 muy lento por lo que tambien
# podemos apoyarnos en este gráfico para determinar que no lo es. 

adtest.h1 <- adf.test(serieTr.SinTendEst.H1)
acf(serieTr.SinTendEst.H1)

# Deberemos por tanto diferenciar la series

serieTr.SinTendEstDiff.H1 <- diff(serieTr.SinTendEst.H1)
serieTs.SinTendEstDiff.H1 <- diff(serieTs.SinTendEst.H1)

# Tras diferenciar las series podriamos ver como el test estadistico ahora si acepta la hipótesis de ser estacionaria.
# Igualmente, el gráfico acf tiende a 0 rápidamente por lo que podremos afirmar este hecho.

adtest.h1 <- adf.test(serieTr.SinTendEstDiff.H1)

# Vamos a visualizar sus gráficos acf y pacf

acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)

#***************************************************************
# CONSTRUCCIÓN DEL MODELO
#***************************************************************

# 7. Justificar la selección del modelo de predicción.

# Podriamos afirmar que estamos ante un AR(4), por el último corte de la correlacion en el gáfico de PACF o MA(1), 
# esto podemos verlo si nos fijamos en el gráfico acf, y vemos el corte de correlación dentro del período buscado. 
# Es decir, los modelos de prediccion que usaremos, podrían ser un ARIMA(4,1,0) o un ARIMA(0,1,1), teniendo en cuenta 
# que el parametro de diferenciacion usado en el diff aunque no lo hemos pasado como argumento coge el valor 1 por defecto.

#Ajustamos el modelo
modelo.H1 <- arima(serieTr.SinTendEst.H1, order=c(4,1,0))
valoresAjustados.H1 <- serieTr.SinTendEst.H1+modelo.H1$residuals


#Predecimos 
Predicciones.H1 <- predict(modelo.H1, n.ahead = NPred);
valoresPredichos.H1 <-Predicciones.H1$pred

#Calculamos el error cuadrático
errorTr.H1 <- sum((modelo.H1$residuals)^2)
errorTs.H1 <- sum((valoresPredichos.H1- serieTs.SinTendEst.H1)^2)

#Vemos los resultados
plot.ts(serieTr.SinTendEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.H1, col="blue")
lines(tiempoTs, serieTs.SinTendEst.H1, col="red")
lines(tiempoTs, valoresPredichos.H1, col="brown")


#***************************************************************
# VALIDACIÓN DEL MODELO
#***************************************************************

# 8. Explicar cómo se valida el modelo ajustado, describiendo qué es cada test, para qué se utiliza y qué resultados puede proporcionar.

# Para validar el modelo utilizamos test estadisticos nuevamente. Por un lado el test de Box Pierce que nos habla de la aleatoriedad
# de los residuos. Es decir, como h0 tenemos que los resultados son fruto de aleatoriedad y con Ha tenemos que son independientes.
# Para ver la normalidad de los residuos de error, podemos usar los test de Jarque Bera y Shapiro Wilk que toma como hipótesis nula
# que los datos provienen de poblaciones normalmente distribuidas. 

#Test de aleatoriedad

bpxtestM1 <- Box.test(modelo.H1$residuals)

#Test de normalidad
jb.h1 <- jarque.bera.test(modelo.H1$residuals)

sw.h1 <- shapiro.test(modelo.H1$residuals)

# Como W es cercano a 1 aprobamnos la hipótesis nula por lo que el modelo es aceptable. 

#Por ultimo vemos el ajuste del modelo gráficamente

hist(modelo.H1$residuals, col="blue", prob=T, ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.H1$residuals))

   
# Ejercicio 9. Describir, en el caso de existir varios modelos de predicción, qué criterio se ha escogido para seleccionar el mejor de ellos (AIC, MSE, etc.), justificando la elección del criterio.

# Para esto primero debemos crear el nuevo modelo.
modelo.H2 <- arima(serieTr.SinTendEst.H1, order=c(0,1,1))
valoresAjustados.H2 <- serieTr.SinTendEst.H1+modelo.H2$residuals


#Predecimos 
Predicciones.H2 <- predict(modelo.H2, n.ahead = NPred);
valoresPredichos.H2 <-Predicciones.H2$pred

#Calculamos el error cuadrático
errorTr.H2 <- sum((modelo.H2$residuals)^2)
errorTs.H2 <- sum((valoresPredichos.H2- serieTs.SinTendEst.H1)^2)


#Ahora creamos los test:

#Test de aleatoriedad
bpxtestM2 <- Box.test(modelo.H2$residuals)

#Test de normalidad
jb.M2 <- jarque.bera.test(modelo.H2$residuals)
sw.M2 <- shapiro.test(modelo.H2$residuals)


#El modelo 2 MA(1) también pasa los test te aleatoriedad y normalidad.

#Por ultimo usamos el test AIC para comprobar que modelo es mejor

AIC(modelo.H1, modelo.H2)

# El modelo H2 parece tener mejores resultados, vamos a comprobarlo con datos más cercanos a la realidad obteniendo las predicciones
# reales para ambos modelos y comparando. 

#***************************************************************
# PREDICCIÓN
#***************************************************************

# Ahora que tenemos validado el modelo volvemos a seguir todos los pasos pero sin dividirlos.      

serieEntera <- serie.log
tiempo <- 1:length(serieEntera)
parametros <- lm(serieEntera ~ tiempo)     
TendEstimada <- parametros$coefficients[1]+tiempo*parametros$coefficients[2]
serieSinTend <- serieEntera-TendEstimada
aux <- ts(serieEntera, f=12)
aux <- decompose(aux)$seasonal
estacionalidad <- as.numeric(aux[1:12])
aux<-rep(estacionalidad, length(serieSinTend)/length(estacionalidad))
serieSinTendEst <- serieSinTend-aux
modelo < -arima(serieSinTendEst, order=c(4,1,0))
valoresAjustatos <- serieSinTendEst+modelo$residuals
Predicciones <- predict(modelo, n.ahead = NPred)
valoresPredichos <- Predicciones$pred

# Por último calculamos las predicciones reales.   

# Ejercicio 10. Describir los pasos necesarios para conseguir la predicción real de los valores de la serie.

# Para realizar la predicción real, debemos tener en cuenta que los valores predichos, se han predicho en función de la
# serie sin componentes de tendencia o estacionareidad por ello, para volver a recuperar los datos reales, deberemos
# deshacer todo el proceso, sumando todo lo restado y resolviendo incluso la transformación logaritmica que se hara con su inversa
# es decir con una exponencial. 

valoresAjustados <- valoresAjustatos+aux
valoresPredichos <- valoresPredichos+estacionalidad

valoresAjustados <- valoresAjustados+TendEstimada
tiempoPred <- (tiempo[length(tiempo)]+(1:NPred))
TendEstimadaPred <- parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos <- valoresPredichos+TendEstimadaPred

valoresAjustados <- exp(valoresAjustados)
valoresPredichos <- exp(valoresPredichos)

plot.ts(serie, xlim=c(1, max(tiempoPred)), ylim=c(100,650))
lines(valoresAjustados, col="blue")
lines(valoresPredichos, col="red")


#Por último vamos a comprobar si ajustaria a la realidad ya que tenemos los datos disponibles

predReales <- scan("data/pasajeros_1960.predict")
lines(tiempoPred, predReales, col="green")

#Vemos que ajusta bastante bien. Pero vamos a calcular el error de la predicción.

ErrorMedio <- sum(abs(predReales-valoresPredichos))
ErrorMedio


#Por último vamos a crear una función capaz de obtener los datos predichos reales para probar el otro modelo disponible

predictTS <- function(serieEntera, AR=0, MA=0, kestacional)
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
  modelo <- arima(serieSinTendEst, order=c(AR,1,MA))
  valoresAjustatos <- serieSinTendEst+modelo$residuals
  Predicciones <- predict(modelo, n.ahead = NPred)
  valoresPredichos <- Predicciones$pred
  #Deshacemos pasos
  valoresAjustados <- valoresAjustatos+aux
  valoresPredichos <- valoresPredichos+estacionalidad
  
  valoresAjustados <- valoresAjustados+TendEstimada
  tiempoPred <- (tiempo[length(tiempo)]+(1:NPred))
  TendEstimadaPred <- parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
  valoresPredichos <- valoresPredichos+TendEstimadaPred
  
  valoresAjustados <- exp(valoresAjustados)
  valoresPredichos <- exp(valoresPredichos)
  
  return(list("Valores-Predichos"=valoresPredichos,
              "Valores-Ajustados"=valoresAjustados))
}

#Hacemos uso de esta función para probar el otro modelo. 

predicionArima011<- predictTS(serie.log, 0, 1, 12)

plot.ts(serie, xlim=c(1, max(tiempoPred)), ylim=c(100,650))
lines(predicionArima011$`Valores-Ajustados`, col="blue")
lines(predicionArima011$`Valores-Predichos`, col="red")
lines(tiempoPred, predReales, col="green")


ErrorMedio <- sum(abs(predReales-predicionArima011a$`Valores-Predichos`))
ErrorMedio

#Parece que el modelo con las medias móviles MA(1) ajusta mejor que el AR(4). El test de AIC estaba en lo cierto. 