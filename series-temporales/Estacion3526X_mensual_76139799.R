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
seriemensual.ts <- ts(datosSerieMensual$x, f=6)
plot(decompose(seriemensual.ts))

#Parece que las varianzas cambian por lo que deberemos normalizar los datos con un logaritmico

seriemensual.ts <- log(seriemensual.ts)
seriemensual.log <- log(seriemensual)
plot(decompose(seriemensual.ts))