# José Ángel Díaz García 76139799R
# joseadiazg02@correo.ugr.es
# Ejercicio de trabajo autonomo. Series Temporales. Curso 2017-2018

#***************************************************************
# OBJETIVOS
#***************************************************************

# El objetivo de la práctica es predecir la temperatura maxima que tendremos la primera semana de marzo de 2018.

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

serie<-datos$Tmax
serie.ts <- ts(datos$Tmax, f=365)
plot(decompose(serie.ts))

#Parece que las varianzas cambian por lo que deberemos normalizar los datos con un logaritmico

serie.ts <- log(serie.ts)
serie.log <- log(serie)
plot(decompose(serie.ts))

#Tras esto dividiremos en training y test que dejaremos 7 días para predecir y ver como ajusta el modelo

NPred <- 7
NTest <- 7

serieTr <- serie.log[1:(length(serie.log)-NTest)]
tiempoTr <- 1:length(serieTr)
serieTs <- serie.log[(length(serie.log)-NTest+1):length(serie)]
tiempoTs <- (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

#Representamos gráficamente el espacio de training y test
plot.ts(serieTr, xlim=c(1, tiempoTs[length(tiempoTs)]))
lines(tiempoTs, serieTs, col="red")


#***************************************************************
# MODELADO DE LA TENDENCIA
#***************************************************************




