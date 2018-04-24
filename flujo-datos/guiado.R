tablenb1<-read.table("data/clasificacion/nb1.txt", sep = ",")
tablenb2<-read.table("data/clasificacion/nb2.txt", sep = ",")
tablenb3<-read.table("data/clasificacion/nb3.txt", sep = ",")
tablenb4<-read.table("data/clasificacion/nb4.txt", sep = ",")
tablenb5<-read.table("data/clasificacion/nb5.txt", sep = ",")
tablenb6<-read.table("data/clasificacion/nb6.txt", sep = ",")
tablenb7<-read.table("data/clasificacion/nb7.txt", sep = ",")
tablenb8<-read.table("data/clasificacion/nb8.txt", sep = ",")
tablenb9<-read.table("data/clasificacion/nb9.txt", sep = ",")
tablenb10<-read.table("data/clasificacion/nb10.txt", sep = ",")
tablenb11<-read.table("data/clasificacion/nb11.txt", sep = ",")
tablenb12<-read.table("data/clasificacion/nb12.txt", sep = ",")
tablenb13<-read.table("data/clasificacion/nb13.txt", sep = ",")
tablenb14<-read.table("data/clasificacion/nb14.txt", sep = ",")
tablenb15<-read.table("data/clasificacion/nb15.txt", sep = ",")
tablenb16<-read.table("data/clasificacion/nb16.txt", sep = ",")
tablenb17<-read.table("data/clasificacion/nb17.txt", sep = ",")
tablenb18<-read.table("data/clasificacion/nb18.txt", sep = ",")
tablenb19<-read.table("data/clasificacion/nb19.txt", sep = ",")
tablenb20<-read.table("data/clasificacion/nb20.txt", sep = ",")
tablenb21<-read.table("data/clasificacion/nb21.txt", sep = ",")
tablenb22<-read.table("data/clasificacion/nb22.txt", sep = ",")
tablenb23<-read.table("data/clasificacion/nb23.txt", sep = ",")
tablenb24<-read.table("data/clasificacion/nb24.txt", sep = ",")
tablenb25<-read.table("data/clasificacion/nb25.txt", sep = ",")
tablenb26<-read.table("data/clasificacion/nb26.txt", sep = ",")
tablenb27<-read.table("data/clasificacion/nb27.txt", sep = ",")
tablenb28<-read.table("data/clasificacion/nb28.txt", sep = ",")
tablenb29<-read.table("data/clasificacion/nb29.txt", sep = ",")
tablenb30<-read.table("data/clasificacion/nb30.txt", sep = ",")



tableht1<-read.table("data/clasificacion/ht1.txt", sep = ",")
tableht2<-read.table("data/clasificacion/ht2.txt", sep = ",")
tableht3<-read.table("data/clasificacion/ht3.txt", sep = ",")
tableht4<-read.table("data/clasificacion/ht4.txt", sep = ",")
tableht5<-read.table("data/clasificacion/ht5.txt", sep = ",")
tableht6<-read.table("data/clasificacion/ht6.txt", sep = ",")
tableht7<-read.table("data/clasificacion/ht7.txt", sep = ",")
tableht8<-read.table("data/clasificacion/ht8.txt", sep = ",")
tableht9<-read.table("data/clasificacion/ht9.txt", sep = ",")
tableht10<-read.table("data/clasificacion/ht10.txt", sep = ",")
tableht11<-read.table("data/clasificacion/ht11.txt", sep = ",")
tableht12<-read.table("data/clasificacion/ht12.txt", sep = ",")
tableht13<-read.table("data/clasificacion/ht13.txt", sep = ",")
tableht14<-read.table("data/clasificacion/ht14.txt", sep = ",")
tableht15<-read.table("data/clasificacion/ht15.txt", sep = ",")
tableht16<-read.table("data/clasificacion/ht16.txt", sep = ",")
tableht17<-read.table("data/clasificacion/ht17.txt", sep = ",")
tableht18<-read.table("data/clasificacion/ht18.txt", sep = ",")
tableht19<-read.table("data/clasificacion/ht19.txt", sep = ",")
tableht20<-read.table("data/clasificacion/ht20.txt", sep = ",")
tableht21<-read.table("data/clasificacion/ht21.txt", sep = ",")
tableht22<-read.table("data/clasificacion/ht22.txt", sep = ",")
tableht23<-read.table("data/clasificacion/ht23.txt", sep = ",")
tableht24<-read.table("data/clasificacion/ht24.txt", sep = ",")
tableht25<-read.table("data/clasificacion/ht25.txt", sep = ",")
tableht26<-read.table("data/clasificacion/ht26.txt", sep = ",")
tableht27<-read.table("data/clasificacion/ht27.txt", sep = ",")
tableht28<-read.table("data/clasificacion/ht28.txt", sep = ",")
tableht29<-read.table("data/clasificacion/ht29.txt", sep = ",")
tableht30<-read.table("data/clasificacion/ht30.txt", sep = ",")


#RESULTADOS HT


resultadosHT<-vector()
resultadosHT[1]<-as.numeric(as.character(tableht1$V5[100]))
resultadosHT[2]<-as.numeric(as.character(tableht2$V5[100]))
resultadosHT[3]<-as.numeric(as.character(tableht3$V5[100]))
resultadosHT[4]<-as.numeric(as.character(tableht4$V5[100]))
resultadosHT[5]<-as.numeric(as.character(tableht5$V5[100]))
resultadosHT[6]<-as.numeric(as.character(tableht6$V5[100]))
resultadosHT[7]<-as.numeric(as.character(tableht7$V5[100]))
resultadosHT[8]<-as.numeric(as.character(tableht8$V5[100]))
resultadosHT[9]<-as.numeric(as.character(tableht9$V5[100]))
resultadosHT[10]<-as.numeric(as.character(tableht10$V5[100]))
resultadosHT[11]<-as.numeric(as.character(tableht11$V5[100]))
resultadosHT[12]<-as.numeric(as.character(tableht12$V5[100]))
resultadosHT[13]<-as.numeric(as.character(tableht13$V5[100]))
resultadosHT[14]<-as.numeric(as.character(tableht14$V5[100]))
resultadosHT[15]<-as.numeric(as.character(tableht15$V5[100]))
resultadosHT[16]<-as.numeric(as.character(tableht16$V5[100]))
resultadosHT[17]<-as.numeric(as.character(tableht17$V5[100]))
resultadosHT[18]<-as.numeric(as.character(tableht18$V5[100]))
resultadosHT[19]<-as.numeric(as.character(tableht19$V5[100]))
resultadosHT[20]<-as.numeric(as.character(tableht20$V5[100]))
resultadosHT[21]<-as.numeric(as.character(tableht21$V5[100]))
resultadosHT[22]<-as.numeric(as.character(tableht22$V5[100]))
resultadosHT[23]<-as.numeric(as.character(tableht23$V5[100]))
resultadosHT[24]<-as.numeric(as.character(tableht24$V5[100]))
resultadosHT[25]<-as.numeric(as.character(tableht25$V5[100]))
resultadosHT[26]<-as.numeric(as.character(tableht26$V5[100]))
resultadosHT[27]<-as.numeric(as.character(tableht27$V5[100]))
resultadosHT[28]<-as.numeric(as.character(tableht28$V5[100]))
resultadosHT[29]<-as.numeric(as.character(tableht29$V5[100]))
resultadosHT[30]<-as.numeric(as.character(tableht30$V5[100]))




resultadosnb<-vector()
resultadosnb[1]<-as.numeric(as.character(tablenb1$V5[100]))
resultadosnb[2]<-as.numeric(as.character(tablenb2$V5[100]))
resultadosnb[3]<-as.numeric(as.character(tablenb3$V5[100]))
resultadosnb[4]<-as.numeric(as.character(tablenb4$V5[100]))
resultadosnb[5]<-as.numeric(as.character(tablenb5$V5[100]))
resultadosnb[6]<-as.numeric(as.character(tablenb6$V5[100]))
resultadosnb[7]<-as.numeric(as.character(tablenb7$V5[100]))
resultadosnb[8]<-as.numeric(as.character(tablenb8$V5[100]))
resultadosnb[9]<-as.numeric(as.character(tablenb9$V5[100]))
resultadosnb[10]<-as.numeric(as.character(tablenb10$V5[100]))
resultadosnb[11]<-as.numeric(as.character(tablenb11$V5[100]))
resultadosnb[12]<-as.numeric(as.character(tablenb12$V5[100]))
resultadosnb[13]<-as.numeric(as.character(tablenb13$V5[100]))
resultadosnb[14]<-as.numeric(as.character(tablenb14$V5[100]))
resultadosnb[15]<-as.numeric(as.character(tablenb15$V5[100]))
resultadosnb[16]<-as.numeric(as.character(tablenb16$V5[100]))
resultadosnb[17]<-as.numeric(as.character(tablenb17$V5[100]))
resultadosnb[18]<-as.numeric(as.character(tablenb18$V5[100]))
resultadosnb[19]<-as.numeric(as.character(tablenb19$V5[100]))
resultadosnb[20]<-as.numeric(as.character(tablenb20$V5[100]))
resultadosnb[21]<-as.numeric(as.character(tablenb21$V5[100]))
resultadosnb[22]<-as.numeric(as.character(tablenb22$V5[100]))
resultadosnb[23]<-as.numeric(as.character(tablenb23$V5[100]))
resultadosnb[24]<-as.numeric(as.character(tablenb24$V5[100]))
resultadosnb[25]<-as.numeric(as.character(tablenb25$V5[100]))
resultadosnb[26]<-as.numeric(as.character(tablenb26$V5[100]))
resultadosnb[27]<-as.numeric(as.character(tablenb27$V5[100]))
resultadosnb[28]<-as.numeric(as.character(tablenb28$V5[100]))
resultadosnb[29]<-as.numeric(as.character(tablenb29$V5[100]))
resultadosnb[30]<-as.numeric(as.character(tablenb30$V5[100]))


#Vamos a comprobar si las distribuciones son normales

resultadosHT
resultadosnb

library(nortest)

# Como tenemos pocos datos usaremos el test de Cramer-von Mises

cvm.test(resultadosHT[1:30])
cvm.test(resultadosnb[1:30])

# Como en ambos casos p value p >= Alfa (0.05) No se rechaza Ho
# es decir, los datos provienen de una distribución normal.Por lo que 
# podemos usar un test paramétrico para ver si hay diferencias. 

wilcox.test(resultadosHT,resultadosnb, paired=T,conf.int = TRUE, exact = FALSE)

# Con un p-value = 1.825e-06, el test de Wilcoxon nos dice que hay
# diferencias significativas por lo que el mejor de los algoritmos es: Hoeffding Trees
# esto es debido a que tenemos una muestra normal y diferencias significativas
# por lo que nos fijamos en la media de acierto ofreciendo este mejores resultados.




#RESULTADOS CONCEPT DRIFT

#Leemos inicialmente los dináicos que tienen el mismo formato que anteriormente:

?read.table

tabledinamico1<-read.table("data/concept-drift/dinamico1.txt", sep = "," , header = T)
tabledinamico2<-read.table("data/concept-drift/dinamico2.txt", sep = "," , header = T)
tabledinamico3<-read.table("data/concept-drift/dinamico3.txt", sep = "," , header = T)
tabledinamico4<-read.table("data/concept-drift/dinamico4.txt", sep = "," , header = T)
tabledinamico5<-read.table("data/concept-drift/dinamico5.txt", sep = "," , header = T)
tabledinamico6<-read.table("data/concept-drift/dinamico6.txt", sep = "," , header = T)
tabledinamico7<-read.table("data/concept-drift/dinamico7.txt", sep = "," , header = T)
tabledinamico8<-read.table("data/concept-drift/dinamico8.txt", sep = "," , header = T)
tabledinamico9<-read.table("data/concept-drift/dinamico9.txt", sep = "," , header = T)
tabledinamico10<-read.table("data/concept-drift/dinamico10.txt", sep = "," , header = T)
tabledinamico11<-read.table("data/concept-drift/dinamico11.txt", sep = "," , header = T)
tabledinamico12<-read.table("data/concept-drift/dinamico12.txt", sep = "," , header = T)
tabledinamico13<-read.table("data/concept-drift/dinamico13.txt", sep = "," , header = T)
tabledinamico14<-read.table("data/concept-drift/dinamico14.txt", sep = "," , header = T)
tabledinamico15<-read.table("data/concept-drift/dinamico15.txt", sep = "," , header = T)
tabledinamico16<-read.table("data/concept-drift/dinamico16.txt", sep = "," , header = T)
tabledinamico17<-read.table("data/concept-drift/dinamico17.txt", sep = "," , header = T)
tabledinamico18<-read.table("data/concept-drift/dinamico18.txt", sep = "," , header = T)
tabledinamico19<-read.table("data/concept-drift/dinamico19.txt", sep = "," , header = T)
tabledinamico20<-read.table("data/concept-drift/dinamico20.txt", sep = "," , header = T)
tabledinamico21<-read.table("data/concept-drift/dinamico21.txt", sep = "," , header = T)
tabledinamico22<-read.table("data/concept-drift/dinamico22.txt", sep = "," , header = T)
tabledinamico23<-read.table("data/concept-drift/dinamico23.txt", sep = "," , header = T)
tabledinamico24<-read.table("data/concept-drift/dinamico24.txt", sep = "," , header = T)
tabledinamico25<-read.table("data/concept-drift/dinamico25.txt", sep = "," , header = T)
tabledinamico26<-read.table("data/concept-drift/dinamico26.txt", sep = "," , header = T)
tabledinamico27<-read.table("data/concept-drift/dinamico27.txt", sep = "," , header = T)
tabledinamico28<-read.table("data/concept-drift/dinamico28.txt", sep = "," , header = T)
tabledinamico29<-read.table("data/concept-drift/dinamico29.txt", sep = "," , header = T)
tabledinamico30<-read.table("data/concept-drift/dinamico30.txt", sep = "," , header = T)


resultadosdinamico<-vector()
resultadosdinamico[1]<-as.numeric(as.character(tabledinamico1$classifications.correct..percent.))
resultadosdinamico[2]<-as.numeric(as.character(tabledinamico2$classifications.correct..percent.))
resultadosdinamico[3]<-as.numeric(as.character(tabledinamico3$classifications.correct..percent.))
resultadosdinamico[4]<-as.numeric(as.character(tabledinamico4$classifications.correct..percent.))
resultadosdinamico[5]<-as.numeric(as.character(tabledinamico5$classifications.correct..percent.))
resultadosdinamico[6]<-as.numeric(as.character(tabledinamico6$classifications.correct..percent.))
resultadosdinamico[7]<-as.numeric(as.character(tabledinamico7$classifications.correct..percent.))
resultadosdinamico[8]<-as.numeric(as.character(tabledinamico8$classifications.correct..percent.))
resultadosdinamico[9]<-as.numeric(as.character(tabledinamico9$classifications.correct..percent.))
resultadosdinamico[10]<-as.numeric(as.character(tabledinamico10$classifications.correct..percent.))
resultadosdinamico[11]<-as.numeric(as.character(tabledinamico11$classifications.correct..percent.))
resultadosdinamico[12]<-as.numeric(as.character(tabledinamico12$classifications.correct..percent.))
resultadosdinamico[13]<-as.numeric(as.character(tabledinamico13$classifications.correct..percent.))
resultadosdinamico[14]<-as.numeric(as.character(tabledinamico14$classifications.correct..percent.))
resultadosdinamico[15]<-as.numeric(as.character(tabledinamico15$classifications.correct..percent.))
resultadosdinamico[16]<-as.numeric(as.character(tabledinamico16$classifications.correct..percent.))
resultadosdinamico[17]<-as.numeric(as.character(tabledinamico17$classifications.correct..percent.))
resultadosdinamico[18]<-as.numeric(as.character(tabledinamico18$classifications.correct..percent.))
resultadosdinamico[19]<-as.numeric(as.character(tabledinamico19$classifications.correct..percent.))
resultadosdinamico[20]<-as.numeric(as.character(tabledinamico20$classifications.correct..percent.))
resultadosdinamico[21]<-as.numeric(as.character(tabledinamico21$classifications.correct..percent.))
resultadosdinamico[22]<-as.numeric(as.character(tabledinamico22$classifications.correct..percent.))
resultadosdinamico[23]<-as.numeric(as.character(tabledinamico23$classifications.correct..percent.))
resultadosdinamico[24]<-as.numeric(as.character(tabledinamico24$classifications.correct..percent.))
resultadosdinamico[25]<-as.numeric(as.character(tabledinamico25$classifications.correct..percent.))
resultadosdinamico[26]<-as.numeric(as.character(tabledinamico26$classifications.correct..percent.))
resultadosdinamico[27]<-as.numeric(as.character(tabledinamico27$classifications.correct..percent.))
resultadosdinamico[28]<-as.numeric(as.character(tabledinamico28$classifications.correct..percent.))
resultadosdinamico[29]<-as.numeric(as.character(tabledinamico29$classifications.correct..percent.))
resultadosdinamico[30]<-as.numeric(as.character(tabledinamico30$classifications.correct..percent.))




# Los resultados del estático podemos obtenerlos a mano:


resultadosestatico<-c(80.359,80.402,80.535,80.418,80.696,80.499,80.394,80.568,80.627,80.444,80.374,80.487,80.497,80.607,
                      80.527,80.757,80.611,80.538, 80.577, 80.367, 80.551, 80.385, 80.502,80.487,80.319,80.755,80.517,
                      80.616, 80.404, 80.562)


resultadosestatico
resultadosdinamico


# Ahora comprobamos la normalidad de los datos para saber si podemos aplicar un test paramétrico de nuevo

# Como tenemos pocos datos usaremos el test de Cramer-von Mises

cvm.test(resultadosestatico[1:30])
cvm.test(resultadosdinamico[1:30])

# Como en ambos casos p value p >= Alfa (0.05) No se rechaza Ho
# es decir, los datos provienen de una distribución normal.Por lo que 
# podemos usar un test paramétrico para ver si hay diferencias. 

wilcox.test(resultadosestatico,resultadosdinamico, paired=T,conf.int = TRUE, exact = FALSE)

# Con un p-value = 1.825e-06, el test de Wilcoxon nos dice que hay
# diferencias significativas, tal y como habiamos pensado. 




