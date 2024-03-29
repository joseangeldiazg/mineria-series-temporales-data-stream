#Vamos a comprobar si hay diferencias significativas entre ambos algoritmos

#Leemos los datos del experimento HT normal

htnormal1<-read.table("data/practica3/htnormal1.txt", sep = "," , header = T)
htnormal2<-read.table("data/practica3/htnormal2.txt", sep = "," , header = T)
htnormal3<-read.table("data/practica3/htnormal3.txt", sep = "," , header = T)
htnormal4<-read.table("data/practica3/htnormal4.txt", sep = "," , header = T)
htnormal5<-read.table("data/practica3/htnormal5.txt", sep = "," , header = T)
htnormal6<-read.table("data/practica3/htnormal6.txt", sep = "," , header = T)
htnormal7<-read.table("data/practica3/htnormal7.txt", sep = "," , header = T)
htnormal8<-read.table("data/practica3/htnormal8.txt", sep = "," , header = T)
htnormal9<-read.table("data/practica3/htnormal9.txt", sep = "," , header = T)
htnormal10<-read.table("data/practica3/htnormal10.txt", sep = "," , header = T)
htnormal11<-read.table("data/practica3/htnormal11.txt", sep = "," , header = T)
htnormal12<-read.table("data/practica3/htnormal12.txt", sep = "," , header = T)
htnormal13<-read.table("data/practica3/htnormal13.txt", sep = "," , header = T)
htnormal14<-read.table("data/practica3/htnormal14.txt", sep = "," , header = T)
htnormal15<-read.table("data/practica3/htnormal15.txt", sep = "," , header = T)
htnormal16<-read.table("data/practica3/htnormal16.txt", sep = "," , header = T)
htnormal17<-read.table("data/practica3/htnormal17.txt", sep = "," , header = T)
htnormal18<-read.table("data/practica3/htnormal18.txt", sep = "," , header = T)
htnormal19<-read.table("data/practica3/htnormal19.txt", sep = "," , header = T)
htnormal20<-read.table("data/practica3/htnormal20.txt", sep = "," , header = T)

#Leemos los datos del experimento HT adaptativo

htadaptativo1<-read.table("data/practica3/htadaptativo1.txt", sep = "," , header = T)
htadaptativo2<-read.table("data/practica3/htadaptativo2.txt", sep = "," , header = T)
htadaptativo3<-read.table("data/practica3/htadaptativo3.txt", sep = "," , header = T)
htadaptativo4<-read.table("data/practica3/htadaptativo4.txt", sep = "," , header = T)
htadaptativo5<-read.table("data/practica3/htadaptativo5.txt", sep = "," , header = T)
htadaptativo6<-read.table("data/practica3/htadaptativo6.txt", sep = "," , header = T)
htadaptativo7<-read.table("data/practica3/htadaptativo7.txt", sep = "," , header = T)
htadaptativo8<-read.table("data/practica3/htadaptativo8.txt", sep = "," , header = T)
htadaptativo9<-read.table("data/practica3/htadaptativo9.txt", sep = "," , header = T)
htadaptativo10<-read.table("data/practica3/htadaptativo10.txt", sep = "," , header = T)
htadaptativo11<-read.table("data/practica3/htadaptativo11.txt", sep = "," , header = T)
htadaptativo12<-read.table("data/practica3/htadaptativo12.txt", sep = "," , header = T)
htadaptativo13<-read.table("data/practica3/htadaptativo13.txt", sep = "," , header = T)
htadaptativo14<-read.table("data/practica3/htadaptativo14.txt", sep = "," , header = T)
htadaptativo15<-read.table("data/practica3/htadaptativo15.txt", sep = "," , header = T)
htadaptativo16<-read.table("data/practica3/htadaptativo16.txt", sep = "," , header = T)
htadaptativo17<-read.table("data/practica3/htadaptativo17.txt", sep = "," , header = T)
htadaptativo18<-read.table("data/practica3/htadaptativo18.txt", sep = "," , header = T)
htadaptativo19<-read.table("data/practica3/htadaptativo19.txt", sep = "," , header = T)
htadaptativo20<-read.table("data/practica3/htadaptativo20.txt", sep = "," , header = T)


resultadosp3HTA_acc<-vector()
resultadosp3HTA_acc[1]<-as.numeric(as.character(htadaptativo1$classifications.correct..percent.[20]))
resultadosp3HTA_acc[2]<-as.numeric(as.character(htadaptativo2$classifications.correct..percent.[20]))
resultadosp3HTA_acc[3]<-as.numeric(as.character(htadaptativo3$classifications.correct..percent.[20]))
resultadosp3HTA_acc[4]<-as.numeric(as.character(htadaptativo4$classifications.correct..percent.[20]))
resultadosp3HTA_acc[5]<-as.numeric(as.character(htadaptativo5$classifications.correct..percent.[20]))
resultadosp3HTA_acc[6]<-as.numeric(as.character(htadaptativo6$classifications.correct..percent.[20]))
resultadosp3HTA_acc[7]<-as.numeric(as.character(htadaptativo7$classifications.correct..percent.[20]))
resultadosp3HTA_acc[8]<-as.numeric(as.character(htadaptativo8$classifications.correct..percent.[20]))
resultadosp3HTA_acc[9]<-as.numeric(as.character(htadaptativo9$classifications.correct..percent.[20]))
resultadosp3HTA_acc[10]<-as.numeric(as.character(htadaptativo10$classifications.correct..percent.[20]))
resultadosp3HTA_acc[11]<-as.numeric(as.character(htadaptativo11$classifications.correct..percent.[20]))
resultadosp3HTA_acc[12]<-as.numeric(as.character(htadaptativo12$classifications.correct..percent.[20]))
resultadosp3HTA_acc[13]<-as.numeric(as.character(htadaptativo13$classifications.correct..percent.[20]))
resultadosp3HTA_acc[14]<-as.numeric(as.character(htadaptativo14$classifications.correct..percent.[20]))
resultadosp3HTA_acc[15]<-as.numeric(as.character(htadaptativo15$classifications.correct..percent.[20]))
resultadosp3HTA_acc[16]<-as.numeric(as.character(htadaptativo16$classifications.correct..percent.[20]))
resultadosp3HTA_acc[17]<-as.numeric(as.character(htadaptativo17$classifications.correct..percent.[20]))
resultadosp3HTA_acc[18]<-as.numeric(as.character(htadaptativo18$classifications.correct..percent.[20]))
resultadosp3HTA_acc[19]<-as.numeric(as.character(htadaptativo19$classifications.correct..percent.[20]))
resultadosp3HTA_acc[20]<-as.numeric(as.character(htadaptativo20$classifications.correct..percent.[20]))

resultadosp3HTA_kappa<-vector()
resultadosp3HTA_kappa[1]<-as.numeric(as.character(htadaptativo1$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[2]<-as.numeric(as.character(htadaptativo2$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[3]<-as.numeric(as.character(htadaptativo3$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[4]<-as.numeric(as.character(htadaptativo4$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[5]<-as.numeric(as.character(htadaptativo5$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[6]<-as.numeric(as.character(htadaptativo6$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[7]<-as.numeric(as.character(htadaptativo7$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[8]<-as.numeric(as.character(htadaptativo8$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[9]<-as.numeric(as.character(htadaptativo9$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[10]<-as.numeric(as.character(htadaptativo10$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[11]<-as.numeric(as.character(htadaptativo11$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[12]<-as.numeric(as.character(htadaptativo12$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[13]<-as.numeric(as.character(htadaptativo13$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[14]<-as.numeric(as.character(htadaptativo14$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[15]<-as.numeric(as.character(htadaptativo15$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[16]<-as.numeric(as.character(htadaptativo16$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[17]<-as.numeric(as.character(htadaptativo17$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[18]<-as.numeric(as.character(htadaptativo18$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[19]<-as.numeric(as.character(htadaptativo19$Kappa.Statistic..percent.[20]))
resultadosp3HTA_kappa[20]<-as.numeric(as.character(htadaptativo20$Kappa.Statistic..percent.[20]))


resultadosp3HT_acc<-vector()
resultadosp3HT_acc[1]<-as.numeric(as.character(htnormal1$classifications.correct..percent.[20]))
resultadosp3HT_acc[2]<-as.numeric(as.character(htnormal2$classifications.correct..percent.[20]))
resultadosp3HT_acc[3]<-as.numeric(as.character(htnormal3$classifications.correct..percent.[20]))
resultadosp3HT_acc[4]<-as.numeric(as.character(htnormal4$classifications.correct..percent.[20]))
resultadosp3HT_acc[5]<-as.numeric(as.character(htnormal5$classifications.correct..percent.[20]))
resultadosp3HT_acc[6]<-as.numeric(as.character(htnormal6$classifications.correct..percent.[20]))
resultadosp3HT_acc[7]<-as.numeric(as.character(htnormal7$classifications.correct..percent.[20]))
resultadosp3HT_acc[8]<-as.numeric(as.character(htnormal8$classifications.correct..percent.[20]))
resultadosp3HT_acc[9]<-as.numeric(as.character(htnormal9$classifications.correct..percent.[20]))
resultadosp3HT_acc[10]<-as.numeric(as.character(htnormal10$classifications.correct..percent.[20]))
resultadosp3HT_acc[11]<-as.numeric(as.character(htnormal11$classifications.correct..percent.[20]))
resultadosp3HT_acc[12]<-as.numeric(as.character(htnormal12$classifications.correct..percent.[20]))
resultadosp3HT_acc[13]<-as.numeric(as.character(htnormal13$classifications.correct..percent.[20]))
resultadosp3HT_acc[14]<-as.numeric(as.character(htnormal14$classifications.correct..percent.[20]))
resultadosp3HT_acc[15]<-as.numeric(as.character(htnormal15$classifications.correct..percent.[20]))
resultadosp3HT_acc[16]<-as.numeric(as.character(htnormal16$classifications.correct..percent.[20]))
resultadosp3HT_acc[17]<-as.numeric(as.character(htnormal17$classifications.correct..percent.[20]))
resultadosp3HT_acc[18]<-as.numeric(as.character(htnormal18$classifications.correct..percent.[20]))
resultadosp3HT_acc[19]<-as.numeric(as.character(htnormal19$classifications.correct..percent.[20]))
resultadosp3HT_acc[20]<-as.numeric(as.character(htnormal20$classifications.correct..percent.[20]))

resultadosp3HT_kappa<-vector()
resultadosp3HT_kappa[1]<-as.numeric(as.character(htnormal1$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[2]<-as.numeric(as.character(htnormal2$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[3]<-as.numeric(as.character(htnormal3$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[4]<-as.numeric(as.character(htnormal4$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[5]<-as.numeric(as.character(htnormal5$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[6]<-as.numeric(as.character(htnormal6$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[7]<-as.numeric(as.character(htnormal7$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[8]<-as.numeric(as.character(htnormal8$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[9]<-as.numeric(as.character(htnormal9$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[10]<-as.numeric(as.character(htnormal10$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[11]<-as.numeric(as.character(htnormal11$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[12]<-as.numeric(as.character(htnormal12$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[13]<-as.numeric(as.character(htnormal13$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[14]<-as.numeric(as.character(htnormal14$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[15]<-as.numeric(as.character(htnormal15$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[16]<-as.numeric(as.character(htnormal16$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[17]<-as.numeric(as.character(htnormal17$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[18]<-as.numeric(as.character(htnormal18$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[19]<-as.numeric(as.character(htnormal19$Kappa.Statistic..percent.[20]))
resultadosp3HT_kappa[20]<-as.numeric(as.character(htnormal20$Kappa.Statistic..percent.[20]))


resultadosp3HT_acc
resultadosp3HT_kappa

resultadosp3HTA_acc
resultadosp3HTA_kappa


#Fusionamos todo en una tabla:

tablap3 <- as.data.frame(cbind("HT Acc"=resultadosp3HT_acc, "HT Kappa"=resultadosp3HT_kappa,
                               "HT Adaptative Acc"=resultadosp3HTA_acc, "HT Adaptative Kappa"=resultadosp3HTA_kappa))


cvm.test(resultadosp3HTA_acc)
cvm.test(resultadosp3HT_acc)


#Como los p-value son > 0.005, podemos concluir que la muestra es normal Usamos tstudent para ver si hay diferencias

t.test(resultadosp3HT_acc,resultadosp3HTA_acc)
