
#Creamos los vectores con los resultados de ambos algoritmos

resultadosp1ht_acc   <- c(84.641, 84.153, 84.799, 84.342, 84.481, 84.666, 84.59, 84.568, 84.512, 84.605, 84.434, 84.513,
                          84.626, 84.648, 84.547, 84.369, 84.457, 84.539, 84.578, 84.509)

resultadosp1ht_kappa <- c(76.963, 76.231, 77.2, 76.514, 76.723, 77.001, 76.887, 76.853, 76.77, 76.91, 76.653, 76.772,
                          76.94, 76.974, 76.822, 76.555, 76.688, 76.81, 76.869, 76.765)

resultadosp1hta_acc   <- c(84.478, 84.243, 84.271, 84.368, 84.262, 84.465, 84.416, 84.459, 84.474, 84.451, 84.456,
                           84.358, 84.328, 84.229, 84.415, 84.498, 84.416, 84.371, 84.326, 84.521 )

resultadosp1hta_kappa <- c(76.719, 76.367, 76.408, 76.554, 76.395, 76.699, 76.625, 76.69, 76.712, 76.679, 76.685,
                           76.539, 76.494, 76.345, 76.624, 76.749, 76.627, 76.558, 76.491, 76.783) 



# Creamos una tabla con los datos necesarios


tabla <- as.data.frame(cbind("HT Acc"=resultadosp1ht_acc, "HT Kappa"=resultadosp1ht_kappa,
                            "HT Adaptative Acc"=resultadosp1hta_acc, "HT Adaptative Kappa"=resultadosp1hta_kappa))



# Obtenemos información sobre la normalidad de la muestra

cvm.test(resultadosp1ht_acc)
cvm.test(resultadosp1hta_acc)


#Como los p-value son > 0.005, podemos concluir que la muestra es normal Usamos tstudent para ver si hay diferencias

t.test(resultadosp1ht_acc,resultadosp1hta_acc)


