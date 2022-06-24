
#Big Data and Machine Learning for Applied Economics
#MEcA - Uniandes
#Problem Set 1
#Equipo 12

#Junio 26, 2022


# Preliminares: preparación de espacio de trabajo y librerías -------------

install.packages("rvest")
install.packages("fabricatr")
install.packages("stargazer")

library(rvest)
library(tidyverse)
library(fabricatr)
library(stargazer)
library(caret)


# Punto 1: adquisición de datos -------------------------------------------



# Punto 2: limpieza de datos ----------------------------------------------



# Punto 3: modelo ingresos por edad ---------------------------------------



# Punto 4: modelo brecha de ingresos --------------------------------------



# Punto 5: modelo de predicción de ingresos -------------------------------



# Cargar base de datos


setwd("~/GitHub/MECA_BD_PS1")
datosGEIH_P5 <-readRDS("./stores/datosGEIH_complete.rds")


#Por ahora, elimino todos los NA en Ingtotob
#Luego se debe hacer un tratamiento mejor

nrow(datosGEIH_P5)
datosGEIH_P5 <- subset(datosGEIH_P5, !is.na(ingtotob))
nrow(datosGEIH_P5)


datosGEIH_P5$ln_ing <- log(datosGEIH_P5$ingtotob)


# Enunciado:


# a.
# Split the sample into two samples: a training (70%) and a test (30%) sample.
# Don’t forget to set a seed 
# (in R, set.seed(10101), where 10101 is the seed.)


set.seed(10101) #sets a seed

datosGEIH_P5 <- datosGEIH_P5 %>%
  mutate(test_dataset= as.logical(1:nrow(datosGEIH_P5) %in%
                               sample(nrow(datosGEIH_P5),
                                      nrow(datosGEIH_P5)*.3))
  )

summary(datosGEIH_P5$test_dataset)
mean(datosGEIH_P5$test_dataset)

test <- datosGEIH_P5[datosGEIH_P5$test_dataset==T,]
training <- datosGEIH_P5[datosGEIH_P5$test_dataset==F,]



# i. Estimate a model that only includes a constant. This will be the benchmark.


model1<-lm(ingtotob~1,data=training)
summary(model1)
stargazer(model1,type="text")

coef(model1)
mean(training$ingtotob) #aún falta tratar los NA


#Se predice el modelo:
test$model1 <- predict(model1,newdata = test)

#Se calcula el MSE:

#Guardo los MSE de los diferentes modelos en una matriz,
#para compararlos al final.

MSE_modelos <- matrix(rep(0,30),nrow=10,ncol=3)
colnames(MSE_modelos) <- c("Modelo","MSE","MSE_CV_5-fold")

MSE_modelos[1,1]=1
MSE_modelos[1,2] <- with(test,mean((ingtotob-model1)^2))



# ii. Estimate again your previous models

#Aquí es necesario tomar los modelos previos. ¿Cuáles son?

#Ejemplo: modelo sencillo con solo edad



#MODELO 2:
model2 <- lm(ingtotob~age,data=training)

#Se predice el modelo:
test$model2 <- predict(model2,newdata = test)

#Se calcula el MSE:

MSE_modelos[2,1]=2
MSE_modelos[2,2] <- with(test,mean((ingtotob-model2)^2))

stargazer(model1,model2,type="text")


#Para cada modelo, se sigue el mismo procedimiento anterior.

#MODELO N:
modelN<-lm(ingtotob~1,data=training)

#Se predice el modelo:
test$modelN <- predict(modelN,newdata = test)

#Se calcula el MSE:

MSE_modelos[N,1]=N
MSE_modelos[N,2] <- with(test,mean((ingtotob-modelN)^2))




# iii. In the previous sections, the estimated models had different
# transformations of the dependent variable. At this point, explore other
# transformations of your independent variables also. For example, you can
# include polynomial terms of certain controls or interactions of these. Try at
# least five (5) models that are increasing in complexity.

#Igual a la sección anterior:

#MODELO N:
modelN<-lm(ingtotob~1,data=training)

#Se predice el modelo:
test$modelN <- predict(modelN,newdata = test)

#Se calcula el MSE:

MSE_modelos[N,1]=N
MSE_modelos[N,2] <- with(test,mean((ingtotob-modelN)^2))


# iv. Report and compare the average prediction error of all the models that
# you estimated before. Discuss the model with the lowest average prediction
# error.

#Presenta la tabla:
stargazer(MSE_modelos,type="text")

#Encuentra el número del modelo con el menor MSE:
which.min(MSE_modelos[,2])




# v. For the model with the lowest average prediction error, compute the
# leverage statistic for each observation in the test sample. Are there any
# outliers, i.e., observations with high leverage driving the results? Are these
# outliers potential people that the DIAN should look into, or are they just
# the product of a flawed model?
  

#Suponiendo que es el modelo 2 (LUEGO SE AJUSTA AL QUE VERDADERAMENTE FUE)

#Lo calculo pero para el Test sample:

ggplot(test) +
  geom_point(aes(x=age,y=ingtotob))

model2_test<-lm(ingtotob~age,data=test)
stargazer(model2,model2_test,type="text")


#Calculamos α “a mano”:

# α=u/(1-h_j)
#   
#Con h_j es el j-ésimo elemento de la diagonal de Px

#Primero se hace para el primer elemento, con fines ilustrativos.
#Luego se hace un bucle.

u <- model2_test$residual[1]
u

h <- lm.influence(model2_test)$hat[1]
h

alpha <-u /(1-h)
alpha

rm(u,h,alpha)

#Inicializamos matriz para u,h,alpha con ceros:
matriz_u_h_alpha <- matrix(rep(0,nrow(test)*5),nrow=nrow(test),ncol=5)
colnames(matriz_u_h_alpha) <- c("Elemento_j","u","h","alpha","abs_alpha")

for (j in 1:nrow(test)){
  matriz_u_h_alpha[j,1]=j #Elemento j
  matriz_u_h_alpha[j,2] <- model2_test$residual[j] #u
  matriz_u_h_alpha[j,3] <- lm.influence(model2_test)$hat[j] #h
  matriz_u_h_alpha[j,4] <- matriz_u_h_alpha[j,2]/(1-matriz_u_h_alpha[j,3]) #alpha
  matriz_u_h_alpha[j,5] <- abs(matriz_u_h_alpha[j,4]) #Valor absoluto alpha
}


head(matriz_u_h_alpha)

leverage <- 
  matriz_u_h_alpha[order(matriz_u_h_alpha[,5],decreasing=TRUE),]

head(leverage)
tail(leverage)

#La medida de Leverage es relativa.
#¿Cómo determinar qué tanto pesan en la regresión?
#¿Cómo saber si es muy grande o no?
#Inquietud más conceptual, pendiente por resolver


#Guardamos en un nuevo df ingreso y edad de los 100 ingresos más altos de Test
pot_outliers <- test[leverage[1:100,1],c("ingtotob","age")]

#Una opción mejor es tal vez agregar una nueva columna al df:

test$pot_outliers <- 0
test$pot_outliers_rank <- 0

#Marcar los primeros 100 con un 1.
test[leverage[1:100,1],"pot_outliers"] <- 1

#También se puede guardar el "ranking" en la misma matriz de test
test[leverage[1:nrow(leverage),1],"pot_outliers_rank"] <- 1:nrow(leverage)



#Graficamos los 100 potenciales outliers en rojo:

ggplot() + 
  geom_point(data=test,aes(x=age,y=ingtotob),color='black') +
  geom_point(data=pot_outliers,aes(x=age,y=ingtotob),color='red')



#b. Repeat the previous point but use K-fold cross-validation.
#Comment on similarities/differences of using this approach

#Se debe repetir para todos los modelos de regresión del punto anterior...


#Cross-validation, model 1:
model1_CV_K <- train(ingtotob ~ .,
                     data = datosGEIH_P5,
                     trControl = trainControl(method = "cv", number = 5),
                     method = "null")

#Guardamos el MSE en la tabla (lo debo elevar porque aquí calcula RMSE)
MSE_modelos[1,3] <- model1_CV_K$results$RMSE^2


#Cross-validation, model 2:
model2_CV_K <- train(ingtotob ~ age,
                data = datosGEIH_P5,
                trControl = trainControl(method = "cv", number = 5),
                method = "lm")

#Guardo el MSE en la tabla
MSE_modelos[2,3] <- model2_CV_K$results$RMSE^2


#Cross-validation, model 2:
model2_CV_K <- train(ingtotob ~ age,
                     data = datosGEIH_P5,
                     trControl = trainControl(method = "cv", number = 5),
                     method = "lm")


#c. LOOCV. With your preferred predicted model (the one with the lowest
# average prediction error) perform the following exercise:
# 
# i. Write a loop that does the following:
# • Estimate the regression model using all but the i − th observation. 
#     (done)
# • Calculate the prediction error for the i − th observation, i.e.
#   (yi − yˆi) 
# • Calculate the average of the numbers obtained in the previous
#   step to get the average mean square error. This is known as the Leave-One-Out
#   Cross-Validation (LOOCV) statistic.
# 
# ii. Compare the results to those obtained in the computation of the
# leverage statistic


#Inicializamos una matriz vacía para guardar los MSE de LOOCV:
loocv_mat <- matrix(rep(0,nrow(datosGEIH_P5)),nrow=nrow(datosGEIH_P5),ncol=3)
colnames(loocv_mat) <- c("Y_k_observado","Y_k_predicho","Diferencia_^2")


#Antes de hacer el bucle, guardamos los Y observados:
loocv_mat[,1] <-  datosGEIH_P5$ingtotob

#Bucle calculando las regresiones y las predicciones de LOOCV:

for (k in 1:nrow(datosGEIH_P5)){
  #Regresión: con todos los elementos excepto el k-ésimo
  loocv_reg <- lm(ingtotob ~ age,data=datosGEIH_P5[-k,])
  
  #Predicción: solo con el k-ésimo
  loocv_mat[k,2] <- predict(loocv_regs,newdata = datosGEIH_P5[k,])
  
  #Diferencia al cuadrado
  loocv_mat[k,3] <- (loocv_mat[k,1]-loocv_mat[k,2])^2 #Observada-predicha
  
  }

#Estadístico LOOCV: Promedio de todas las diferencias al cuadrado
loocv_mse <- mean(loocv_mat[1:nrow(datosGEIH_P5),4])
loocv_mse


#Comparación de resultados:

MSE_modelos[1,2] #Modelo simple
MSE_modelos[2,2] #Modelo N
MSE_modelos[2,3] #MSE K-fold, K=5
loocv_mse #MSE con LOOCV

#El enunciado me pide comparar con las estadísticas de Leverage, pero
# no tengo claro qué se puede comparar con los leverage.
#¿Será un error en el enunciado?
#¿O realmente es algo que deba calcularse?

#Lo que sí se me ocurre comparar es el MSE del K-Fold con el del LOOCV y los
#demás modelos
  

MSE_modelos[2,3] #MSE K-fold, K=5
loocv_mse #MSE con LOOCV

(abs(MSE_modelos[2,3]-loocv_mse)/loocv_mse)*100

#El procedimiento de LOOCV se demoró aproximadamente XXXX minutos.
#El de K-fold, es menos de 10 segundos.
#El porcentaje de error entre uno y otro es de 0,33%.
#No se justifica el LOOCV por lo intensivo en cómputo.
