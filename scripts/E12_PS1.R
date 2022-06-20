
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


# Cargar la página inicial del taller
# Esto crea la variable page1
# read_html carga una página desde una URL y lee el código HTML
# La flecha <- asigna esos caracteres a la variable

page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")

#llamar page1 para ver que hay en la variable
page1

#Ver qué hay en los títulos de la tabla %>% es un pipe para enviar los datos que
#hay en la variable page1 a htlm_nodes html_nodes devuelve una lista con los
#nodos (objetos) identificados table > thead > tr > th con esto se consulta para
#cada tabla, todos los titulos (columnas),tr (filas) y th (contenido)
page1 %>% html_nodes("table > thead > tr > th")

#El resultado es que no hay tablas :( Se eecuentra en el código fuente de la
#página web que la tabla es llamada desde otra página web La tabla se carga
#desde un `div` usando `w3-include-html`.

#Hacer web scrapping te ese div. La consulta es: Deme un nodo (objeto) de tipo
#div cuyo atributo w3-include-html contiene la palabra pages
page1 %>% html_nodes("div[w3-include-html*='pages']")

# Los tags HTML están compuestos de la siguiente forma: Signo Menor qué, seguido
# de una cadena de caracteres que identifican el tag y cierra con el signo mayor
# qué. E.g. <div> Los tag se abren y cierran para simbolizar un objeto html, en
# el ejemplo anterior se abre un tag div, para tener un elemento completo, debo
# cerrarlo precediendo el identificador con un backslask "/". E.g. <div></div>
# Los elementos html también pueden tener atributos que ayudan a especificar
# configuración como por ejemplo una imagen tiene una fuente de donde obtener.
# E.g. <img src="http://www.imagenes.com/mi_imagen.jpg" /> Los tags también
# pueden cerrarse en el tag de apertura con un slash "/" antes del mayor qué
# 
page1 %>% html_nodes("div[w3-include-html*='pages']") %>%  html_attr("w3-include-html")

#Ya sé cuál es la página y la leo
page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")

# Ahora cargo la tabla
tabla_page1 <- page1 %>% html_nodes("table") %>% html_table()


head(tabla_page1)
tail(tabla_page1)

tabla_page1 <- as.data.frame(tabla_page1)

datosGEIH <- tabla_page1

# #Tabla de control: una tabla en donde vamos contando los números de filas
# y la suma de la edad de los individuos de la respectiva tabla, a modo de control.
# También se saca esta información para la tabla agregada que se va creando en cada iteración.
# Las columnas de control 
# Si las columnas de control son igual a cero, el proceso está bien.

data_control <- matrix(1:70,nrow=10,ncol=7)
colnames(data_control) <- c("Page","nrow_page","nrow_aggr","nrow_test",
                            "sum_age_page","sum_age_aggr","sum_age_test")

data_control[1,1] <- 1
data_control[1,2] <- nrow(tabla_page1)
data_control[1,3] <- nrow(datosGEIH)
data_control[1,4] <- 0
data_control[1,5] <- sum(tabla_page1$age)
data_control[1,6] <- sum(datosGEIH$age)
data_control[1,7] <- 0


#Para cargar las demás páginas se utiliza un bucle For y se va almacenando la
#info en listas: Inicializo una lista del tamaño que necesito (esto lo puedo
#mejorar después) Nota de Camilo: esto ya lo había corregido en otra versión del
#código! (último commit del 1? Revisar)

# page_list <- list(page1,page1,page1,page1,page1,page1,page1,page1,page1,page1)
# tabla_page_list <- list(tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1)


#Parece que se me perdió el código en el que inicializaba estas listas de manera
#correcta (usando "vector") :(


#Ya lo encontré, estaba en el Branch Punto 2 - Camilo. Ya hice Merge con Main.
#¿Esto irá a generar conflicto después?
  
  
page_list <- vector("list",10)
tabla_page_list <- vector("list",10)



#Bucle para leer las demás páginas:

for (i in 2:10)
{
  page_list[[i]] <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"))
  tabla_page_list[[i]] <- page_list[[i]] %>% html_nodes("table") %>% html_table()
  tabla_page_list[[i]] <- as.data.frame(tabla_page_list[[i]])

  datosGEIH <- rbind(datosGEIH,tabla_page_list[[i]])
  
  #Revisión de la consulta: tabla de control
  data_control[i,1] <- i
  data_control[i,2] <- nrow(tabla_page_list[[i]])
  data_control[i,3] <- nrow(datosGEIH)
  data_control[i,4] <- (data_control[i-1,3]+data_control[i,2])-data_control[i,3]
  data_control[i,5] <- sum(tabla_page_list[[i]]$age)
  data_control[i,6] <- sum(datosGEIH$age)
  data_control[i,7] <- (data_control[i-1,6]+data_control[i,5])-data_control[i,6]
  
  i
  }

nrow(datosGEIH)
max(datosGEIH$Var.1)
sum(datosGEIH$age)


#PROCESO MANUAL, PARA COMPARAR:

page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
tabla_page1 <- page1 %>% html_nodes("table") %>% html_table()
tabla_page1  <- as.data.frame(tabla_page1)
datosGEIH_m <- tabla_page1

page2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
tabla_page2 <- page2 %>% html_nodes("table") %>% html_table()
tabla_page2  <- as.data.frame(tabla_page2)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page2)

page3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")
tabla_page3 <- page3 %>% html_nodes("table") %>% html_table()
tabla_page3  <- as.data.frame(tabla_page3)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page3)

page4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")
tabla_page4 <- page4 %>% html_nodes("table") %>% html_table()
tabla_page4  <- as.data.frame(tabla_page4)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page4)

page5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")
tabla_page5 <- page5 %>% html_nodes("table") %>% html_table()
tabla_page5  <- as.data.frame(tabla_page5)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page5)

page6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")
tabla_page6 <- page6 %>% html_nodes("table") %>% html_table()
tabla_page6  <- as.data.frame(tabla_page6)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page6)

page7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")
tabla_page7 <- page7 %>% html_nodes("table") %>% html_table()
tabla_page7  <- as.data.frame(tabla_page7)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page7)

page8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")
tabla_page8 <- page8 %>% html_nodes("table") %>% html_table()
tabla_page8  <- as.data.frame(tabla_page8)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page8)

page9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")
tabla_page9 <- page9 %>% html_nodes("table") %>% html_table()
tabla_page9  <- as.data.frame(tabla_page9)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page9)

page10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")
tabla_page10 <- page10 %>% html_nodes("table") %>% html_table()
tabla_page10  <- as.data.frame(tabla_page10)
datosGEIH_m <- rbind(datosGEIH_m,tabla_page10)

#Comparo si la tabla generada manualmente y la del For son iguales

all_equal(datosGEIH, datosGEIH_m)

#Guardo la base de datos en un archivo .rds

setwd("~/GitHub/MECA_BD_PS1")
saveRDS(datosGEIH,"./stores/datosGEIH_complete.rds")


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
model2<-lm(ingtotob~age,data=training)

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
  
#PENDIENTE! VER CLASE Y CÓDIGO DE LEVERAGE!



#b. Repeat the previous point but use K-fold cross-validation.
#Comment on similarities/differences of using this approach

#Se debe repetir para todos los modelos de regresión del punto anterior...


#Cross-validation, model 1:
model1_CV_K <- train(ingtotob ~ .,
                     data = datosGEIH_P5,
                     trControl = trainControl(method = "cv", number = 5),
                     method = "null")

#Guardo el MSE en la tabla (lo debo elevar porque aquí calcula RMSE)
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
# • Calculate the prediction error for the i − th observation, i.e.
#   (yi − yˆi) 
# • Calculate the average of the numbers obtained in the previous
#   step to get the average mean square error. This is known as the Leave-One-Out
#   Cross-Validation (LOOCV) statistic.
# 
# ii. Compare the results to those obtained in the computation of the
# leverage statistic


#PUNTO C, PENDIENTE!



