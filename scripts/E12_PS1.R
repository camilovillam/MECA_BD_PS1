
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

page_list <- list(page1,page1,page1,page1,page1,page1,page1,page1,page1,page1)
tabla_page_list <- list(tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1)


#Parece que se me perdió el código en el que inicializaba estas listas de manera
#correcta (usando "vector") :(





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



# Enunciado:


# a.
# Split the sample into two samples: a training (70%) and a test (30%) sample.

# Don’t forget to set a seed 
# (in R, set.seed(10101), where 10101 is the seed.)


# i. Estimate a model that only includes a constant. This will be the benchmark.


# ii. Estimate again your previous models


# iii. In the previous sections, the estimated models haddifferent
# transformations of the dependent variable. At this point, explore other
# transformations of your independent variables also. For example, you can
# include polynomial terms of certain controls or interactions of these. Try at
# least five (5) models that are increasing in complexity.



# iv. Report and compare the average prediction error of all the models that
# you estimated before. Discuss the model with the lowest average prediction
# error.



# v. For the model with the lowest average prediction error, compute the
# leverage statistic for each observation in the test sample. Are there any
# outliers, i.e., observations with high leverage driving the results? Are these
# outliers potential people that the DIAN should look into, or are they just
# the product of a flawed model?
  



#b. Repeat the previous point but use K-fold cross-validation.
#Comment on similarities/differences of using this approach



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




# El siguiente es un código de ejemplo, todavía no es definitivo

reg1<-lm(logwage~schooling,db1)
reg2<-lm(logwage~schooling+ability,db1)

stargazer(reg1,reg2,type="text")


db1<- db1 %>% mutate(yhat_reg1=predict(reg1),
                     yhat_reg2=predict(reg2))

var(db1$yhat_reg1)
var(db1$yhat_reg2)


reg3<-lm(logwage~schooling,db2)
reg4<-lm(logwage~schooling+ability,db2)
stargazer(reg3,reg4,type="text")

db2$yhat_reg3<-predict(reg3)
db2$yhat_reg4<-predict(reg4)
var(db2$yhat_reg3)


data(matchdata) #loads the data

set.seed(101010) #sets a seed

matchdata <- matchdata %>%
  mutate(price=exp(lnprice),
         holdout= as.logical(1:nrow(matchdata) %in%
                               sample(nrow(matchdata),
                                      nrow(matchdata)*.2))
         )

test<-matchdata[matchdata$holdout==T,]
train<-matchdata[matchdata$holdout==F,]

#Naive approach

model1<-lm(price~1,data=train)
summary(model1)


coef(model1)

test$model1<-predict(model1,newdata = test)
with(test,mean((price-model1)^2))


#that was Starting point, now improve it:

model2<-lm(price~bedrooms,data=train)
test$model2<-predict(model2,newdata = test)
with(test,mean((price-model2)^2))

model3<-lm(price~bedrooms+bathrooms+centair+fireplace+brick,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((price-model3)^2))


?predict

model4<-lm(price~bedrooms+bathrooms+centair+fireplace+brick+
             lnland+lnbldg+rooms+garage1+garage2+dcbd+rr+
             yrbuilt+factor(carea)+latitude+longitude,data=train)
test$model4<-predict(model4,newdata = test)
with(test,mean((price-model4)^2))


#LOOCV:



