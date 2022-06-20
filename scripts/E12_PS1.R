
#Junio 12 de 2022



# Punto 1: adquisición de datos -------------------------------------------



install.packages("rvest")

#Usar rvest / cargar librería

library(rvest)
library(tidyverse)

# Cargar la página inicial del taller
# Esto crea la variable page1
# read_html carga una página desde una URL y lee el código HTML
# La flecha <- asigna esos caracteres a la variable

page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")

#llamar page1 para ver que hay en la variable
page1

#Ver qué hay en los títulos de la tabla
# %>% es un pipe para enviar los datos que hay en la variable page1 a htlm_nodes 
#html_nodes devuelve una lista con los nodos (objetos) identificados 
#table > thead > tr > th con esto se consulta para cada tabla, todos los titulos (columnas),tr (filas) y th (contenido)
page1 %>% html_nodes("table > thead > tr > th")

#El resultado es que no hay tablas :( 
#Se eecuentra en el código fuente de la página web que la tabla es llamada desde otra página web
#La tabla se carga desde un `div` usando `w3-include-html`.

#Hacer web scrapping te ese div.
#La consulta es: Deme un nodo (objeto) de tipo div cuyo atributo w3-include-html contiene la palabra pages
page1 %>% html_nodes("div[w3-include-html*='pages']")

# Los tags HTML están compuestos de la siguiente forma:
# Signo Menor qué, seguido de una cadena de caracteres que identifican el tag y cierra 
# con el signo mayor qué. E.g. <div>
# Los tag se abren y cierran para simbolizar un objeto html, en el ejemplo anterior se
# abre un tag div, para tener un elemento completo, debo cerrarlo precediendo el identificador
# con un backslask "/". E.g. <div></div>
# Los elementos html también pueden tener atributos que ayudan a especificar configuración
# como por ejemplo una imagen tiene una fuente de donde obtener.
# E.g. <img src="http://www.imagenes.com/mi_imagen.jpg" />
# Los tags también pueden cerrarse en el tag de apertura con un slash "/" antes del mayor qué
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
# Si las columnas de control son igual a cero, el proceso está bien (?).

data_control <- matrix(1:70,nrow=10,ncol=7)
colnames(data_control) <- c("Page","nrow_page","nrow_aggr","nrow_test","sum_age_page","sum_age_aggr","sum_age_test")

data_control[1,1] <- 1
data_control[1,2] <- nrow(tabla_page1)
data_control[1,3] <- nrow(datosGEIH)
data_control[1,4] <- 0
data_control[1,5] <- sum(tabla_page1$age)
data_control[1,6] <- sum(datosGEIH$age)
data_control[1,7] <- 0


#Para cargar las demás páginas se utiliza un bucle For y se va almacenando la info en listas:

#Inicializo una lista del tamaño que necesito (esto lo puedo mejorar después)

page_list <- list(page1,page1,page1,page1,page1,page1,page1,page1,page1,page1)
tabla_page_list <- list(tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1)



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


#Se toma la tabla llamada datosGEIH y se crea una copia por si acaso

datosGEIH_P4 <- data.frame(datosGEIH)

#se toma la variable ingreso total observado (ingtotob) como la variable ingreso

#Se convierte la variable ingreso (ingtotob) en logaritmo y se guarda en la misma tabla
#El nombre de la variable ingreso en logaritmo es ln_ing
#https://www.programmingr.com/tutorial/natural-log-in-r/

datosGEIH_P4$ln_ing <- log(datosGEIH_P4$ingtotob)

head(datosGEIH_P4)

#Se crea la variable mujer a partir de la variable sex y se deja en la misma tabla

library(dplyr) #se llama esta libreria

# Se crea una columna nueva basada en otra variable de otra columna
datosGEIH_P4 <- datosGEIH_P4 %>%
  mutate(mujer = case_when(
    sex == 0 ~ 1,
    sex == 1 ~ 0
  ))

#se verifica la variable mediante comparación
datosGEIH_P4 %>% select (sex,mujer) 

#se hace la pimera regresión del punto 4

#Reviso los valores de las variables antes de correr la regresión
datosGEIH_P4 %>% select (ln_ing,mujer)

#NOTA: Se eliminan los NA de las variables de la regresión (esto porque aun nos falta terminar el P2)
datosGEIH_P4 <- datosGEIH_P4 %>% drop_na(mujer,ln_ing)

#NOTA: Se eliminan las filas  que tienen Inf de la variable ln_ing (revisar cuando se termine el P2)
datosGEIH_P4 <- datosGEIH_P4[is.finite(datosGEIH_P4$ln_ing), ] 

#Se corre la regresión
regP4_1<-lm(ln_ing~mujer,data=datosGEIH_P4)
summary(regP4_1)

#Siguiente inciso: Estimación y grafica del perfil de edad versus ingresos pronosticado por género

#Se estima primero la regresión del logaritmo de ingreso
#para eso se crea la variable de edad al cuadrado y luego se corre la regresión

datosGEIH_P4 <- datosGEIH_P4 %>% mutate(age2 = age*age)

#Reviso los valores de las variables antes de correr la regresión
datosGEIH_P4 %>% select (age,age2)

#Se prueban  regresiones
regP4_2<-lm(ln_ing~mujer+age, data=datosGEIH_P4)
regP4_3<-lm(ln_ing~mujer+age+age2, data=datosGEIH_P4)
regP4_4<-lm(ln_ing~mujer+age+age2+(mujer*age), data=datosGEIH_P4)

#Se utiliza stargazer para viasualizar las regresiones
install.packages("stargazer")
require("stargazer")
stargazer(regP4_1,regP4_2,regP4_3,regP4_4,type="text")

#Se predice y grafica con la regresión regP4_4

library(ggplot2)

#plot predicted(ln_ing) vs. age

ggplot(datosGEIH_P4, aes(x=age, y=predict(regP4_4),color=mujer)) + 
  geom_point() +
  labs(x='Edad', y='logartimo ingreso estimado', title='Edad vs. logaritmo ingreso estimado')

#Siguiente inciso: usar bootstrap para calcular errores estandar e intervalos de confianza
#del "peak age" por genero

#https://towardsdatascience.com/a-practical-guide-to-bootstrap-with-r-examples-bd975ec6dcea

install.packages('boot',dep=TRUE)
library(boot)

regP4_4 #se llama la regresión de interés que ya está definida previamente
coefs<-regP4_4$coef #se usa la función coef para extraer los coeficientes de la regresión
coefs

#se asignan los coeficientes calculados
b0<-coefs[1]
b1<-coefs[2]
b2<-coefs[3]
b3<-coefs[4]
b4<-coefs[5]

#Se define la función que calcula el "peak age" 

peak_age_m <- -(b2+b4)/(2*b3) #peak age si es mujer
peak_age_h <- -(b2)/(2*b3) #peak age si es hombre

#boot(datosGEIH_P4, peak_age, R = 1000)
  
  
# Punto 5: modelo de predicción de ingresos -------------------------------


