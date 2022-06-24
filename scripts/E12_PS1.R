
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

install.packages("pacman")
require(pacman)

# usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown

# Eliminamos todas las variables pre existentes
rm(list = ls())

#Se toma la tabla llamada datosGEIH y se crea una copia por si acaso
datosGEIH_P2 <- data.frame(datosGEIH)

# Número de filas
nrow(datosGEIH_P2)#Se inicia con 32177 filas

# Número de columnas
ncol(datosGEIH_P2)

# Vamos a analizar su estructura
str(datosGEIH_P2)

#El enunciado dice que debemos enfocarnos en empleados mayores de 18 años
#entonces se borran los datos de las personas que tienen 18 años o menos
datosGEIH_P2 <- datosGEIH_P2[datosGEIH_P2$age > 18, ] # Atención a la coma y el espacio al final

# Número de filas
nrow(datosGEIH_P2)#Quedan 24054 filas

#El enunciado dice que debemos enfocarnos en empleados mayores de 18 años
#entonces se borran los datos de las personas que no se encuentran ocupadas
datosGEIH_P2 <- datosGEIH_P2[datosGEIH_P2$ocu == 1, ] # Atención a la coma y el espacio al final

# Número de filas
nrow(datosGEIH_P2)#Quedan 16397 filas

#Ahora se analiza el número y el porcentaje de NAs por variable.

cantidad_na <- sapply(datosGEIH_P2, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(datosGEIH_P2)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 44.96% de las entradas están vacías"

#Se visualiza el porcentaje de observaciones faltantes por variable

# se ordena de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))

# se convierte el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# se quitan las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))

porcentaje_na <- porcentaje_na[!filtro,]

orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]
porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)

# Como son tantas variables se hacen 3 gráficas
ggplot(porcentaje_na[1:50,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

ggplot(porcentaje_na[51:100,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

ggplot(porcentaje_na[101:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

##Filrado

##Se eliminan las variables que tienen más del 5% en NAS
filtro <- porcentaje_na$cantidad_na > 0.05
variables_eliminar <- porcentaje_na$variable[filtro]
k0 <- ncol(datosGEIH_P2)
datosGEIH_P2 <- (datosGEIH_P2) %>%
  select(-variables_eliminar)
k1 <- ncol(datosGEIH_P2)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))

# Número de filas
nrow(datosGEIH_P2)#Ahora tiene 16397 filas

# Número de columnas
ncol(datosGEIH_P2)#Ahora tiene 63 columnas

#Se vuelve aanalizar el número y el porcentaje de NAs por variable.

cantidad_na <- sapply(datosGEIH_P2, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(datosGEIH_P2)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
#En promedio el 0.03% de las entradas están vacías"

#Las variables que son determinantes para los ingresos son las relacionadas con:
#Edad: age -> Variable númerica
#Sexo: sex -> Variable categórica
#Nivel de educación: p6210 -> Variable categórica
#Profesión/oficio: oficio -> Variable categórica
#Tamaño de la empresa: sizeFirm -> Variable categórica
#tipo de trabajador(#Tipo de contrato): formal, informal-> Variable categórica
#Número de personas en la empresa
#horas trabajadas:totalHoursWorked -> Variable númerica

#Para la variable ingreso, se toma ingreso total, porque se asume como empleado también a los independientes, cuenta propias y contratistas
#ingreso: ingtot -> Variable númerica

#Se crea un subconjunto con las variables de interes
db_P2=subset(datosGEIH_P2, select=c(ingtot,age,sex,p6210,oficio,sizeFirm,formal,informal,totalHoursWorked))

#Se hace un resumen de estas variables

summary(db_P2)

#prueba gráficas de las variables
install.packages('GGally')# Se instala un paquete para gráficos
library(GGally)
ggpairs(db_P2)

#variables númericas: histograma y diagrama de cajas
ggplot(db_P2) + geom_histogram (aes(ingtot))
ggplot(db_P2) + geom_boxplot (aes(ingtot))

ggplot(db_P2) + geom_histogram (aes(age))
ggplot(db_P2) + geom_boxplot (aes(age))

ggplot(db_P2) + geom_histogram (aes(totalHoursWorked))
ggplot(db_P2) + geom_boxplot (aes(totalHoursWorked))
  
#variables categóricas: diagrama de barras y diagrama de sectores 

ggplot(db_P2) + geom_bar (aes(sex)) +
  labs(x='Sexo', y='Frecuencia', title='Diagrama de barras: Sexo')

ggplot(db_P2) + geom_bar (aes(p6210))
ggplot(db_P2) + geom_bar (aes(oficio))
ggplot(db_P2) + geom_bar (aes(sizeFirm))
ggplot(db_P2) + geom_bar (aes(formal))

# Punto 3: modelo ingresos por edad ---------------------------------------



# Punto 4: modelo brecha de ingresos --------------------------------------



# Punto 5: modelo de predicción de ingresos -------------------------------


