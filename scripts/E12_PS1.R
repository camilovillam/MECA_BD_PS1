
#Junio 12 de 2022


install.packages('GGally')# Se instala un paquete para gráficos
library(GGally)
library(stargazer)
library(tidyverse)

library(tableone)

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

#Inicializamos una lista del tamaño que necesito 

# (esto lo puedo mejorar después)

# page_list <- list(page1,page1,page1,page1,page1,page1,page1,page1,page1,page1)
# tabla_page_list <- list(tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1)

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


#Proceso de lectura de las 10 páginas "manual", uno a uno. Con fines comparativos y de control:

# page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
# tabla_page1 <- page1 %>% html_nodes("table") %>% html_table()
# tabla_page1  <- as.data.frame(tabla_page1)
# datosGEIH_m <- tabla_page1
# 
# page2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
# tabla_page2 <- page2 %>% html_nodes("table") %>% html_table()
# tabla_page2  <- as.data.frame(tabla_page2)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page2)
# 
# page3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")
# tabla_page3 <- page3 %>% html_nodes("table") %>% html_table()
# tabla_page3  <- as.data.frame(tabla_page3)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page3)
# 
# page4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")
# tabla_page4 <- page4 %>% html_nodes("table") %>% html_table()
# tabla_page4  <- as.data.frame(tabla_page4)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page4)
# 
# page5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")
# tabla_page5 <- page5 %>% html_nodes("table") %>% html_table()
# tabla_page5  <- as.data.frame(tabla_page5)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page5)
# 
# page6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")
# tabla_page6 <- page6 %>% html_nodes("table") %>% html_table()
# tabla_page6  <- as.data.frame(tabla_page6)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page6)
# 
# page7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")
# tabla_page7 <- page7 %>% html_nodes("table") %>% html_table()
# tabla_page7  <- as.data.frame(tabla_page7)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page7)
# 
# page8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")
# tabla_page8 <- page8 %>% html_nodes("table") %>% html_table()
# tabla_page8  <- as.data.frame(tabla_page8)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page8)
# 
# page9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")
# tabla_page9 <- page9 %>% html_nodes("table") %>% html_table()
# tabla_page9  <- as.data.frame(tabla_page9)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page9)
# 
# page10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")
# tabla_page10 <- page10 %>% html_nodes("table") %>% html_table()
# tabla_page10  <- as.data.frame(tabla_page10)
# datosGEIH_m <- rbind(datosGEIH_m,tabla_page10)

#Comparo si la tabla generada manualmente y la del For son iguales

# all_equal(datosGEIH, datosGEIH_m)


#Elimino las variables que no necesito
rm(list=ls(pattern="page"))
rm(data_control)
# rm(datosGEIH_m)


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



#TEMPORAL: CARGA:
setwd("~/GitHub/MECA_BD_PS1")
datosGEIH <-readRDS("./stores/datosGEIH_complete.rds")

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


#VARIABLES DE INTERÉS:

#Las variables que son determinantes para los ingresos son las relacionadas con:
#Edad: age -> Variable númerica
#Sexo: sex -> Variable categórica
#Nivel de educación: p6210 -> Variable categórica
#Años de educación: p6210 y p6210s1 -> Variable continua
#Profesión/oficio: oficio -> Variable categórica
#Tamaño de la empresa: sizeFirm -> Variable categórica
#Tipo de trabajador(#Tipo de contrato): formal, informal-> Variable categórica
#Número de personas en la empresa
#Horas trabajadas:totalHoursWorked -> Variable númerica

#Número de hijos: se calcula más adelante
#Experiencia laboral: no disponible, se calcula el proxy de Experiencia potencial.
#Tiempo en el trabajo actual (¿antigüedad?) - p6426

#Para la variable ingreso, se toma ingreso total, porque se asume como empleado
#también a los independientes, cuenta propias y contratistas ingreso: ingtot ->
#Variable númerica

#ingtot: variable Y

#Se crea un subconjunto con las variables de interés
datosGEIH_P2 <- subset(datosGEIH_P2,
                select=c(directorio,
                         secuencia_p,
                         orden,
                         ingtot,
                         age,
                         sex,
                         p6210,
                         p6210s1,
                         oficio,
                         sizeFirm,
                         formal,
                         totalHoursWorked,
                         p6426,
                         p6050
                         ))

#Se crean etiquetas para estas variables:

datosGEIH_P2 <-  apply_labels(datosGEIH_P2,
                              directorio="id directorio",
                              secuencia_p="id hogar",
                              orden="id persona en el hogar",
                              ingtot="Ingresos totales",
                              age="Edad",
                              sex="Sexo",
                              p6210="Nivel educativo",
                              p6210s1="Último grado",
                              oficio="Tipo de oficio",
                              sizeFirm="Tamaño de la empresa",
                              formal="Tipo de trabajo",
                              totalHoursWorked="Total horas de trabajo a la semana",
                              p6426="Antigüedad en el trabajo actual (meses)",
                              p6050="Parentezco con el jefe de familia")

#Se hace un resumen de estas variables

summary(datosGEIH_P2)


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

# # se quitan las variables que no tienen NAs
# filtro <- porcentaje_na$cantidad_na == 0
# variables_sin_na <- porcentaje_na[filtro, "variable"]
# variables_sin_na <- paste(variables_sin_na, collapse = ", ")
# print(paste("Las variables sin NAs son:", variables_sin_na))
# 
# porcentaje_na <- porcentaje_na[!filtro,]
# 
orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]

porcentaje_na$variable <- factor(porcentaje_na$variable,
                                  levels = orden)


# Se grafica el % de NA de las diferentes variables de interés
ggplot(porcentaje_na[1:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

# 
# ggplot(porcentaje_na[51:100,], 
#        aes(y = variable, x = cantidad_na)) +
#   geom_bar(stat = "identity", fill = "darkblue") +
#   geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
#             colour = "white", position = "dodge", hjust = 1.3,
#             size = 2, fontface = "bold") +
#   theme_classic() +
#   labs(x = "Porcentaje de NAs", y = "Variables") +
#   scale_x_continuous(labels = scales::percent, limits = c(0, 1))
# 
# ggplot(porcentaje_na[101:nrow(porcentaje_na),], 
#        aes(y = variable, x = cantidad_na)) +
#   geom_bar(stat = "identity", fill = "darkblue") +
#   geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
#             colour = "white", position = "dodge", hjust = 1.3,
#             size = 2, fontface = "bold") +
#   theme_classic() +
#   labs(x = "Porcentaje de NAs", y = "Variables") +
#   scale_x_continuous(labels = scales::percent, limits = c(0, 1))
# 


##Filtrado o imputación:

#NO QUEDAN REGISTROS DE NA EN LAS VARIABLES
#SE DEJA TODO ESTA SECCIÓN COMENTADA


# #Después de escoger las variables de interés,
# #podemos definir la estrategia de filtrado:
# 
# # a. Eliminar los NA en las variables de interés
# # b. Imputar mediana 
# # c. Imputar media
# 
# 
# # ##Se eliminan las variables que tienen más del 5% en NAS
# # filtro <- porcentaje_na$cantidad_na > 0.05
# # variables_eliminar <- porcentaje_na$variable[filtro]
# # k0 <- ncol(datosGEIH_P2)
# # datosGEIH_P2 <- (datosGEIH_P2) %>%
# #   select(-variables_eliminar)
# # k1 <- ncol(datosGEIH_P2)
# # print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))
# 
# 
# #Un primer filtro, general, es dejar solo los registros completos:
# 
# nrow(datosGEIH_P2)
# datosGEIH_P2_sin_NA <- datosGEIH_P2[complete.cases(datosGEIH_P2), ]
# nrow(datosGEIH_P2)
# 
# #Se pasa de 24.054 a 16.397 observaciones. Se eliminan 7657 observaciones.
# 
# 
# #Una segunda estrategia es imputar la mediana de cada variable a las faltantes:
# 
# # Número de filas
# nrow(datosGEIH_P2)#Ahora tiene 16397 filas
# 
# # Número de columnas
# ncol(datosGEIH_P2)#Ahora tiene 63 columnas
# 
# #Se vuelve aanalizar el número y el porcentaje de NAs por variable.
# 
# cantidad_na <- sapply(datosGEIH_P2, function(x) sum(is.na(x)))
# cantidad_na <- data.frame(cantidad_na)
# porcentaje_na <- cantidad_na/nrow(datosGEIH_P2)
# 
# # Porcentaje de observaciones faltantes. 
# p <- mean(porcentaje_na[,1])
# print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))
# #En promedio el 0.03% de las entradas están vacías"
# 



#Cálculo de variables adicionales:

#Después de haber filtrado / imputado valores, es posible hacer el 
#cálculo de variables adicionales de interés: años de educación,
#experiencia potencial y número de hijos.

#--
#Años de educación:

#Tal vez no considerar el nivel educativo como una categórica,
#sino calcular el número de años aproximado.
#Esto es importante porque el nivel terciario puede ser muy
#diferente, y puede haber importante variación en cantidad de
#años y salarios.

#Se pueden aproximar los años estudiados con la información en
#p6210 y p6210s1 (Nivel educativo y último año aprobado)

datosGEIH_P2 <- datosGEIH_P2 %>%
  mutate(años_educ = case_when(
    p6210 == 6 ~ 11 + as.numeric(p6210s1),
    p6210 == 9 ~ median(as.numeric(p6210s1)),#Se imputa la mediana a un par de casos
    TRUE ~ as.numeric(p6210s1)
  ))

#--

#--
#Años de experiencia laboral
#Se puede utilizar un proxy: Experiencia potencial:

# En la literatura se ha utilizado como proxy de la experiencia la experiencia
# potencial. Esta nace de restarle a la edad de la persona los años que ha
# estudiado y, además, cinco (5) años –pues en sus años de primera infancia ni
# estudió ni trabajó.

datosGEIH_P2$exper_pot <- (datosGEIH_P2$age - datosGEIH_P2$años_educ - 5)

#--

#--
#NÚMERO DE HIJOS:

#Hay evidencia en la teoría (ver artículo de Lorena)
#que indica que el número de hijos, para las mujeres,
#es determinante.

#La variable no está incorporada directamente en la BD,
#pero se puede calcular de manera indirecta con base en
#la variable P6050:

#P6050 ("PARENT")- PARENTESCO CON EL JEFE ACTUAL:
#   
# P6050 - Cuál es el parentesco de --- con el jefe o jefa:
# 1. Jefe (a) del hogar
# 2. Pareja, esposo(a), cónyuge, compañero(a)
# 3. Hijo(a), hijastro(a)  
# 4. Nieto(a)
# 5. Otro pariente
# 6. Empleado(a) del servicio doméstico y sus parientes
# 7. Pensionista
# 8. Trabajador
# 9. Otro no pariente

summary(datosGEIH_P2$p6050)
hist(datosGEIH_P2$p6050)

datosGEIH_P2$dummy_hijos <- ifelse(datosGEIH_P2$p6050==3,1,0)
summary(datosGEIH_P2$dummy_hijos)

#Se calcula el número de hijos por hogar:

num_hijos <- datosGEIH_P2 %>% 
  group_by(directorio,secuencia_p) %>%
  summarize(directorio,
            secuencia_p,
            orden,
            p6050,
            num_hijos=sum(dummy_hijos))


#Solo a quienes son padres/madres o su pareja
#se les imputa el número de hijos;
#a los demás, se les imputa 0.
#(esto es un proxy al número de hijos, puede haber imprecisiones)

num_hijos <- within(num_hijos, {
  num_hijos[p6050 != 1 & p6050 != 2] <- 0
})

#Luego se añaden estos hijos a los padres o madres
#Se combinan las dos bases de datos

datosGEIH_P2 <- 
  inner_join(datosGEIH_P2, num_hijos,
             by = c("directorio","secuencia_p","orden","p6050"))

#--  

#Variables cuadráticas
datosGEIH_P2$age_cuad <- datosGEIH_P2$age^2
datosGEIH_P2$años_educ_cuad <- datosGEIH_P2$años_educ^2
datosGEIH_P2$exp_pot_cuad <- datosGEIH_P2$exper_pot^2

datosGEIH_P2$p6426 <- datosGEIH_P2$p6426/12 #Se pasa a años



#prueba gráficas de las variables

?ggpairs

#ggpairs(datosGEIH_P2)

#variables númericas: histograma y diagrama de cajas
ggplot(datosGEIH_P2) + geom_histogram (aes(ingtot))
ggplot(datosGEIH_P2) + geom_boxplot (aes(ingtot))


ggplot(datosGEIH_P2) + geom_histogram (aes(age))
ggplot(datosGEIH_P2) + geom_boxplot (aes(age))

ggplot(datosGEIH_P2) + geom_histogram (aes(totalHoursWorked))
ggplot(datosGEIH_P2) + geom_boxplot (aes(totalHoursWorked))

ggplot(datosGEIH_P2) + geom_histogram (aes(años_educ))
ggplot(datosGEIH_P2) + geom_boxplot (aes(años_educ))

ggplot(datosGEIH_P2) + geom_histogram (aes(num_hijos))
ggplot(datosGEIH_P2) + geom_boxplot (aes(num_hijos))

  
#variables categóricas: diagrama de barras y diagrama de sectores 

ggplot(datosGEIH_P2) + geom_bar (aes(sex)) +
  labs(x='Sexo', y='Frecuencia', title='Diagrama de barras: Sexo')

ggplot(datosGEIH_P2) + geom_bar (aes(p6210))
ggplot(datosGEIH_P2) + geom_bar (aes(oficio))
ggplot(datosGEIH_P2) + geom_bar (aes(sizeFirm))
ggplot(datosGEIH_P2) + geom_bar (aes(formal))


#También faltan las tablas de estadísticas descriptivas
#de las variables empleadas (?)

#Antes de la tabla, se deben convertir las variables
#categóricas

datosGEIH_P2$sex <- factor(datosGEIH_P2$sex,
                           levels = c(0,1),
                           labels = c("Mujer", "Hombre"))

datosGEIH_P2$sizeFirm <- factor(datosGEIH_P2$sizeFirm,
                          levels = c(1,2,3,4,5),
                          labels = c("Independiente",
                                     "2-5 trabajadores",
                                     "6-10 trabajadores",
                                     "11-50 trabajadores",
                                     "Más de 50 trabajadores"))

datosGEIH_P2$formal <- factor(datosGEIH_P2$formal,
                              levels = c(0,1),
                              labels = c("Informal","Formal"))

datosGEIH_P2$oficio <- factor(datosGEIH_P2$oficio)

# #Por presentación, cambiamos los nombres de algunas variables:
# #
# 
# colnames(datosGEIH_P2)
# 
# colnames(datosGEIH_P2) <- c("directorio",       
#   "secuencia_p",      
#   "orden",           
#   "ingtot",           
#   "age",              
#   "sex",             
#   "nivel_educ",      #p6210     
#   "max_grad_educ", 	#p6201s1         
#   "oficio",          
#   "sizeFirm",         
#   "formal",           
#   "totalHoursWorked",
#   "antig_trab_act",		#p6426            
#   "parentezco",		#p6050            
#   "años_educ",       
#   "exper_pot",        
#   "dummy_hijos",      
#   "num_hijos",       
#   "age_cuad",         
#   "años_educ_cuad",   
#   "exp_pot_cuad")

#Tabla descriptiva:
#Se usa la librería "CreateTableOne" para crear una tabla con todas las variables

Tabla_descr <- CreateTableOne(data = datosGEIH_P2)
Tabla_descr
print(Tabla_descr,showAllLevels = TRUE)
summary(Tabla_descr)
Tabla_descr_csv <- print(Tabla_descr, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

## Save to a CSV file
setwd("~/GitHub/MECA_BD_PS1")
write.csv(Tabla_descr_csv, file = "./views/tabla_descr.csv")




#Guardo la base de datos en un archivo .rds
setwd("~/GitHub/MECA_BD_PS1")
saveRDS(datosGEIH_P2,"./stores/datosGEIH_P2.rds")


#Ensayo de modelo de regresión (adelantándome a otros puntos...)

reg_completa <- lm(ingtot ~ 
                     age + 
                     age_cuad +
                     sex +
                     años_educ +
                     años_educ_cuad +
                     oficio +
                     sizeFirm +
                     formal +
                     totalHoursWorked +
                     p6426 +
                     num_hijos+
                     sex:num_hijos,
                   data=datosGEIH_P2)

stargazer(reg_completa,type="text")


#Pregunta: ¿outliers? (Ver sección de "Leverage" en el punto 5).
#¿Qué hacer con los outliers?



# Punto 3: modelo ingresos por edad ---------------------------------------



# Punto 4: modelo brecha de ingresos --------------------------------------



# Punto 5: modelo de predicción de ingresos -------------------------------


