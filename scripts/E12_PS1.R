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

page_list <- vector("list",10)
tabla_page_list <- vector("list",10)

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
##prueba para iniciar

#Se instalan los paquetes que pueden utlizarse durante el ejercicio
install.packages("pacman")

require(pacman)
p_load(rio, 
       tidyverse, 
       skimr, 
       caret,
       rvest)
##en este paso se carga la base de datos
GIH<- import("https://gitlab.com/Lectures-R/bd-meca-2022-summer/lecture-01/-/raw/main/data/GEIH_sample1.Rds")
browseURL("https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html")

##ahora se hace una descripción del contenido de la base de datos y de la variable y_inglab (ingreso laboral mensual, incluidos propinas y comisiones) 
summary("y_ingLab_m, na.rm=T")
summary(GIH$y_ingLab_m)

##resultado: (se pone una parcial, mientras se hace el punto 2) la variable ing:lab cuenta con un mínimo de 40.000 y un valor máximo de ingreso de 40.000.000. 
#De igual manera, cuenta con una mediana de 983140; es decir, es el valor que se encuentra en la mitad de los datos muestrales. Además, se cuenta conuna media de 1547069; es decir, una persona
#en promedio gana 1547069 en Colombia. 

##se crea una nueva variable, sin eliminar las demás, llamada age2, que corresponde a la variable edad elevada al cuadrado.
#Es importante aclarar que esta variable se convierte en exponencial para explicar el comportamiento del ingreso respecto de la edad; es decir,
#de acuerdo con la teoría económica laboral, una persona en la medida en que crece, adquiere no solo otros estudios académicos, sino una experiencia laboral, lo que le permite obtener un mayor ingreso con el paso del tiempo.
#Sin embargo, llega un punto (forma cóncava) donde la persona deja de ser menos productiva dada la edad.
GIH<-GIH %>% mutate (age2= age*age)
age2
#Otra forma de crear la variable al cuadrado.
a<-age^2

##**Antes de hacer la regresión, poner la explicación de la variable ingreso utilizada y poneranálisis del modelo
##Ahora se hace la regresión, donde y=ingreso laboral mensual y x=edad+edad^2. Luego se hace 
reg_1<-lm(y_ingLab_m~age+age2,GIH)
summary(reg1)

##Ahora se trae la librería tidiverse para hacer el plot de la predicción edad-ingreso
require("tidiverse")
GIH<-data.frame(age=runif(30,18,80))
GIH<- GIH %>% mutate(age2=age^2,
                     y_ingLab_m=rnorm(30,mean=12+0.06*age-0.001*age2))                
reg_1<-lm(y_ingLab_m~age+age2,GIH)
ggplot(GIH , mapping = aes(x = age , y = predict(reg_1))) +
  geom_point(col = "red" , size = 0.5)

#otra opción
ggplot(GIH, aes(x=age, y=predict(reg_1)))+geom_point() 
+ labs(x="y_ingLab_m",y="edad", title="grafico")+geom_point(col="red", size=0.5)
+geom_smooth(method="lm")

#########

install.packages("boot", dep=TRUE)
library(boot)

plot(hist(eta_reg1))
mean(eta_reg1)
reg_1
coefs<-reg_1$coef
coefs
b0<-coefs[0]
b1<-coefs[1]
b2<-coefs[2]

peak_age_a<-b1/2*(b2)
peak_age_a

results<-boot(GIH, peak_age_a, R=1000)

eta_mod_f<-function(GIH,index,
                    age_bar=mean(GIH$age)
                    age2_bar=mean(GIH$age2)){
  f<-lm(y_ingLab_m~age+age2,GIH,subset=index)
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  elastpt<-b2+2*b3*age_bar+b3*age2_bar
  return(elastpt)
}

eta_mod_f(GIH, 1:n)


# Punto 4: modelo brecha de ingresos --------------------------------------


#Se toma la tabla llamada datosGEIH y se crea una copia por si acaso

setwd("C:\\Users\\Lore Molano\\Documents\\MECA_BD_PS1") #ajustar dirección al inicio, dependiendo del pc
datosGEIH_P4 <-readRDS("./stores/datosGEIH_P2.rds")

summary(datosGEIH_P4)

#se toma la variable ingreso total observado (ingtot) como la variable ingreso

#Se convierte la variable ingreso (ingtot) en logaritmo y se guarda en la misma tabla
#El nombre de la variable ingreso en logaritmo es ln_ing
#https://www.programmingr.com/tutorial/natural-log-in-r/

datosGEIH_P4$ln_ing <- log(datosGEIH_P4$ingtot)

#NOTA: Se eliminan las filas  que tienen Inf de la variable ln_ing (revisar cuando se termine el P2)
datosGEIH_P4 <- datosGEIH_P4[is.finite(datosGEIH_P4$ln_ing), ]

#Se crea la variable mujer a partir de la variable sex y se deja en la misma tabla

library(dplyr) #se llama esta libreria

# Se crea una columna nueva basada en otra variable de otra columna
datosGEIH_P4 <- datosGEIH_P4 %>%
  mutate(mujer = case_when(
    sex == 'Mujer' ~ 1,
    sex == 'Hombre' ~ 0
  ))

#se verifica la variable mediante comparación
datosGEIH_P4 %>% select (sex,mujer) 

#se hace la pimera regresión del punto 4

#Reviso los valores de las variables antes de correr la regresión
datosGEIH_P4 %>% select (ln_ing,mujer)


#Se corre la regresión
regP4_1<-lm(ln_ing~mujer,data=datosGEIH_P4)
summary(regP4_1)


#Siguiente inciso: Estimación y grafica del perfil de edad versus ingresos pronosticado por género

#Se estima primero la regresión del logaritmo de ingreso
#para eso se crea la variable de edad al cuadrado y luego se corre la regresión

#Se crea una variable de interacción entre la edad y la variable dicotoma mujer
datosGEIH_P4 <- datosGEIH_P4 %>% mutate(mujer_age = age*mujer)

#Se prueban  regresiones
regP4_2<-lm(ln_ing~mujer+age, data=datosGEIH_P4)
regP4_3<-lm(ln_ing~mujer+age+age_cuad, data=datosGEIH_P4)
regP4_4<-lm(ln_ing~mujer+age+age_cuad+mujer_age, data=datosGEIH_P4)

#Se utiliza stargazer para viasualizar las regresiones
install.packages("stargazer")
require("stargazer")
stargazer(regP4_1,regP4_2,regP4_3,regP4_4,type="text")

#Se predice y grafica con la regresión regP4_4

library(ggplot2)

#plot predicted(ln_ing) vs. age

ggplot(datosGEIH_P4, aes(x=age, y=predict(regP4_4),color=mujer)) + 
  geom_point(aes(color=factor(mujer))) +
  labs(x='Edad', y='logartimo ingreso estimado', title='Edad vs. logaritmo ingreso estimado')

#Siguiente inciso: usar bootstrap para calcular errores estandar e intervalos de confianza
#del "peak age" por genero

install.packages('boot',dep=TRUE)
library(boot)

regP4_4 #se llama la regresión de interés que ya está definida previamente
coefs<-regP4_4$coef #se usa la función coef para extraer los coeficientes de la regresión
coefs#muestra los coeficientes

#se asignan los coeficientes calculados
b0<-coefs[1]
b1<-coefs[2]
b2<-coefs[3]
b3<-coefs[4]
b4<-coefs[5]

#Se define la función que calcula el "peak age" 

peak_age_m <- -(b2+b4)/(2*b3) #peak age si es mujer
peak_age_h <- -(b2)/(2*b3) #peak age si es hombre

peak_age_m
peak_age_h

#Se aplica el bootstrap para identificar el error estandar del peak age si es mujer
peakage_m.fn<-function(datosGEIH_P4,index){
  
  f<-lm(regP4_4,datosGEIH_P4, subset = index)
  coefs<-f$coef#se usa la función coef para extraer los coeficientes de la regresión
  b0<-coefs[1]#se asignan los coeficientes calculados
  b1<-coefs[2]
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5]
  peak_age_m <- -(b2+b4)/(2*b3) #peak age si es mujer
  
  return(peak_age_m)
}

results_m <- boot(datosGEIH_P4, peakage_m.fn,R=1000)
results_m

#Se calcula el interlvalo de confianza para el peak age si es mujer
# CI=[coef−1.96×SE,coef+1.96×SE]
#ojo con el error estándar, se debe cambiar si ajustamos la regresión o la base de datos
IC_inf_m <- peak_age_m - 1.96*0.6262007 #límite inferior del intervalo
IC_sup_m <- peak_age_m + 1.96*0.6262007  #límite superior del intervalo

#Se aplica el bootstrap para identificar error estandar del peak age si es hombre
peakage_h.fn<-function(datosGEIH_P4,index){
  
  f<-lm(regP4_4,datosGEIH_P4, subset = index)
  coefs<-f$coef#se usa la función coef para extraer los coeficientes de la regresión
  b0<-coefs[1]#se asignan los coeficientes calculados
  b1<-coefs[2]
  b2<-coefs[3]
  b3<-coefs[4]
  b4<-coefs[5]
  peak_age_h <- -(b2)/(2*b3) #peak age si es hombre

  return(peak_age_h)
}

results_h <- boot(datosGEIH_P4, peakage_h.fn,R=1000)
results_h

#Se calcula el interlvalo de confianza para el peak age si es hombre
# CI=[coef−1.96×SE,coef+1.96×SE]
#ojo con el error estándar, se debe cambiar si ajustamos la regresión o la base de datos
IC_inf_h <- peak_age_h - 1.96*0.7453415 #límite inferior del intervalo
IC_sup_h <- peak_age_h + 1.96*0.7453415 #límite superior del intervalo


#Se llaman los resultados del intervalo de confianza del peak age mujer para escribirlos en el texto
peak_age_m 
IC_inf_m
IC_sup_m

#Se llaman los resultados del intervalo de confianza del peak age hombre para escribirlos en el texto
peak_age_h 
IC_inf_h
IC_sup_h

#Siguiente inciso:  Equal Pay for Equal Work?

#Estimar la brecha condicional incorporando variables de control

#primero se debe formular la regresión con dicha variables de control
#se utiliza la función factor para crear variables dicotoma de cada categoría 
#se toma como variable de control: p6210 que indica el nivel de educación

#se prueban varias regresiones

regP4_5p1<-lm(ln_ing~mujer+
              age+
              age_cuad+
              mujer_age+
              (mujer*num_hijos)+
              años_educ+
              años_educ_cuad+
              factor(p6210)+
              factor(oficio)+
              exp_pot_cuad +
              sizeFirm+ 
              totalHoursWorked+
              formal
              , data=datosGEIH_P4)

summary(regP4_5p1)

regP4_5p2<-lm(ln_ing~mujer+
                age+
                age_cuad+
                mujer_age+
                años_educ+
                años_educ_cuad+
                factor(p6210)+
                factor(oficio)+
                exp_pot_cuad +
                sizeFirm+ 
                totalHoursWorked+
                formal
              , data=datosGEIH_P4)
summary(regP4_5p2)

regP4_5p3<-lm(ln_ing~mujer+
                age+
                age_cuad+
                mujer_age+
                años_educ+
                años_educ_cuad+
                factor(oficio)+
                exp_pot_cuad +
                sizeFirm+ 
                totalHoursWorked+
                formal
              , data=datosGEIH_P4)
summary(regP4_5p3)

regP4_5p4<-lm(ln_ing~mujer+
                age+
                age_cuad+
                mujer_age+
                factor(p6210)+
                factor(oficio)+
                exp_pot_cuad +
                sizeFirm+ 
                totalHoursWorked+
                formal
              , data=datosGEIH_P4)
summary(regP4_5p4)

stargazer(regP4_5p1,regP4_5p2,regP4_5p3,regP4_5p4,type="text")

regP4_5=regP4_5p3 #Se escoge el modelo regP4_5p3 porque los signos de esta regresión tienen mayor sentido

#aplicación teorema FWL(Frisch-Waugh-Lovell)
library(dplyr)#Siempre llamar esta libreria antes de correr la prueba 

reg_ing<-lm(ln_ing~age+age_cuad+años_educ+años_educ_cuad+factor(oficio)+exp_pot_cuad+sizeFirm+totalHoursWorked+formal,datosGEIH_P4)
reg_mujer<-lm(mujer~age+age_cuad+años_educ+años_educ_cuad+factor(oficio)+exp_pot_cuad+sizeFirm+totalHoursWorked+formal,datosGEIH_P4)
reg_age_mujer=lm(mujer_age~age+age_cuad+años_educ+años_educ_cuad+factor(oficio)+exp_pot_cuad+sizeFirm+totalHoursWorked+formal,datosGEIH_P4)

datosGEIH_P4 <- datosGEIH_P4 %>% mutate (res_ing=reg_ing$residuals,
                                         res_mujer=reg_mujer$residuals,
                                         res_age_mujer=reg_age_mujer$residuals)


regP4_6<-lm(res_ing~res_mujer+res_age_mujer,datosGEIH_P4)
stargazer(regP4_5,regP4_6,type="text",keep=c("mujer","res_mujer"))

#intento 3 de merge

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

