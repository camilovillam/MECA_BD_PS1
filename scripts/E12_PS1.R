
#Junio 12 de 2022



# Punto 1: adquisición de datos -------------------------------------------


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

#Ahora se analiza el número y el porcentaje de NAs por variable.

?sapply

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
#¿Por qué se guardaron los nombres de variables como categóricas?
#Creo que cuando hay más de 99 niveles (creo) en una categórca no
#los toma como categóricas.


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
#Tipo de trabajador(#Tipo de contrato): formal, informal-> Variable categórica
#Número de personas en la empresa
#Horas trabajadas:totalHoursWorked -> Variable númerica


#Para la variable ingreso, se toma ingreso total, porque se asume como empleado
#también a los independientes, cuenta propias y contratistas ingreso: ingtot ->
#Variable númerica

#Se crea un subconjunto con las variables de interes
db_P2 <- subset(datosGEIH_P2,
                select=c(ingtot,age,sex,p6210,oficio,
                         sizeFirm,formal,informal,totalHoursWorked))


#--
#NÚMERO DE HIJOS:
#Se puede calcular al final, luego de filtrar las demás variables.

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

summary(datosGEIH$p6050)
hist(datosGEIH$p6050)

datosGEIH$dummy_hijos <- ifelse(datosGEIH$p6050==3,1,0)
summary(datosGEIH$dummy_hijos)

#Se calcula el número de hijos por hogar:

num_hijos <- datosGEIH %>% 
  group_by(directorio,secuencia_p) %>%
  summarize(directorio,
            secuencia_p,
            orden,
            num_hijos=sum(dummy_hijos))
            
#Luego se añaden estos hijos a los padres o madres
#Casos en que P6050 = 1 o 2 (para los demás, se supone 0)
#Esto es tal vez una aproximación?
#Cómo tratar a los nietos? No lo sé todavía.

nrow(datosGEIH)
nrow(num_hijos)

datosGEIH <- 
  inner_join(datosGEIH, num_hijos$num_hijos,
             by = c("directorio","secuencia_p","orden"))

#FALTA TERMINAR ESTO, QUE CUANDO NO SEAN NI PADRE NI MADRE
#QUEDE EN CERO
if(!(datosGEIH$p6050 %in% c(1,2))){datosGEIH$num_hijos <- 0}

#--  
  

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


