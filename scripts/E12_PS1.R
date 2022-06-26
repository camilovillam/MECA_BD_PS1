
#Junio 12 de 2022



# Punto 1: adquisición de datos -------------------------------------------



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

#Se crea una variable de interacción entre la edad y la variable dicotoma mujer
datosGEIH_P4 <- datosGEIH_P4 %>% mutate(mujer_age = age*mujer)

#Se prueban  regresiones
regP4_2<-lm(ln_ing~mujer+age, data=datosGEIH_P4)
regP4_3<-lm(ln_ing~mujer+age+age2, data=datosGEIH_P4)
regP4_4<-lm(ln_ing~mujer+age+age2+mujer_age, data=datosGEIH_P4)

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

#ggplot(datosGEIH_P4, aes(x=age, y=predict(regP4_4),color=mujer)) + 
  #geom_point() +
  #labs(x='Edad', y='logartimo ingreso estimado', title='Edad vs. logaritmo ingreso estimado')

#Siguiente inciso: usar bootstrap para calcular errores estandar e intervalos de confianza
#del "peak age" por genero

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
IC_inf_m <- peak_age_m - 1.96*0.5533107 #límite inferior del intervalo
IC_sup_m <- peak_age_m + 1.96*0.5533107 #límite superior del intervalo

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
IC_inf_h <- peak_age_h - 1.96*0.5983215 #límite inferior del intervalo
IC_sup_h <- peak_age_h + 1.96*0.5983215 #límite superior del intervalo


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

regP4_5<-lm(ln_ing~mujer+age+age2+mujer_age+factor(p6210), data=datosGEIH_P4)
summary(regP4_5)
stargazer(regP4_5,type="text")

#aplicación teorema FWL(Frisch-Waugh-Lovell)
library(dplyr)

reg_ing<-lm(ln_ing~age+age2+factor(p6210),datosGEIH_P4)
reg_mujer<-lm(mujer~age+age2+factor(p6210),datosGEIH_P4)
reg_age_mujer=lm(mujer_age~age+age2+factor(p6210),datosGEIH_P4)


datosGEIH_P4 <- datosGEIH_P4 %>% mutate (res_ing=reg_ing$residuals, 
                                        res_mujer=reg_mujer$residuals,
                                        res_age_mujer=reg_age_mujer$residuals
                                        )

regP4_6<-lm(res_ing~res_mujer+res_age_mujer,datosGEIH_P4)
stargazer(regP4_5,regP4_6,type="text",keep=c("mujer","res_mujer"))


# Punto 5: modelo de predicción de ingresos -------------------------------

