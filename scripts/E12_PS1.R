
#Junio 12 de 2022



# Punto 1: adquisici√≥n de datos -------------------------------------------



install.packages("rvest")

#Usar rvest / cargar librer√≠a

library(rvest)

# Cargar la p√°gina inicial del taller
# Esto crea la variable page1
# read_html carga una p√°gina desde una URL y lee el c√≥digo HTML
# La flecha <- asigna esos caracteres a la variable

page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")

#llamar page1 para ver que hay en la variable
page1

#Ver qu√© hay en los t√≠tulos de la tabla
# %>% es un pipe para enviar los datos que hay en la variable page1 a htlm_nodes 
#html_nodes devuelve una lista con los nodos (objetos) identificados 
#table > thead > tr > th con esto se consulta para cada tabla, todos los titulos (columnas),tr (filas) y th (contenido)
page1 %>% html_nodes("table > thead > tr > th")

#El resultado es que no hay tablas :( 
#Se eencuentr en el c√≥digo fuente de la p√°gina web que la tabla es llamada desde otra p√°gina web
#La tabla se carga desde un `div` usando `w3-include-html`.

#Hacer web scrapping te ese div.
#La consulta es: Deme un nodo (objeto) de tipo div cuyo atributo w3-include-html contiene la palabra pages
page1 %>% html_nodes("div[w3-include-html*='pages']")

# Los tags HTML est√°n compuestos de la siguiente forma:
# Signo Menor qu√©, seguido de una cadena de caracteres que identifican el tag y cierra 
# con el signo mayor qu√©. E.g. <div>
# Los tag se abren y cierran para simbolizar un objeto html, en el ejemplo anterior se
# abre un tag div, para tener un elemento completo, debo cerrarlo precediendo el identificador
# con un backslask "/". E.g. <div></div>
# Los elementos html tambi√©n pueden tener atributos que ayudan a especificar configuraci√≥n
# como por ejemplo una imagen tiene una fuente de donde obtener.
# E.g. <img src="http://www.imagenes.com/mi_imagen.jpg" />
# Los tags tambi√©n pueden cerrarse en el tag de apertura con un slash "/" antes del mayor qu√©
# 
page1 %>% html_nodes("div[w3-include-html*='pages']") %>%  html_attr("w3-include-html")

#Ya s√© cu√°l es la p√°gina y la leo
page1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")

# Ahora cargo la tabla
tabla_page1 <- page1 %>% html_nodes("table") %>% html_table()


head(tabla_page1)
tail(tabla_page1)

datosGEIH <- as.data.frame(tabla_page1)

#N˙mero de observaciones
nrow(datosGEIH)

#Ensayo para la p·gina 2

page2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
tabla_page2 <- page2 %>% html_nodes("table") %>% html_table()
tabla_page2  <- as.data.frame(tabla_page2)
datosGEIH <- rbind(datosGEIH,tabla_page2)
nrow(datosGEIH)


#Ensayo para la p·gina 3

i <- 3

#Esta lÌnea no funciona:

eval(as.name(paste0("page",i))) <- read_html(
  paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  )


#Como no funciona con <- , se asigna con "assign":

assign(paste0("page",i),read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")))

#Lo anterior funciona, pero es una sintaxis compleja. HabrÌa que hacer lo mismo para las dem·s lÌneas.
# øQuÈ tal con una lista?



#Inicializo una lista del tamaÒo que necesito, esto lo puedo mejorar despuÈs

page_list <- list(page1,page1,page1,page1,page1,page1,page1,page1,page1,page1)
tabla_page_list <- list(tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1,tabla_page1)

page_list[[i]] <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"))
tabla_page_list[[i]] <- page_list[[i]] %>% html_nodes("table") %>% html_table()
tabla_page_list[[i]] <- as.data.frame(tabla_page_list[[i]])
datosGEIH <- rbind(datosGEIH,tabla_page_list[[i]])
nrow(datosGEIH)



#Bucle para leer las dem·s p·ginas:

for (i in 4:10)
{
  page_list[[i]] <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html"))
  tabla_page_list[[i]] <- page_list[[i]] %>% html_nodes("table") %>% html_table()
  tabla_page_list[[i]] <- as.data.frame(tabla_page_list[[i]])
  datosGEIH <- rbind(datosGEIH,tabla_page_list[[i]])
  nrow(datosGEIH)
  
}

nrow(datosGEIH)

#Control para revisar si sÌ pegÛ bien los datos
max(datosGEIH$Var.1)

#Parece que no los pegÛ bien; revisar!


saveRDS(datosGEIH,"datosGEIH_complete.rds")


# Punto 2: limpieza de datos ----------------------------------------------




# Punto 3: modelo ingresos por edad ---------------------------------------



# Punto 4: modelo brecha de ingresos --------------------------------------



# Punto 5: modelo de predicci√≥n de ingresos -------------------------------


