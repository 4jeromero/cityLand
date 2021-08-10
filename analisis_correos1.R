library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(readxl)
library(tm)
library(cluster)
library(wordcloud)

correos <- read.csv(file = "datos/data-1628021216169.csv", encoding = "UTF-8")
# propietario <- read_excel(path = "datos/Santamarta_R1_03_agosto_2021.xlsx", sheet = "General")


# write.xlsx(correosA, file = "correos.xlsx")
missings <- function(x) return(sum(is.na(x)))

cleanCadena <- function(x){
  x <- str_to_lower(x)   
  x <- str_replace_all(x,"[^a-záéíóúüñ  ]","")
  x <- str_squish(x)
  return(x)
}

cleanCorreo <- function(x){
  x <- str_to_lower(x)   
  x <- chartr('áéíóúüñ', 'aeiouuñ', x) # quitar tildes
  x <- str_squish(x)
  return(x)
}

correos$remitente <- cleanCadena(correos$remitente)
correos$subject <- cleanCorreo(correos$subject)
correos$contenido <- cleanCorreo(correos$contenido)
correos$adjuntos <- cleanCadena(correos$adjuntos)
correos$archivos <- cleanCadena(correos$archivos)
correos$carpeta <- cleanCadena(correos$carpeta)
correos$correo <- cleanCorreo(correos$correo)
correos <- correos %>% mutate_if(is.character, list(~na_if(.,""))) 

recibidos <- correos%>%
  filter(carpeta!="elementos enviados" & !is.na(remitente) &
           (!is.na(contenido) | !is.na(subject)))

#####################################
r11 <- recibidos %>%
  filter(correo!="grupo@")%>%
  mutate(tipoPersona="Natural")

r12 <- recibidos %>%
  filter(correo=="grupo@")%>%
  mutate(tipoPersona="Juridica")

recibidos <- full_join(r11,r12)
#####################################
r21 <- recibidos %>%
  filter(adjuntos=="true")%>%
  mutate(adj="Si")

r22 <- recibidos %>%
  filter(adjuntos!="true")%>%
  mutate(adj="No")

recibidos <- full_join(r21,r22)
######################################
# mutaciones
contenido <- recibidos%>%
  select(contenido)

subject <- recibidos%>%
  select(subject)

remitente <- recibidos%>%
  select(remitente)

#contenido <- head(contenido,1000)
contenido <- removeWords(contenido$contenido, words = stopwords("spanish"))
contenido <- removePunctuation(contenido)
#contenido <- removeNumbers(contenido)
contenido <- stripWhitespace(contenido)
contenido <- chartr('áéíóúüñ', 'aeiouuñ', contenido) # quitar tildes

subject <- removeWords(subject$subject, words = stopwords("spanish"))
subject <- removePunctuation(subject)
#subject <- removeNumbers(subject)
subject <- stripWhitespace(subject)
subject <- chartr('áéíóúüñ', 'aeiouuñ', subject) # quitar tildes


remitente <- removeWords(remitente$remitente, words = stopwords("spanish"))
remitente <- removePunctuation(remitente)
#remitente <- removeNumbers(remitente)
remitente <- stripWhitespace(remitente)
remitente <- chartr('áéíóúüñ', 'aeiouuñ', remitente) # quitar tildes

recibidos$contenido <-contenido
recibidos$subject <- subject
recibidos$remitente <- remitente

#write.xlsx(recibidos, "recibidos1.xlsx")
recibidos1 <- read_excel(path = "recibidos1.xlsx")

texto <- recibidos1%>%
  select(texto)

texto <- removeWords(texto$texto, words = stopwords("spanish"))
texto <- removePunctuation(texto)
#texto <- removeNumbers(texto)
texto <- stripWhitespace(texto)
texto <- chartr('áéíóúüñ', 'aeiouuñ', texto) # quitar tildes

#stringr::str_detect(a, )
#stringr::str_match(string = a, pattern = c("Hola", "series"))
#stringr::str_count(a, "la")
#str_extract(contenido, "entiendo")
#x <- as.vector(contenido)

# ingresa la palabra clave, de la clase 1
#buscador <- function(t, p, n){
  a = NULL
  for (i in 1:n) {
    a[i] = stringr::str_count(t, p[i])
  }
  sum(a)
}
clase1 <- c("propietario", "poseedor", "cambio propietario", "traspaso", "vendieron",
            "escrituras", "poseer")

a1 <- stringr::str_count(texto, clase1[1])
a2 <- stringr::str_count(texto, clase1[2])
a3 <- stringr::str_count(texto, clase1[3])
a4 <- stringr::str_count(texto, clase1[4])
a5 <- stringr::str_count(texto, clase1[5])
a6 <- stringr::str_count(texto, clase1[6])
clase1 <- a1+a2+a3+a4+a5+a6

# clase 2
clase2 <- c("englobe", "desenglobe", "agrege", "segregacion", "agregacion",
            "constructora", "division", "dividi", "separar")
a1 <- stringr::str_count(texto, clase2[1])
a2 <- stringr::str_count(texto, clase2[2])
a3 <- stringr::str_count(texto, clase2[3])
a4 <- stringr::str_count(texto, clase2[4])
a5 <- stringr::str_count(texto, clase2[5])
a6 <- stringr::str_count(texto, clase2[6])
a7 <- stringr::str_count(texto, clase2[7])
a8 <- stringr::str_count(texto, clase2[8])
a9 <- stringr::str_count(texto, clase2[9])
clase2 <- a1+a2+a3+a4+a5+a6+a7+a8+a9

clase3 <- c("contruccion", "edificar", "demoli", "area", "edificacion", "construir")
a1 <- stringr::str_count(texto, clase3[1])
a2 <- stringr::str_count(texto, clase3[2])
a3 <- stringr::str_count(texto, clase3[3])
a4 <- stringr::str_count(texto, clase3[4])
a5 <- stringr::str_count(texto, clase3[5])
clase3 <- a1+a2+a3+a4+a5

clase4 <- c("avaluo", "valor", "cambio avaluo", "revision avaluo", "precio")
a1 <- stringr::str_count(texto, clase4[1])
a2 <- stringr::str_count(texto, clase4[2])
a3 <- stringr::str_count(texto, clase4[3])
a4 <- stringr::str_count(texto, clase4[4])
a5 <- stringr::str_count(texto, clase4[5])
clase4 <- a1+a2+a3+a4+a5

clase5 <- c("mejora", "mejoras", "inscribir", "inscripcion")
a1 <- stringr::str_count(texto, clase5[1])
a2 <- stringr::str_count(texto, clase5[2])
a3 <- stringr::str_count(texto, clase5[3])
a4 <- stringr::str_count(texto, clase5[4])
clase5 <- a1+a2+a3+a4

# variable dicotomica
rectificacion <- c("uso", "utilizacion", "utilizar", "generacion", "ficha", "genere",
                   "rectificacion", "correccion")
a1 <- stringr::str_count(texto, rectificacion[1])
a2 <- stringr::str_count(texto, rectificacion[2])
a3 <- stringr::str_count(texto, rectificacion[3])
a4 <- stringr::str_count(texto, rectificacion[4])
a5 <- stringr::str_count(texto, rectificacion[5])
a6 <- stringr::str_count(texto, rectificacion[6])
a7 <- stringr::str_count(texto, rectificacion[7])
rectificacion <- a1+a2+a3+a4+a5+a6+a7

# variable dicotomica
ventaServicio <- c("certificado", "venta", "comprar", "genere", "ficha", "expedicion")
a1 <- stringr::str_count(texto, ventaServicio[1])
a2 <- stringr::str_count(texto, ventaServicio[2])
a3 <- stringr::str_count(texto, ventaServicio[3])
a4 <- stringr::str_count(texto, ventaServicio[4])
a5 <- stringr::str_count(texto, ventaServicio[5])
a6 <- stringr::str_count(texto, ventaServicio[6])
ventaServicio <- a1+a2+a3+a4+a5+a6

# variable dicotomica
tutela <- c("tutelas", "abogado", "tutela", "juzgado", "contra", "escrito tutela",
            "juez", "rama judicial", "interponer")
a1 <- stringr::str_count(texto, tutela[1])
a2 <- stringr::str_count(texto, tutela[2])
a3 <- stringr::str_count(texto, tutela[3])
a4 <- stringr::str_count(texto, tutela[4])
a5 <- stringr::str_count(texto, tutela[5])
a6 <- stringr::str_count(texto, tutela[6])
a7 <- stringr::str_count(texto, tutela[7])
a8 <- stringr::str_count(texto, tutela[8])
a9 <- stringr::str_count(texto, tutela[9])
tutela <- a1+a2+a3+a4+a5+a6+a7+a8+a9

derechoPeticion <- c("derecho", "peticion", "pqr")
a1 <- stringr::str_count(texto, derechoPeticion[1])
a2 <- stringr::str_count(texto, derechoPeticion[2])
a3 <- stringr::str_count(texto, derechoPeticion[3])
derechoPeticion <- a1+a2+a3

id_predio <- c("folio", "matr", "matricula")
a1 <- stringr::str_count(texto, id_predio[1])
a2 <- stringr::str_count(texto, id_predio[2])
a3 <- stringr::str_count(texto, id_predio[3])
id_predio <- a1+a2+a3


# agrega las nuevas variables
recibidos1 <- recibidos1%>%
  mutate(clase1, clase2, clase3, clase4, clase5, rectificacion, ventaServicio, tutela,
         derechoPeticion, id_predio)

recibidos1 <- recibidos1%>%
  mutate(tutelaC = if_else(tutela>=1,"si","no"),
         rectificacionC = if_else(rectificacion>=1,"si","no"),
         ventaServicioC = if_else(ventaServicio>=1,"si","no"),
         derechoPeticionC = if_else(derechoPeticion>=1,"si","no"),
         id_predioC = if_else(id_predio>=1,"si","no"))

# crear la variable mutacionC
clase <- cbind(clase1,clase2,clase3,clase4,clase5)

recibidos2 <- recibidos1%>%
  filter(clase1==0 & clase2==0 & clase3==0 & clase4==0 & clase5==0)%>%
  mutate(mutacion=0)

recibidos3 <- recibidos1%>%
  filter((clase1!=0 | clase2!=0 | clase3!=0 | clase4!=0 | clase5!=0))

#clase1 <- head(clase, 10)
#clase1

clase00 <- cbind(recibidos3$clase1,
      recibidos3$clase2,
      recibidos3$clase3,
      recibidos3$clase4,
      recibidos3$clase5) 

b <- NULL
for (i in 1:length(clase00[,1])) {
  b[i] <- which.max(clase00[i,])
}

recibidos3 <- recibidos3%>%
  mutate(mutacion = b)
############################################
recibidos4 <- rbind(recibidos2, recibidos3)
table(recibidos4$mutacion)
############################################



########################
RsinTsinVsinD <- recibidos4 %>%
  filter(ventaServicioC=="no" & tutelaC=="no" & derechoPeticionC=="no")

tutelaDF <- recibidos%>%
  filter(tutelaC=="si")

ventaDF <- recibidos%>%
  filter(ventaServicioC=="si")

derechoPeticionDF <- recibidos%>%
  filter(derechoPeticionC=="si")

# exportacion
#write.xlsx(tutelaDF, file = "Recibido_conT_050821.xlsx")
#write.xlsx(ventaDF, file = "Recibido_conV_050821.xlsx")
#write.xlsx(derechoPeticionDF, file = "Recibido_conD_050821.xlsx")
#write.xlsx(RsinTsinVsinD, file = "Recibido_sinT_sinV_sinD_050821.xlsx")
write.xlsx(recibidos4, file = "recibidos_050821.xlsx")

#########################################
t3 <- recibidos4%>%
  group_by(ventaServicioC, derechoPeticionC, tutelaC, tipoPersona, mutacion,
           rectificacionC, adj, id_predioC)%>%
  summarise(n= n())%>%
  arrange(desc(n))%>%
  as.data.frame()

#write.xlsx(t3, file = "clasificacion_050821.xlsx")

recibidos%>%
  filter(tutelaC=="no")%>%
  summarise(n())

recibidos%>%
  filter(derechoPeticionC=="no")%>%
  summarise(n())

recibidos%>%
  filter(ventaServicioC=="no")%>%
  summarise(n())

#########################################

rec1 <- correos%>%
  filter(carpeta!="elementos enviados" & is.na(contenido))

#write.xlsx(rec1, "na_contenido.xlsx")


rec1 <- correos%>%
  filter(carpeta!="elementos enviados" & is.na(contenido))

rec2 <- left_join(recibidos, rec1)


#write.csv(recibidos, file = "datos/recibidos_4063.csv", row.names = FALSE)

# concatenar subject y contenido
q1 <- head(recibidos$subject, 10)
q2 <- head(recibidos$contenido, 10)
q3 <- cbind(q1,q2)
q3 <- c(q1,q2)

recibidos4%>%
  group_by(idcorreo)%>%
  summarise(n())

