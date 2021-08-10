library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(readxl)
library(tm)
library(cluster)
library(wordcloud)

recibidos <- read_excel(path = "datos/consolidado_060821.xlsx", sheet = "BD")

# mutaciones
texto <- recibidos%>%
  select(texto)

texto <- removeWords(texto$texto, words = stopwords("spanish"))
texto <- removePunctuation(texto)
texto <- stripWhitespace(texto)
texto <- chartr('áéíóúüñ', 'aeiouuñ', texto) # quitar tildes

clase1 <- c("propietario", "poseedor", "cambio propietario", "traspaso", "vendieron",
            "escrituras", "poseer", "cambio nombre")

a11 <- stringr::str_count(texto, clase1[1]) # cuantas veces se repitio propietario
a12 <- stringr::str_count(texto, clase1[2])
a13 <- stringr::str_count(texto, clase1[3])
a14 <- stringr::str_count(texto, clase1[4])
a15 <- stringr::str_count(texto, clase1[5])
a16 <- stringr::str_count(texto, clase1[6])
a17 <- stringr::str_count(texto, clase1[7])
a18 <- stringr::str_count(texto, clase1[8]) # cuantas veces se repitio cambio nombre
c1 <- a11+a12+a13+a14+a15+a16+a17+a18

recibidos <- recibidos%>%
  mutate(a11,a12,a13,a14,a15,a16,a17,a18,c1)

# clase 2
clase2 <- c("englobe", "desenglobe", "agrege", "segregacion", "agregacion",
            "constructora", "division", "dividi", "separar", "separe")
a21 <- stringr::str_count(texto, clase2[1])
a22 <- stringr::str_count(texto, clase2[2])
a23 <- stringr::str_count(texto, clase2[3])
a24 <- stringr::str_count(texto, clase2[4])
a25 <- stringr::str_count(texto, clase2[5])
a26 <- stringr::str_count(texto, clase2[6])
a27 <- stringr::str_count(texto, clase2[7])
a28 <- stringr::str_count(texto, clase2[8])
a29 <- stringr::str_count(texto, clase2[9])
a2_10 <- stringr::str_count(texto, clase2[10])
c2 <- a21+a22+a23+a24+a25+a26+a27+a28+a29+a2_10

recibidos <- recibidos%>%
  mutate(a21,a22,a23,a24,a25,a26,a27,a28,a29,a2_10,c2)

clase3 <- c("contruccion", "edificar", "demoli", "area", "edificacion", "construir")
a31 <- stringr::str_count(texto, clase3[1])
a32 <- stringr::str_count(texto, clase3[2])
a33 <- stringr::str_count(texto, clase3[3])
a34 <- stringr::str_count(texto, clase3[4])
a35 <- stringr::str_count(texto, clase3[5])
a36 <- stringr::str_count(texto, clase3[6])
c3 <- a31+a32+a33+a34+a35+a36

recibidos <- recibidos%>%
  mutate(a31,a32,a33,a34,a35,a36,c3)

clase4 <- c("avaluo", "valor", "cambio avaluo", "revision avaluo", "precio")
a41 <- stringr::str_count(texto, clase4[1])
a42 <- stringr::str_count(texto, clase4[2])
a43 <- stringr::str_count(texto, clase4[3])
a44 <- stringr::str_count(texto, clase4[4])
a45 <- stringr::str_count(texto, clase4[5])
c4 <- a41+a42+a43+a44+a45

recibidos <- recibidos%>%
  mutate(a41,a42,a43,a44,a45,c4)

clase5 <- c("mejora", "mejoras", "inscribir", "inscripcion")
a51 <- stringr::str_count(texto, clase5[1])
a52 <- stringr::str_count(texto, clase5[2])
a53 <- stringr::str_count(texto, clase5[3])
a54 <- stringr::str_count(texto, clase5[4])
c5 <- a51+a52+a53+a54

recibidos <- recibidos%>%
  mutate(a51,a52,a53,a54,c5)

# variable dicotomica
rectificacion <- c("uso", "utilizacion", "utilizar", "generacion", "ficha", "genere",
                   "rectificacion", "correccion")
a61 <- stringr::str_count(texto, rectificacion[1])
a62 <- stringr::str_count(texto, rectificacion[2])
a63 <- stringr::str_count(texto, rectificacion[3])
a64 <- stringr::str_count(texto, rectificacion[4])
a65 <- stringr::str_count(texto, rectificacion[5])
a66 <- stringr::str_count(texto, rectificacion[6])
a67 <- stringr::str_count(texto, rectificacion[7])
a68 <- stringr::str_count(texto, rectificacion[8])
rectificacion <- a61+a62+a63+a64+a65+a66+a67+a68

recibidos <- recibidos%>%
  mutate(a61,a62,a63,a64,a65,a66,a67,a68, rectificacion)

# variable dicotomica
ventaServicio <- c("certificado", "venta", "comprar", "genere", "ficha", "expedicion")
a71 <- stringr::str_count(texto, ventaServicio[1])
a72 <- stringr::str_count(texto, ventaServicio[2])
a73 <- stringr::str_count(texto, ventaServicio[3])
a74 <- stringr::str_count(texto, ventaServicio[4])
a75 <- stringr::str_count(texto, ventaServicio[5])
a76 <- stringr::str_count(texto, ventaServicio[6])
ventaServicio <- a71+a72+a73+a74+a75+a76

recibidos <- recibidos%>%
  mutate(a71,a72,a73,a74,a75,a76, ventaServicio)


# variable dicotomica
tutela <- c("tutelas", "abogado", "tutela", "juzgado", "contra", "escrito tutela",
            "juez", "rama judicial", "interponer")
a81 <- stringr::str_count(texto, tutela[1])
a82 <- stringr::str_count(texto, tutela[2])
a83 <- stringr::str_count(texto, tutela[3])
a84 <- stringr::str_count(texto, tutela[4])
a85 <- stringr::str_count(texto, tutela[5])
a86 <- stringr::str_count(texto, tutela[6])
a87 <- stringr::str_count(texto, tutela[7])
a88 <- stringr::str_count(texto, tutela[8])
a89 <- stringr::str_count(texto, tutela[9])
tutela <- a81+a82+a83+a84+a85+a86+a87+a88+a89

recibidos <- recibidos%>%
  mutate(a81,a82,a83,a84,a85,a86,a87,a88, tutela)


derechoPeticion <- c("derecho", "peticion", "pqr")
a91 <- stringr::str_count(texto, derechoPeticion[1])
a92 <- stringr::str_count(texto, derechoPeticion[2])
a93 <- stringr::str_count(texto, derechoPeticion[3])
derechoPeticion <- a91+a92+a93

recibidos <- recibidos%>%
  mutate(a91,a92,a93, derechoPeticion)


id_predio <- c("folio", "matr", "matricula")
a101 <- stringr::str_count(texto, id_predio[1])
a102 <- stringr::str_count(texto, id_predio[2])
a103 <- stringr::str_count(texto, id_predio[3])
id_predio <- a101+a102+a103

recibidos <- recibidos%>%
  mutate(a101,a102,a103, id_predio)


write.xlsx(recibidos, file = "recibidos_060821.xlsx")











  