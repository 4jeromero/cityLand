library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(readxl)
library(tm)
library(cluster)
library(wordcloud)

baseMaestra <- read_excel(path = "datos/consolidado_060821.xlsx", 
                          sheet = "BDMaestra")

# concatenar subject y cuerpo de correo 
# aplicado el filtro se tiene 2873 obs
t1 <- baseMaestra%>%
  group_by(remitente, tutelaC, derechoPeticionC, ventaServicioC)%>%
  mutate(duplicado = n())%>%
  arrange(desc(duplicado))%>%
  as.data.frame()

write.xlsx(t1, file = "duplicados_060821_of.xlsx")


# me devuelve valores unicos
t2 <- baseMaestra%>%
  group_by(remitente, tutelaC, derechoPeticionC, ventaServicioC)%>%
  summarise(n= n())%>%
  arrange(desc(n))%>%
  as.data.frame()

write.xlsx(t2, file = "duplicados_060821.xlsx")

# es decir que si no hubiesen duplicados se tendria 1164 en t1, esto implica que 
# hay 1709 dupliciddes para este filtro





x <- "hola"
rep(x,3)

