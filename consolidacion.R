library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(readxl)
library(tm)
library(cluster)
library(wordcloud)

baseMaestra <- read_excel(path = "datos/baseMaestra_050821.xlsx",
                          sheet = "baseMaestra")

t1 <- baseMaestra%>%
  group_by(ventaServicioC, derechoPeticionC, tutelaC, tipoPersona, mutacion,
           rectificacionC, adj, id_predioC)%>%
  summarise(n= n())%>%
  arrange(desc(n))%>%
  as.data.frame()

#write.xlsx(t1, file = "clasificacion_050821.xlsx")
 
baseMaestra%>%
  filter(derechoPeticionC=="no" & ventaServicioC=="no" & tutelaC=="no")%>%
  summarise(n())
