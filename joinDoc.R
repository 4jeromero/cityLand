library(dplyr)
library(stringr)
library(openxlsx)
library(tidyverse)
library(tidytext)
library(readxl)
library(tm)
library(cluster)
library(wordcloud)

general <- read_excel(path = "recibidos_060821.xlsx")
predios <- read_excel(path = "datos/Identificacion_Predios.xlsx", sheet = "Sheet1")

predios <- predios%>%
  select(idcorreo, cedula, num_cedula, Tener_cédula, matricula, num_matricula,
         `Tener Matrícula`, predio, num_predio, `Tener Num Predio`, nombre,
         `Tiene Nombre`, direccion, `Tiene dirección`, predio_cedula,
         predio_matricula, `predio_numero _predio`, haynombre_cuepo, 
         haynombre_remitente)

consolidado_con <- full_join(general, predios, by="idcorreo")
write.xlsx(consolidado_con, "consolidado_con_060821_V1.xlsx")


Nopredios <- read_excel(path = "datos/Identificacion_Predios.xlsx",
                        sheet = "Predios sin identificación")


Nopredios <- Nopredios%>%
  select(idcorreo, cedula, num_cedula, Tener_cédula, matricula, num_matricula,
         `Tener Matrícula`, predio, num_predio, `Tener Num Predio`, nombre,
         `Tiene Nombre`, direccion, `Tiene dirección`)

consolidado_sin <- left_join(Nopredios, general, by="idcorreo")
write.xlsx(consolidado_sin, "consolidado_sin_060821_V1.xlsx")





