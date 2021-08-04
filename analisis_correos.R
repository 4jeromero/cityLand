library(dplyr)
library(stringr)
library(openxlsx)

correos <- read.csv(file = "datos/data-1628021216169.csv", encoding = "UTF-8")

#write.xlsx(correosA, file = "correos.xlsx")
missings <- function(x) return(sum(is.na(x)))
cleanCadena <- function(x){
  x <- str_to_lower(x)   
  x <- str_replace_all(x,"[^a-záéíóúüñ. ]","")
  x <- str_squish(x)
  return(x)
}
cleanCorreo <- function(x){
  x <- str_to_lower(x)   
  x <- str_replace_all(x,"[^a-z1-9@. ]","")
  x <- str_squish(x)
  return(x)
}

correosA = correos
correosA$remitente <- cleanCadena(correosA$remitente)
correosA$subject <- cleanCadena(correosA$subject)
correosA$contenido <- cleanCadena(correosA$contenido)
correosA$adjuntos <- cleanCadena(correosA$adjuntos)
correosA$archivos <- cleanCadena(correosA$archivos)
correosA$carpeta <- cleanCadena(correosA$carpeta)
correosA$correo <- cleanCorreo(correosA$correo)

correosA <- correosA %>% mutate_if(is.character, list(~na_if(.,""))) 
