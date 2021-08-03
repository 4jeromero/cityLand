library(readxl)
library(dplyr)
library(ggplot2)

predio <- read_excel(path = "datos/R1_Mpio_47001_Vigencia_2021_R1Generado_20210104_V2 (1).xlsx",
                     sheet = "Predio")
summary(predio)

propietario <- read_excel(path = "datos/R1_Mpio_47001_Vigencia_2021_R1Generado_20210104_V2 (1).xlsx",
                          sheet = "General")
t0 <- predio%>%
  group_by(Zona, Condicion_del_predio)%>%
  filter(AREA_CONSTRUIDA<145)


t1 <- predio%>%
  group_by(Zona, Condicion_del_predio)%>%
  summarise(conteo = n())


k

ggplot(data = predio)+
  geom_bar(aes(x = Zona, fill=Condicion_del_predio),position=position_dodge())+
  ylab('Conteo')+
  ggtitle("Cantidad de Predios por zona y condicion")

ggplot(data = predio)+
  geom_bar(aes(x = Condicion_del_predio, fill=Zona),position=position_dodge())+
  ylab('Conteo')+
  ggtitle("Cantidad de Predios por zona y condicion")

t2 <- predio%>%
  group_by(Zona, Condicion_del_predio)%>%
  summarise(mean(AREA_TERRENO), min(AREA_TERRENO), max(AREA_TERRENO))

ggplot(data = t0)+
  geom_density(aes(x = AREA_CONSTRUIDA, fill=Zona, alpha=0.5))+
  ggtitle(label = 'Area Construida diferenciada por zona')+
  theme(axis.title.y = element_blank())+
  xlab("Area (M2)")

predio%>%
  filter(AREA_TERRENO<417)%>%
ggplot()+
  geom_density(aes(x = AREA_TERRENO, fill=Zona, alpha=0.5))+
  ggtitle(label = 'Area TERRENO diferenciada por zona')+
  theme(axis.title.y = element_blank())+
  xlab("Area (M2)")


predio%>%
  filter(AVALUO<=45400000)%>%
  ggplot()+
  geom_density(aes(x = AVALUO, fill=Zona, alpha=0.5))+
  ggtitle(label = 'Avaluo diferenciada por zona')+
  theme(axis.title.y = element_blank())+
  xlab("Area (M2)")


summary(predio)

predio%>%
  group_by(Zona, Condicion_del_predio)%>%
  summarise(mean(AREA_CONSTRUIDA), min(AREA_CONSTRUIDA), max(AREA_CONSTRUIDA))

predio%>%
  group_by(Zona, Condicion_del_predio)%>%
  summarise(mean(AVALUO), min(AVALUO), max(AVALUO))

propietario%>%
  group_by(Zona, Condicion_del_predio)%>%
  summarise(n())

ggplot(data = propietario)+
  geom_bar(aes(x = Zona, fill=Condicion_del_predio),position=position_dodge())+
  ylab('Conteo')+
  ggtitle("Cantidad de Registros por zona y condicion")

ggplot(data = predio)+
  geom_bar(aes(x = Condicion_del_predio, fill=Zona),position=position_dodge())+
  ylab('Conteo')+
  ggtitle("Cantidad de Registros por zona y condicion")

predio%>%
  filter(AREA_CONSTRUIDA<145)%>%
  ggplot()+
  geom_boxplot(aes(fill = Zona, y = AREA_CONSTRUIDA))+
  facet_grid(.~Condicion_del_predio)+
  ylab(label = "Area Construida (M2)")+
  ggtitle("Area Construida segun la condicion del predio")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

predio%>%
  filter(AREA_TERRENO<417)%>%
  ggplot()+
  geom_boxplot(aes(fill = Zona, y = AREA_TERRENO))+
  facet_grid(.~Condicion_del_predio)+
  ylab(label = "Area del Terreno (M2)")+
  ggtitle("Area del terreno segun la condicion del predio")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


predio%>%
  filter(AVALUO<=45400000)%>%
  ggplot()+
  geom_boxplot(aes(fill = Zona, y = AVALUO))+
  facet_grid(.~Condicion_del_predio)+
  ylab(label = "Avaluo (M2)")+
  ggtitle("avaluo del predio segun la condicion del predio")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())


predio%>%
  group_by(Zona)%>%
  summarise(d_cons = quantile(AREA_CONSTRUIDA, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)),
            d_terr = quantile(AREA_TERRENO, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)),
            d_aval = quantile(AVALUO, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))%>%
  as.data.frame()












