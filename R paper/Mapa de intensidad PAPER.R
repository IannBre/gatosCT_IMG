

update.packages(ask = FALSE)



install.packages("tidyr")
library(tidyr)
library(pak)
library(cli)

### library --
library(tidyverse)
library(grid)
library(sf)
library(tmap)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(writexl)


# data ----

#detalles de camaras funcionamiento
camtrap2018 <- 
  read.csv("E:/Análisis_R/IMG/data/camtrap_2018.csv",header=T,sep=";")

camtrap2019 <- 
  read.csv("E:/Análisis_R/IMG/data/camtrap_2019.csv", header=T, sep=";")

camtrap2021 <- 
  read_xlsx ("E:/Análisis_R/IMG/data/camtrap_2021.xlsx")

#dataframe con registros y 2min de independencia

metadata2018 <- 
  read.csv("E:/Análisis_R/IMG/data/Metadata2018_2min.csv",
           header=T,sep=";",stringsAsFactors=FALSE)

metadata2019 <- 
  read.csv("E:/Análisis_R/IMG/data/Metadata2019_2min.csv",
           header=T,sep=";",stringsAsFactors=FALSE)

metadata2021 <- 
  read.csv("E:/Análisis_R/IMG/data/Metadata2021_2min.csv",
           header=T,sep=";",stringsAsFactors=FALSE)

# Sumar eventos de grupo gatos y filtrar por gatos eventos

#2018 ###

data2018 <- metadata2018%>% 
  filter(Species=='Gato')%>% 
  group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
  summarise(Species=n())%>%
  rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
  select(Station, utm_x,utm_y,ngatos,detecciones)%>%
  replace_na(list(detecciones=1))%>%
  mutate(gatos=(ngatos*detecciones))%>% #tabla con compresion de eventos por estacion en una sola fila
  select(Station,utm_y,utm_x,gatos)%>%
  summarise(gatos=sum(gatos))

abundancia2018 <- write_xlsx(data2018,"abundanciaB2018.xlsx")
#le agregue el esfuerzo de camtrap2019 mediante excel
intensidad2018 <- read_excel("E:/Análisis_R/IMG/data/otra/abundancia2018.xlsx") 
 
#hago la tasa 2018 
tasagatos2018 <- intensidad2018 %>% 
  mutate(esfuerzo=as.numeric(esfuerzo))%>% 
  mutate(tasa=abundancia/esfuerzo*10) %>% 
  select(Station,utm_y,utm_x,tasa)
 

#### 2019 ##         
data2019 <- metadata2019%>%
  filter(Species=='Gato')%>% 
  group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
  summarise(Species=n())%>%
  rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
  select(Station, utm_x,utm_y,ngatos,detecciones)%>%
  replace_na(list(detecciones=1))%>%
  mutate(gatos=(ngatos*detecciones))%>% #tabla con compresion de eventos por estacion en una sola fila
  select(Station,utm_y,utm_x,gatos)%>%
  group_by(Station,utm_y,utm_x)%>%
  summarise(gatos=sum(gatos))

abundancia2019 <- write_xlsx(data2019,"abundanciaB2019.xlsx")
#le agregue el esfuerzo de camtrap2019 mediante excel
intensidad2019 <- read_excel("E:/Análisis_R/IMG/data/otra/abundancia2019.xlsx")
#hago la tasa 2019 
tasagatos2019 <- intensidad2019 %>% 
  mutate(esfuerzo=as.numeric(esfuerzo))%>%
  rename(abundancia=4) %>% 
  mutate(tasa=abundancia/esfuerzo*10) %>% 
  select(Station,utm_y,utm_x,tasa)

#### 2021 ##         
data2021 <- metadata2021%>%
  filter(Species=='Gato')%>% 
  group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
  summarise(Species=n())%>%
  rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
  select(Station, utm_x,utm_y,ngatos,detecciones)%>%
  replace_na(list(detecciones=1))%>%
  mutate(gatos=(ngatos*detecciones))%>% #tabla con compresion de eventos por estacion en una sola fila
  select(Station,utm_y,utm_x,gatos)%>%
  group_by(Station,utm_y,utm_x)%>%
  summarise(gatos=sum(gatos))

abundancia2021 <- write_xlsx(data2021,"abundanciaB2021.xlsx")
#le agregue el esfuerzo de camtrap2019 mediante excel
intensidad2021 <- read_excel("E:/Análisis_R/IMG/data/otra/abundancia2021.xlsx")
#hago la tasa 2019 
tasagatos2021 <- intensidad2021 %>% 
  mutate(esfuerzo=as.numeric(esfuerzo))%>%
  rename(abundancia=4) %>% 
  mutate(tasa=abundancia/esfuerzo*10) %>% 
  select(Station,utm_y,utm_x,tasa)

#todos

intensidadtodos <- read_excel("E:/Análisis_R/IMG/data/eventosgatosB18-21.xlsx") 

#hago la tasa 2018 
tasagatostodos <- intensidadtodos %>% 
  mutate(esfuerzo=as.numeric(esfuerzo))%>% 
  mutate(tasa=eventos/esfuerzo*10) %>% 
  select(station,utm_y,utm_x,tasa, etapa)


#### 2019 ##         
data2019 <- metadata2019%>%
  filter(Species=='Gato')%>% 
  group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
  summarise(Species=n())%>%
  rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
  select(Station, utm_x,utm_y,ngatos,detecciones)%>%
  replace_na(list(detecciones=1))%>%
  mutate(gatos=(ngatos*detecciones))%>% #tabla con compresion de eventos por estacion en una sola fila
  select(Station,utm_y,utm_x,gatos)%>%
  group_by(Station,utm_y,utm_x)%>%
  summarise(gatos=sum(gatos))




### data mapa -----
metadata2018<- as_tibble(metadata2018)
metadata2019<- as_tibble(metadata2019)
metadata2021<- as_tibble(metadata2021)  

###camaras 2018 y 2019 shape con etiquetas --
camtrap2018 <- as_tibble(camtrap2018)

camaras18 <- camtrap2018%>%
  select(Station, utm_x,utm_y)%>%
  mutate(num=(seq.int(nrow(camtrap2018))))

###camaras 2021 shape con etiquetas --
camtrap2021 <- as_tibble(camtrap2021)

camaras21 <- camtrap2021%>%
  select(Station, utm_x,utm_y)%>%
  mutate(num=(seq.int(nrow(camtrap2021))))%>%
  mutate(num=as.character(num))
#cambio valores de la columna de 2021 que solo tiene camaras
camaras21[1,4] <-"2"
camaras21[2,4] <-"4"
camaras21[3,4] <-"6"
camaras21[4,4] <-"13"
camaras21[5,4] <-"16"
camaras21[6,4] <-"21"
camaras21[7,4] <-"25"
camaras21[8,4] <-"27"

  
####camaras sin registros de gato --
#2018
  cam18_cero <-read.csv("E:/Análisis_R/IMG/data/cam18_cero.csv",header=T,sep=";")
  cam18_cruz<- cam18_cero%>%
    select(Station, utm_x,utm_y)
#2019 
  cam19_cero <-read.csv("E:/Análisis_R/IMG/data/cam19_cero.csv",header=T,sep=";")
  cam19_cruz<- cam19_cero%>%
    select(Station, utm_x,utm_y)
#2021
  cam21_cero <-read.csv("E:/Análisis_R/IMG/data/cam21_cero.csv",header=T,sep=";")
  cam21_cruz<- cam21_cero%>%
    select(station, utm_x,utm_y)
  
# Carga de shapes mapa -----  

#shapes del mapa
  img <- st_read("E:/Análisis_R/IMG/data/mapa/Isla_shape.shp") 
  baja <- st_read("E:/Análisis_R/IMG/data/mapa/Zona_baja.shp")
  media <- st_read("E:/Análisis_R/IMG/data/mapa/Zona_media.shp")
  alta <- st_read("E:/Análisis_R/IMG/data/mapa/Zona_alta.shp")
  arenal <- st_read("E:/Análisis_R/IMG/data/mapa/Arenales.shp")
  pistaimg <-st_read("E:/Análisis_R/IMG/data/mapa/Pista.shp")
  intangible <-st_read ("E:/Análisis_R/IMG/data/mapa/Intangibless.shp")
  sitios <-st_read("E:/Análisis_R/IMG/data/mapa/Sitios y referencias.shp")
  senderos <- st_read("E:/Análisis_R/IMG/data/mapa/calles.shp")
  
##ubicacion de camaras shape
  camaras<- st_read("E:/Análisis_R/IMG/data/mapa/Waypoint_Camaras_trampa-point.shp")
  camaras2 <- st_read("E:/Análisis_R/IMG/data/mapa/Camaras_img.shp")

#cargo datos de posición de las camaras como objeto sf
# Siempre fijarse si esta bien la proyeccion CRS
  
camtrap2018.sf <- st_as_sf(camtrap2018, coords=c("utm_x","utm_y"), crs=32721)
camtrap2019.sf <- st_as_sf(camtrap2019, coords = c("utm_x","utm_y"), crs=32721)
camaras18.sf <- st_as_sf(camaras18,coords=c("utm_x","utm_y"), crs=32721)
camaras21.sf <- st_as_sf(camaras21,coords=c("utm_x","utm_y"), crs=32721)
cam19_cruz.sf <- st_as_sf(cam19_cruz,coords=c("utm_x","utm_y"), crs=32721)
cam18_cruz.sf <- st_as_sf(cam18_cruz,coords=c("utm_x","utm_y"), crs=32721)
cam21_cruz.sf <- st_as_sf(cam21_cruz,coords=c("utm_x","utm_y"), crs=32721)
intangible.sf <- st_as_sf (intangible,coords=c("utm_x","utm_y"), crs=32721)

#eventos a SHAPEFORM con camaras geometria utm
gatos2018.sf <- st_as_sf(data2018, coords=c("utm_x","utm_y"), crs=32721)
gatos2019.sf <- st_as_sf(data2019, coords=c("utm_x", "utm_y"), crs=32721)
gatos2021.sf <- st_as_sf(data2021, coords=c("utm_x", "utm_y"), crs=32721)

#tasa a SHAPEFORM con camaras geometria utm
tasa2018.sf <- st_as_sf(tasagatos2018, coords=c("utm_x","utm_y"), crs=32721)
tasa2019.sf <- st_as_sf(tasagatos2019, coords=c("utm_x", "utm_y"), crs=32721)
tasa2021.sf <- st_as_sf(tasagatos2021, coords=c("utm_x", "utm_y"), crs=32721)
tasagatostodos.sf  <- st_as_sf(tasagatostodos, coords=c("utm_x", "utm_y"), crs=32721)

#Diseño de mapa ----

#### Mapa para agregar eventos en texto ----

 #backbone del mapa

mapaIMG <-
  tm_shape(img,unit = "km")+
    tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
    tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
    tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  tm_shape(sitios)+
    tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  tm_shape(arenal)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.02 )+
  tm_shape(pistaimg)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.03 )+
  tm_shape(intangible)+
    tm_fill(col ="darkgreen",lwd=1,border.alpha=1,alpha = 0.2)+
  tm_shape(senderos)+
    tm_lines(alpha = 0.2)+
  
  #etiquetas de las estaciones
  tm_shape(camaras18.sf)+ 
    tm_text("num",scale=.6, xmod=-1, ymod=0.18, col="black", alpha=0, fontface="bold")+
  
  #posiciones transparentes
  tm_shape(camaras)+
    tm_bubbles(col="darkred", shape=21, size =.18, alpha=1)+

  #escala y norte
  tm_compass(type ="arrow",size=2.5, position=c(0.88, 0.87), show.labels= 1)+
  tm_scale_bar(width = 0.25, position=c(.55,0),lwd = 0.01, text.size = .7)+

  #background del mapa
  tm_layout(frame=T, frame.lwd=1,bg.color='white',inner.margins = c(.05,.02, .04, .05), outer.margins=c(.02,.02, .02, .02), #abajo,izquiera,arriba,derecha
            main.title = "", main.title.size=2,main.title.position=c(x=0.98,y=0.005),
            title="2018 | 2019 | 2021", title.position= c("left","top"),title.size = 1.3,
            legend.show = T, legend.text.size = 1, legend.outside = T,legend.outside.position = c("left","bottom") )

mapaIMG

 ##Gatos 2018 por color y tamaño ----

mapa2018 <-
  tm_shape(img,unit = "km")+
    tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
    tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
    tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  
  tm_shape(sitios)+
    tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.01 )+
  tm_shape (pistaimg)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.01 )+
  tm_shape(intangible)+
    tm_fill(col ="darkgreen",lwd=1,border.alpha=1,alpha = 0.2)+
  tm_shape(senderos)+
    tm_lines(alpha = 0.2)+
  
  #camaras sin registros
  tm_shape(cam18_cruz.sf)+ 
    tm_dots(col="red",shape = 3, size = .08, alpha=0.9, xmod=-0.1,ymod=0,)+ 
  
  #posiciones transparentes
  tm_shape(camaras)+
    tm_dots(col="red", shape = 2,  size = .2, alpha=0)+
  #escala y norte
  tm_compass(type ="arrow",size=2.5, position=c(0.88, 0.87), show.labels= 1)+
  tm_scale_bar(width = 0.25, position=c(.55,0),lwd = 0.01, text.size = .7)+
 
   #eventos en tamaño y color
  tm_shape(tasa2018.sf) + #eventos/esfuerzo 
    tm_bubbles(shape=21,title.size="Detections size",title.col="Detections color",
             size="tasa", col="tasa", style="fixed", breaks=c(.08,.4,.8,1.6,3.2,4.8,8,32),
             border.col="black", border.lwd=1 ,palette=c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c"),
             contrast=5,scale=3.5, alpha=.7)+
  
  #etiquetas de las estaciones
  tm_shape(camaras18.sf)+ 
  tm_text("num",scale=.76, xmod=-0.75, ymod=0.17, col="black", 
          alpha=1, fontface="bold")+
  
   #diseño marco
  tm_layout(frame=T, frame.lwd=1,bg.color='white',inner.margins = c(.05,.02, .04, .05), outer.margins=c(.02,.02, .02, .02), #abajo,izquiera,arriba,derecha
            main.title = "", main.title.size=2,main.title.position=c("center","top"),
            title="2018", title.position= c(x=.09,y=0.95),title.size = 2,
            legend.show = F, legend.text.size = 1, legend.outside = T,legend.outside.position = c("left","bottom") )

mapa2018

setwd("E:/Análisis_R/IMG")

# Exportar el mapa como un archivo PNG
tmap_save(mapa2018, filename = "mapa2018.png",units = "cm", 
          width = 10, height = 20, dpi = 300)

#para tasa
#Breaks para eventos: breaks=c(1,5,10,20,40,60,100,400,700)
#palette=c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026")

#para evento
#breaks=c(.08,.4,.8,1.6,3.2,4.8,8,12),
#palette=c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c")


##Gatos 2019 por color y tamaño ----

mapa2019 <-
  tm_shape(img,unit = "km")+
    tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
    tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
    tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  
  tm_shape(sitios)+
    tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.0 )+
  tm_shape (pistaimg)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.01 )+
  tm_shape(intangible)+
    tm_fill(col ="darkgreen",lwd=1,border.alpha=1,alpha = 0.2)+
  tm_shape(senderos)+
    tm_lines(alpha = 0.2)+

  #camaras sin registros
  tm_shape(cam19_cruz.sf)+ 
    tm_dots(col="red",shape = 3, size = .08, alpha=0.9, xmod=-0.1,ymod=0,)+ 
  
  #posiciones transparentes
  tm_shape(camaras)+
    tm_dots(col="red", shape=2,  size=.2, alpha=0)+
  
  #escala y norte
  tm_compass(type ="arrow",size=2.5, position=c(0.88, 0.87), show.labels= 1)+
  tm_scale_bar(width = 0.25, position=c(.55,0),lwd = 0.01, text.size = .7)+
  
  #eventos en tamaño
  tm_shape(tasa2019.sf) +
    tm_bubbles(shape=21,title.size="Detections size",title.col="Detections color",
             size="tasa", col="tasa", style="fixed", breaks=c(.08,.4,.8,1.6,3.2,4.8,8,32,56),
             border.col="black", border.lwd=1 ,palette=c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
             contrast=5,scale=5, alpha=.7)+
  
  #etiquetas de las estaciones
  tm_shape(camaras18.sf)+ 
  tm_text("num",scale=.76, xmod=-0.75, ymod=0.17, col="black", 
          alpha=1, fontface="bold")+
  
  tm_layout(frame=T, frame.lwd=1,bg.color='white',inner.margins = c(.05,.02, .04, .05), outer.margins=c(.02,.02, .02, .02), #abajo,izquiera,arriba,derecha
            main.title = "", main.title.size=2,main.title.position=c("center","top"),
            title="2019", title.position= c(x=.09,y=0.95),title.size = 2,
            legend.show = F, legend.text.size = 1, legend.outside = T,legend.outside.position = c("left","bottom") )


mapa2019

setwd("E:/Análisis_R/IMG")

# Exportar el mapa como un archivo PNG
tmap_save(mapa2019, filename = "mapa2019.png",units = "cm",  
          width = 10, height = 20, dpi = 300)


##Gatos 2021 por color y tamaño ----

mapa2021 <-
  tm_shape(img,unit = "km")+
    tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
    tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
    tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  
  tm_shape(sitios)+
    tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.01 )+
  tm_shape (pistaimg)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.01 )+
  tm_shape(intangible)+
    tm_fill(col ="darkgreen",lwd=1,border.alpha=1,alpha = 0.2)+
  tm_shape(senderos)+
    tm_lines(alpha = 0.2)+
  
  #camaras sin registros
  tm_shape(cam21_cruz.sf)+ 
    tm_dots(col="red",shape = 3, size = .08, alpha=0.9, xmod=-0.1,ymod=0,)+ 
  
  #posiciones transparentes
  tm_shape(camaras)+
    tm_dots(col="red", shape=2,  size=.2, alpha=0)+
  
  #escala y norte
  tm_compass(type ="arrow",size=2.4, position=c(0.87, 0.86), show.labels= 1)+
  tm_scale_bar(width = 0.25, position=c(.55,0),lwd = 0.01, text.size = .7)+
  
  #eventos en tamaño
  tm_shape(tasa2021.sf) +
    tm_bubbles(shape=21,title.size="Detections size",title.col="Detections color",
             size="tasa", col="tasa", style="fixed", breaks=c(.015,.035,.4,.8,2,6),
             border.col="black", border.lwd=1 ,palette=c("#ffffcc","#ffeda0","#fed976"),
             contrast=5,scale=1, alpha=.7)+
  
  #etiquetas de las estaciones
  tm_shape(camaras21.sf)+ 
  tm_text("num",scale=.72, xmod=-0.48, ymod=0.3, col="black", 
          alpha=1, fontface="bold")+ 
  
  #marco
  tm_layout(frame=T, frame.lwd=1,bg.color='white',inner.margins = c(.05,.02, .04, .05), outer.margins=c(.02,.02, .02, .02), #abajo,izquiera,arriba,derecha
            main.title = "", main.title.size=2,main.title.position=c("center","top"),
            title="2021", title.position= c(x=.09,y=0.95),title.size = 2,
            legend.show = F, legend.text.size = 1, legend.outside = T,legend.outside.position = c("left","bottom") )

mapa2021

setwd("E:/Análisis_R/IMG")

# Exportar el mapa como un archivo PNG
tmap_save(mapa2021, filename = "mapa2021.png",units = "cm", 
          width = 10, height = 20, dpi = 300)


##Gatos TODOS por color y tamaño ----

mapatodos <-
  tm_shape(img,unit = "km")+
    tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
    tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
    tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  
  tm_shape(sitios)+
    tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.01 )+
  tm_shape (pistaimg)+
    tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.01 )+
  tm_shape(intangible)+
    tm_fill(col ="darkgreen",lwd=1,border.alpha=1,alpha = 0.2)+
  tm_shape(senderos)+
    tm_lines(alpha = 0.2)+
  
  #etiquetas de las estaciones
  tm_shape(camaras18.sf)+ 
    tm_text("num",scale=.7, xmod=-1, ymod=0.17, col="black", 
          alpha=1, fontface="bold")+
  
  #camaras sin registros
  tm_shape(cam18_cruz.sf)+ 
    tm_dots(col="red",shape = 3, size = .08, alpha=0.7, xmod=-0.1,ymod=0,)+
  tm_shape(cam19_cruz.sf)+ 
    tm_dots(col="blue",shape = 3, size = .08, alpha=0.7, xmod=-0.1,ymod=0,)+
  tm_shape(cam21_cruz.sf)+ 
    tm_dots(col="yellow",shape = 3, size = .08, alpha=0.7, xmod=-0.1,ymod=0,)+
  
  #posiciones transparentes
  tm_shape(camaras)+
  tm_dots(col="red", shape=2,  size=.2, alpha=0)+
  
  #escala y norte
  tm_compass(type ="arrow",size=2.5, position=c(0.88, 0.87), show.labels= 1)+
  tm_scale_bar(width = 0.25, position=c(.55,0),lwd = 0.01, text.size = .7)+
  
  #eventos en tamaño
  #todos
  tm_shape(tasagatostodos.sf) +
  tm_bubbles(shape=21,title.size="Detections size",title.col="Detections color",
             size="tasa", col="tasa", style="fixed", breaks=c(0,.015,.035,.4,.8,1.6,3.2,4.8,8,32,56),
             border.col="black", border.lwd=1 ,palette=c("#ffffcc","#fffda9","#ffeda0","#ffcda0","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"),
             contrast=5,scale=6, alpha=.65)+
  #marco
  tm_layout(frame=T, frame.lwd=1,bg.color='white',inner.margins = c(.05,.02, .04, .05), outer.margins=c(.02,0, .02, 0), #abajo,izquiera,arriba,derecha
            main.title = "", main.title.size=2,main.title.position=c("center","top"),
            title="2018 - 2019 - 2021", title.position= c("left","top"),title.size = 1.5,
            legend.show = T,legend.text.size = 1, legend.outside = T,legend.outside.position = c("left","bottom") )

mapatodos
setwd("E:/Análisis_R/IMG")

# Exportar el mapa como un archivo PNG
tmap_save(mapatodos, filename = "mapaTODOS.png",units = "cm", 
          width = 10, height = 20, dpi = 300)
