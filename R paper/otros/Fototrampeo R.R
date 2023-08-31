########librarys###############
library(tidyverse)
library(lubridate)
library(grid)
library(sf)
library(tmap)
library(camtrapR)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library (shinyjs)
library(scales)

####Para scales, paleta de colores tmap#######
library (shinyjs)
tmaptools::palette_explorer()

###Para scales, comprimir detalle de grafico#####
library(scales)

magnify_trans <- function(interval_low = 0, interval_high = 7,  reducer = 20) {
  
  trans <- function(x, i_low = interval_low, i_high = interval_high, r = reducer) 
    sapply(x, function(x) {
      if(x >= i_low & x <= i_high ) x
      else if(x < i_low) x / r + i_low
      else (x - i_high) / r + i_high
    })
  
  inv <- function(x, i_low = interval_low, i_high = interval_high, r = reducer) 
    sapply(x, function(x) {
      if(x >= i_low & x <= i_high ) x
      else if(x < i_low) (x - i_low) * r
      else ((x - i_high) * r) + i_high
    })
  
  trans_new(name = 'custom',  transform = trans,inverse = inv )
}


############Carga de datos########################

#CARGAR CSV CON DATOS
PRE18camtrap <- read.csv("camtrap_2018.csv",header=T,sep=";")
POST19camtrap <- read.csv("camtrap_2019.csv", header=T, sep=";")

#CREAR DIRECTORIO CON FOTOS ORGANIZADAS
PRE18CAM <- file.path("CamarasIMG18")
CARPETAPRE18 <- createStationFolders(inDir = PRE18CAM, stations = as.character(PRE18camtrap$Station), createinDir = T)
POST19CAM <- file.path("CamarasIMG19")
CARPETAPOST19 <- createStationFolders(inDir = POST19CAM, stations = as.character(POST19camtrap$Station), createinDir = T)  

#DIRECTORIO
Fototrampeo <- file.path("Fototrampeo")
Shapes <- file.path ("Shapes")

#PONER FOTOS EN CARPETAS
#Info en metadata
exifTagNames(PRE18CAM, returnMetadata = TRUE)
exifTagNames(POST19CAM, returnMetadata = TRUE)

#taggear fotos con Digikam: HECHO
#construir tabla de registros independientes
#usar minDeltaTime=x


#### Datos para mapas 2018 y 2019####
###DatosPRE####
#codigo para 2 min
metadataPRE<-recordTable(inDir = PRE18CAM,
                      cameraID = "directory",
                      camerasIndependent = F,
                      IDfrom = "metadata",
                      minDeltaTime = 2,
                      writecsv = T,
                      outDir = Fototrampeo,
                      deltaTimeComparedTo="lastRecord",
                      metadataSpeciesTag = "SP",
                      metadataHierarchyDelimitor ="|" ,
                      timeZone ="America/Argentina/Buenos_Aires" )


#cargar metadata 2 min que tiene coordenadas utm agregadas
metadataPRE <- read.csv("Fototrampeo/Metadata2018_2min.csv",header=T,sep=";",stringsAsFactors=FALSE)


##Summary 2018

report2018 <- surveyReport (recordTable          = metadataPRE,
                            CTtable              = PRE18camtrap,
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "%d/%m/%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                            sinkpath             ="Fototrampeo/reporte18",
                            makezip              = TRUE,
                            CTHasProblems        = TRUE
                             )

## Report 2018

str(report2018)
report2018[[1]]
report2018[[2]]
report2018[[3]]
report2018[[4]]
report2018[[5]]

####DatosPOST####
metadataPOST<-recordTable(inDir = POST19CAM,
                          cameraID = "directory",
                          camerasIndependent = F,
                          IDfrom = "metadata",
                          minDeltaTime = 2,
                          writecsv = T,
                          outDir = Fototrampeo,
                          deltaTimeComparedTo="lastRecord",
                          metadataSpeciesTag = "SP",
                          metadataHierarchyDelimitor ="|" ,
                          timeZone ="America/Argentina/Buenos_Aires" )

#cargar metadata 2 min que tiene coordenadas utm agregadas
metadataPOST <- read.csv("Fototrampeo/Metadata2019_2min.csv",header=T,sep=";",stringsAsFactors=FALSE)

report2019 <- surveyReport (recordTable          = metadata2019,
                            CTtable              = POST19camtrap,
                            speciesCol           = "Species",
                            stationCol           = "Station",
                            setupCol             = "Setup_date",
                            retrievalCol         = "Retrieval_date",
                            CTDateFormat         = "%d/%m/%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                            sinkpath             ="Fototrampeo/reporte19",
                            makezip              = TRUE,
                            CTHasProblems        = TRUE
)

## Report 2019

str(report2019)
report2019[[1]]
report2019[[2]]
report2019[[3]]
report2019[[4]]
report2019[[5]]

####CODIGOS mapas####
######PRE#############


metadataPRE<- as_tibble(metadataPRE)
 
#tabla con compresion de eventos por estacion en una sola fila

dataPRE <- metadataPRE%>% 
    filter(Species=='Gato')%>% 
      group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
        summarise(Species=n())%>%
          rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
             select(Station, utm_x,utm_y,ngatos,detecciones)%>%
                replace_na(list(detecciones=1))%>%
                  mutate(eventos=(ngatos*detecciones))%>%
                     select(Station,utm_y,utm_x,eventos)%>%
                        summarise(eventos=sum(eventos))

guardardatos2018 <- write_excel_csv(dataPRE,"eventos 2018")


  
summary(dataPRE)


  
######POST#########          
metadataPOST<- as_tibble(metadataPOST)  

dataPOST <- metadataPOST%>%
  filter(Species=='Gato')%>% 
    group_by(Station,utm_y,utm_x,metadata_Grupo)%>%
      summarise(Species=n())%>%
        rename("ngatos" = "Species", "detecciones"= "metadata_Grupo")%>%
          select(Station, utm_x,utm_y,ngatos,detecciones)%>%
            replace_na(list(detecciones=1))%>%
              mutate(eventos=(ngatos*detecciones))%>%
                select(Station,utm_y,utm_x,eventos)%>%
                  group_by(Station,utm_y,utm_x)%>%
                    summarise(eventos=sum(eventos))


guardardatos2019 <- write_excel_csv(dataPOST,"eventos 2019")
                  
###camaras shape con etiquetas 

PRE18camtrap <- as_tibble(PRE18camtrap)

camaras18 <- PRE18camtrap%>%
  select(Station, utm_x,utm_y)
    
####camaras sin registrod de gato
cam18_cero <-read.csv("cam18_cero.csv",header=T,sep=";")
cam18_cruz<- cam18_cero%>%
  select(Station, utm_x,utm_y)

cam19_cero <-read.csv("cam19_cero.csv",header=T,sep=";")
cam19_cruz<- cam19_cero%>%
  select(Station, utm_x,utm_y)

       
#####DISEÑO Mapa############          
#capas para mapa

#leo los shapes

img <- st_read("Shapes/Isla_shape.shp") 
baja <- st_read("Shapes/Zona_baja.shp")
media <- st_read("Shapes/Zona_media.shp")
alta <- st_read("Shapes/Zona_alta.shp")
arenal <- st_read("Sapes2/Arenales.shp")
pistaimg <-st_read("Sapes2/Pista.shp")
intangible <-st_read ("Sapes2/Intangibless.shp")
sitios <-st_read("Sapes2/Sitios y referencias.shp")
#cargar senderos
calles <- st_zm(st_read("Sapes2/Callesssssss.shp"),drop = TRUE, what = "ZM")
##ubicacion de camaras shape
camaras<- st_read("Shapes/Waypoint_Camaras_trampa-point.shp")
camaras2 <- st_read("Shapes/Camaras_img.shp")



#reproyecto capas no se usa

img18 <- st_transform(img,32719)
z_baja <- st_transform(baja,32719)
z_media <- st_transform(media,32719)
z_alta <- st_transform(alta,32719)
arenales <- st_transform(arenal,32719)
z_intangible <- st_transform(intangible, 32719)
sitiosimg <-st_transform(sitios, 32719)

#transformar senderos
calles <- st_transform(callesssssss,32719)



#cargo datos de posición de las camaras como objeto sf

PRE18camtrap.sf <- st_as_sf(PRE18camtrap, coords=c("utm_x","utm_y"), crs=32721)
POST19camtrap.sf <- st_as_sf(POST19camtrap, coords = c("utm_x","utm_y"), crs=32721)

camaras18.sf <- st_as_sf(camaras18,coords=c("utm_x","utm_y"), crs=32721)
cam19_cruz.sf <- st_as_sf(cam19_cruz,coords=c("utm_x","utm_y"), crs=32721)
cam18_cruz.sf <- st_as_sf(cam18_cruz,coords=c("utm_x","utm_y"), crs=32721)

#agragar columnas con coordenadas al metadata 
#cargo datos de gatos en las camaras

gatosPRE.sf <- st_as_sf(dataPRE, coords=c("utm_x","utm_y"), crs=32721)
gatosPOST.sf <- st_as_sf(dataPOST, coords=c("utm_x", "utm_y"), crs=32721)




##########INTENSIDAD 2018 Y 2019################

#Mapas shape
mapaPREPOST <-tm_shape(img,unit = "km")+
          tm_polygons(col='ivory1',lwd=2)+
        tm_shape(baja)+ 
              tm_polygons(col='pink',alpha = 0.2,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
      tm_polygons(col='lightgreen',alpha = 0.2,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
    tm_polygons(col='yellow',alpha = 0.2,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  tm_shape(calles)+
    tm_lines(col='black',lwd=0.5,alpha = 0.4)+
  tm_shape(sitios)+
  tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
#Figuras del mapa
  
  tm_shape(arenal)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.1 )+
  tm_shape (pistaimg)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.2 )+
  tm_shape(intangible)+
  tm_polygons(col="white",lwd=1, border.alpha = 1, alpha = 0.1 )+
  
  tm_shape(camaras18.sf)+ 
    tm_dots(col="red",shape = 20, size = .5, alpha=.9, xmod=-.02,ymod=.065)+ 
      tm_text("Station",scale=.65, xmod=-.55, ymod=.18, col = "black", alpha=1)+
  

  
  tm_shape(camaras)+
  tm_dots(col="red", shape = 21,  size = .2, alpha=0)+
  
  tm_compass(type ="8star",position=c(0, 0.825), show.labels = 3,
             text.size = 0.4, cardinal.directions = c("N", "E", "S", "O"))+
  tm_scale_bar(width = 0.2,position=c(0.62, 0),lwd = 0.01)+
  
  #Gatos 2018
  tm_shape(gatosPRE.sf)+
tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  tm_bubbles(shape=21,title.size=2,title.col="Detecciones",
             size=.6, style="fixed", breaks=c(-Inf,10,40,100,300,Inf),
             border.col=1, border.lwd=1, col = "eventos",palette=c("whitesmoke","lightgrey","rosybrown","dimgrey","midnightblue"),
             contrast=5,scale=1.7, alpha=1, )+
  tm_layout(frame=T,bg.color='ivory1',inner.margins = 0,title="", 
            title.position= c("left","top"),title.size = 3,legend.show = T, legend.text.size =.7, 
            legend.outside = T, legend.outside.position = "right",main.title = "Intensidad de presencia de F. catus" )+

#Gatos 2019
tm_shape(gatosPOST.sf)+
tm_text("Station",scale=.65, xmod=-.7, ymod=.3)+
  tm_bubbles(size=.6,shape=21,scale=1.7,alpha=1,contrast=5, style="fixed", breaks=c(-Inf,10,40,100,300,Inf),
             title.size=2, title.col="Detecciones",
             border.col=1,border.lwd=1, col= "eventos", palette=c("whitesmoke","lightgrey","rosybrown","dimgrey","midnightblue"))+
  
  tm_layout(frame=T,bg.color='ivory1',inner.margins = 0, outer.margins = 0, title="18/19",
            title.position= c("left","top"),legend.show = T,legend.text.size = .7, 
            title.size = 3, legend.outside = T, legend.outside.position = "right",  
            main.title= "Intensidad de presencia F.catus")

  
  
mapaPREPOST

dev.off()

######################## MAPA PRE ############

library(tmap)

#Mapas shape
mapaPRE <-tm_shape(img,unit = "km")+
  tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
  tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
  tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
  tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  tm_shape(calles)+
  tm_lines(col='black',lwd=0.57,alpha = 0.2)+
  tm_shape(sitios)+
  tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.02 )+
  tm_shape (pistaimg)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.05 )+
  tm_shape(intangible)+
  tm_polygons(col="white",lwd=1, border.alpha = 1, alpha = 0.02,  )+
  
  tm_shape(camaras18.sf)+ 
      tm_text("Station",scale=.65, xmod=-0.55, ymod=0.17, col = "black", alpha=1, fontface = "bold")+
  
  tm_shape(cam18_cruz.sf)+ 
    tm_dots(col="red",shape = 20, size = .08, alpha=0.7, xmod=-0.1,ymod=0,)+ 
  
  
  tm_shape(camaras)+
  tm_dots(col="red", shape = 2,  size = .2, alpha=0)+
  
  tm_compass(type ="8star",position=c(0.15, 0.84), show.labels = 3,
             text.size = 0.45, cardinal.directions = c("N", "E", "S", "O"))+
  tm_scale_bar(width = 0.45,position=c(0.3, 0),lwd = 0.01)+


tm_shape(gatosPRE.sf)+
  
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  
  tm_bubbles(shape=21,title.size=2,title.col="",
             col = "red",style="fixed",breaks=c(1,5,10,20,40,60,100,400,700),
             palette=c("ivory2","yellow","gold","goldenrod","orange","red","darkred","black"), 
             labels=c("1-5","5-10","10-20","20-40","40-60","60-100","100-400","400-700"),
             legend.col.show=T,legend.col.is.portrait=F,legend.hist=F,legend.col.z=0.5,
             
             size="eventos",border.col=1, border.lwd=1,alpha=0.7,scale=9, 
             size.max=659, size.lim = c(0,350),sizes.legend=(breaks=c(5,10,20,40,60,100,400,700)),
             sizes.legend.labels=(breaks=c("1-5","5-10","10-20","20-40","40-60","60-100","100-400","400-700")),
             legend.size.show=T, legend.size.is.portrait=T,legend.size.z=0.6,
             
             )+

  
  tm_layout(frame=F,bg.color='white',outer.margins=.03, inner.margins=.08, between.margin=.03, title="Detecciones",title.position= c("left","top"),title.size =0,
            legend.show = F,main.title.size = 1,legend.text.size =0,legend.width = 2,  
            legend.outside = T, legend.outside.position = "right", 
            main.title = "Preintervenci?n 2018",legend.title.size=1,)

   
            
        

mapaPRE


#legend.hist.size = 1,  legend.height = 0.9, 
#style="fixed", breaks=c(1,5,10,20,40,60,100,400,700)
#palette=c("ivory2","yellow","gold","goldenrod","orange","red","darkred","black")
#legend("topright",legend=c("a","b","c","d","e")), cex = 0.75, ncol=1, horiz = F,


############### POR COLOR ###########


#Gatos 2018 por color

tm_shape(gatosPRE.sf)
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  tm_bubbles(shape=21,title.size=2,title.col="Detecciones",
             size=.6, style="fixed", breaks=c(1,5,10,20,40,60,100,400,700),
             border.col=1, border.lwd=1, col = "eventos",palette=c("ivory2","yellow","gold","goldenrod","orange","red","darkred","black"),
             contrast=5,scale=1.7, alpha=1, )+
  tm_layout(frame=F,bg.color='white',inner.margins = 0,title="2018", 
            title.position= c("left","top"),title.size = 3,legend.show = T, legend.text.size =.7, 
            legend.outside = T, legend.outside.position = "left",main.title = "" )

mapaPRE

############ POR TAMA?O ##########

#Gatos 2018 por tama?o

tm_shape(gatosPRE.sf)+
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  tm_bubbles(shape=21,title.size=2,title.col="Detecciones",
             size="eventos", style="fixed", breaks=c(1,5,10,20,40,60,100,400,700),
             border.col=1, border.lwd=1, 
             contrast=5,scale=1.7, alpha=1, )+
  tm_layout(frame=F,bg.color='white',inner.margins = 0,title="2018", 
            title.position= c("left","top"),title.size = 3,legend.show = T, legend.text.size =.7, 
            legend.outside = T, legend.outside.position = "left",main.title = "" )


mapaPRE


################# MAPA POST############

#Mapas shape
mapaPOST <-tm_shape(img,unit = "km")+
  tm_polygons(col='ivory1',lwd=2.2)+
  tm_shape(baja)+ 
  tm_polygons(col='cornflowerblue',alpha = 0.5,lwd=1,lty=2,border.col = "black",border.alpha = 1)+ 
  tm_shape(media)+
  tm_polygons(col='green',alpha = 0.5,lwd=1, lty=2, border.col="black",border.alpha = 1)+ 
  tm_shape(alta)+ 
  tm_polygons(col='violet',alpha = 0.5,lwd=1, lty=2, border.col = "black",border.alpha = 1)+
  tm_shape(calles)+
  tm_lines(col='black',lwd=0.57,alpha = 0.2)+
  tm_shape(sitios)+
  tm_dots(col="black", border.alpha = 1, alpha = 0 )+
  
  #Figuras del mapa
  
  tm_shape(arenal)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha = 0.02 )+
  tm_shape (pistaimg)+
  tm_polygons(col="lightblue",lwd=1, border.alpha = 1, alpha=0.05 )+
  tm_shape(intangible)+
  tm_polygons(col="white",lwd=1, border.alpha = 1, alpha = 0.02,  )+
  
  tm_shape(camaras18.sf)+ 
  tm_text("Station",scale=.65, xmod=-0.55, ymod=0.17, col = "black", alpha=1, fontface = "bold")+
  
  tm_shape(cam19_cruz.sf)+ 
  tm_dots(col="red",shape = 20, size = .08, alpha=0.7, xmod=-0.1,ymod=0,)+ 
  
  
  tm_shape(camaras)+
  tm_dots(col="red", shape = 2,  size = .5, alpha=0)+
  
  tm_compass(type ="8star",position=c(0.15, 0.84), show.labels = 3,
             text.size = 0.45, cardinal.directions = c("N", "E", "S", "O"))+
  tm_scale_bar(width = 0.45,position=c(0.3, 0),lwd = 0.01)+
  
#Gatos 2019
  
tm_shape(gatosPOST.sf)+
  
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  
  tm_bubbles(shape=21,title.size=2,title.col="",
             col = "orange",palette="-RdYlBu", contrast=1,
             labels=c("1-5","5-10","10-20","20-40","40-60","60-100","100-400","400-700"),
             legend.col.show=T,legend.col.is.portrait=F,legend.hist=F,legend.col.z=0.5,
             
             size="eventos",border.col=1, border.lwd=1,alpha=0.65,scale=6, 
             size.max=700, size.lim = c(0,350),sizes.legend=(breaks=c(5,10,20,40,60,100,400,700)),
             sizes.legend.labels=(breaks=c("1-5","5-10","10-20","20-40","40-60","60-100","100-400","400-700")),
             legend.size.show=T, legend.size.is.portrait=T,legend.size.z=0.6,
             
  )+

 

  tm_layout(frame=F,bg.color='white',outer.margins=.03, inner.margins=.08, between.margin=.03, title="Detecciones",title.position= c("left","top"),title.size =0,
            legend.show = F,main.title.size =1,legend.text.size =2,legend.width = 2,  
            legend.outside = T, legend.outside.position = "right", 
            main.title = "Postintervenci?n 2019",legend.title.size=1,)
            
mapaPOST


#sizes.legend=(breaks=c(1,5,10,20,40,60,100,400,700)),
#sizes.legend.labels=(breaks=c("0-1","1-5","5-10","10-20","20-40","40-60","60-100","100-400","400-700"))

############### POR COLOR ###########


#Gatos 2019 por color

tm_shape(gatosPOST.sf)+
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha = 0)+
  tm_bubbles(size=.6,shape=21,scale=1.7,alpha=1,contrast=5, style="fixed", breaks=c(1,5,10,20,40,60,100,400,700),
             title.size=2, title.col="Detecciones",
             border.col=1,border.lwd=1, col= "eventos", palette=c("ivory2","yellow","gold","goldenrod","orange","red","darkred","black"))+
  
  tm_layout(frame=F,bg.color='white',inner.margins = 0, outer.margins = 0, title="2019",
            title.position= c("left","top"),legend.show = T,legend.text.size = .7, 
            title.size = 3, legend.outside = T, legend.outside.position = "right",  
            main.title= "")

mapaPOST


############ POR TAMA?O ##########

#Gatos 2019 por tama?o

tm_shape(gatosPOST.sf)+
  tm_text("Station",scale=.65, xmod=-.7, ymod=.3, alpha=0)+  
  tm_bubbles(shape=21,title.size=2,title.col="Detecciones",
             size="eventos", style="fixed", breaks=c(1,5,10,20,40,60,100,400,700),
             border.col=1, border.lwd=1, 
             contrast=5,scale=1.7, alpha=1, )+
  tm_layout(frame=F,bg.color='white',inner.margins = 0,title="2018", 
            title.position= c("left","top"),title.size = 3,legend.show = T, legend.text.size =.7, 
            legend.outside = T, legend.outside.position = "left",main.title = "" )


##############TODAS LAS ESPECIES REGISTRADAS###############
#####TODAS LAS ESPECIES2018##########
especiesPRE <- metadataPRE%>% 
  select(Station, utm_x,utm_y,Species,metadata_Grupo)%>%
  rename("Especies" = "Species", "detecciones"= "metadata_Grupo")%>%
  replace_na(list(detecciones=1))%>%
  mutate(Species2=Especies)%>%
  group_by(Station,utm_y,utm_x,detecciones,Especies)%>%
  summarise(Species2=n())%>%
  rename("nEspecies"="Species2" )%>%
  mutate(eventos=(nEspecies*detecciones))%>%
  select(Station, utm_x,utm_y,Especies,eventos)%>%
  group_by(Station,utm_y,utm_x,Especies)%>%
  summarise(eventos=sum(eventos))%>%
  filter(Especies=='Gato'|Especies=='Persona'|Especies=='Carpincho'
         |Especies=='Colilargo'|Especies=='Perro'|Especies=='Coipo' )

#####TODAS LAS ESPECIES2019##########
especiesPOST <- metadataPOST%>% 
  select(Station, utm_x,utm_y,Species,metadata_Grupo)%>%
  rename("Especies" = "Species", "detecciones"= "metadata_Grupo")%>%
  replace_na(list(detecciones=1))%>%
  mutate(Species2=Especies)%>%
  group_by(Station,utm_y,utm_x,detecciones,Especies)%>%
  summarise(Species2=n())%>%
  rename("nEspecies"="Species2" )%>%
  mutate(eventos=(nEspecies*detecciones))%>%
  select(Station, utm_x,utm_y,Especies,eventos)%>%
  group_by(Station,utm_y,utm_x,Especies)%>%
  summarise(eventos=sum(eventos))%>%
  filter(Especies=='Gato'|Especies=='Persona'|Especies=='Carpincho'
         |Especies=='Colilargo'|Especies=='Perro'|Especies=='Coipo' )




######Patrones de actividad##############

#circular plot horario gatos y gente


horarios <- read.csv("Fototrampeo/HorariosPREPOST.csv",header=T,sep=";",stringsAsFactors=FALSE)

horarios <- as_tibble(horarios)

horariosPREPOST <- horarios%>%
  select(PRE_POST,Station,DateTimeOriginal,Species,metadata_Grupo)%>%
    rename(FechayHora=DateTimeOriginal, Grupo=metadata_Grupo)%>%
      mutate(Cuando=dmy_hm(FechayHora))%>%
        mutate(hora_redondeo=round_date(Cuando,"hour"))
       
        
horarios_PREPOST <- horariosPREPOST %>%
  mutate_at(vars(Species),factor) %>%
    replace_na(list(Grupo=1)) %>%
      filter(Species=="Gato")%>%
        group_by(PRE_POST,Station,Species,.drop=FALSE)%>%
          mutate(hora=hour(hora_redondeo))%>%
              filter(PRE_POST=='PRE'|PRE_POST=='POST')


write.csv(horarios_PREPOST,file="horariosprepost.csv" )
horariosPREPOSTOK <- read.csv("horarios_PREPOSTOK.csv",header=T,sep=";",stringsAsFactors=FALSE)

horariosPREPOSTOK2 <- horarios_PREPOSTOK

###### ESTE HORARIO ES EL QUE VA ############

horariosOK <- horariosPREPOSTOK2 %>%
  select(PRE_POST,Station,FechayHora,Species,Grupo)%>%
    mutate(Cuando=dmy_hm(FechayHora))%>%
      mutate(hora_redondeo=round_date(Cuando,"hour"))%>%
        mutate_at(vars(Species),factor) %>%
          group_by(PRE_POST,Station,Species,.drop=FALSE)%>%
            mutate(hora=hour(hora_redondeo))%>%
              filter(PRE_POST=='PRE'|PRE_POST=='POST')

write.csv (horariosOK, file= "patronesactividad.csv") 

###############ES ESTE EL POSTA - PREPOST BARRAS###########


PA18y19 <-
  
  ggplot(horariosOK, aes(x =hora, fill = PRE_POST)) +
  
   geom_bar( position=position_dodge(),width =0.7, show.legend = T, size=1, linetype=1, )+

  
  scale_fill_manual(values = alpha(c("dodgerblue3","green3"), .9)) +
  scale_y_continuous("Detecciones independientes", breaks =c(0,30,60,90))+
  coord_trans(y = magnify_trans(reducer = 30))+ #compresion
  
  scale_x_continuous ("Hora del d?a (hs)",limits = c(-1,25),  breaks = seq(00,23), labels = seq(00,23))+
  
  
  labs(title="", fill = "Etapa",size=5,tag="")+
  theme(text = element_text(size = 12))+
  theme ( plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white",colour = "black",linetype = "solid"),
          panel.grid.major = element_line(size = .65, linetype = 'solid',colour = "darkgray",), 
          panel.grid.minor  = element_line(size = .65, linetype = 'solid',colour = "darkgray"),
          plot.tag.position = "top", plot.tag = element_text(size=15, face = "bold"),
          legend.text = element_text(size=11),plot.title = element_text(size=12,hjust = 0.5),
          legend.position = "right",legend.title=element_text(size=10),
          axis.text.x=element_text(size=10,color="black", face ="bold"), 
          axis.text.y=element_text(size=10,color="black", face ="bold"))

PA18y19

ggsave("horarios18y19.jpeg",PA18y19,scale=1,width = 70,
       height = 50,units = "cm" ,dpi = 300, limitsize = F)



###########PRE##############

metadataPRE<- as_tibble(metadataPRE)
metadataPOST <- as_tibble(metadata2019)

horarios2018 <- metadataPRE %>%
  select(Station,DateTimeOriginal,metadata_SP, metadata_Grupo)%>% 
  rename(FechayHora=DateTimeOriginal, Species=metadata_SP, Grupo=metadata_Grupo)%>% 
  mutate(Cuando=dmy_hm(FechayHora))

horarios2019 <- metadataPOST %>%
  select(Station,DateTimeOriginal,metadata_SP, metadata_Grupo)%>% 
  rename(FechayHora=DateTimeOriginal, Species=metadata_SP, Grupo=metadata_Grupo)%>% 
  mutate(Cuando=dmy_hm(FechayHora))

horariosPRE <- horarios2018%>%
  mutate_at(vars(Species),factor)%>%
  replace_na(list(Grupo=1))%>%
  group_by(Station,Species,.drop=FALSE)%>%
  mutate(hora=hour(Cuando))%>%
  filter(Species=='Gato')

horariosPOST <- horarios2019 %>%
  mutate_at(vars(Species),factor)%>%
  replace_na(list(Grupo=1))%>%
  group_by(Station,Species,.drop=FALSE)%>%
  mutate(hora=hour(Cuando))%>%
  filter(Species=='Gato')

############MAPA PRE###########
PA18 <- ggplot(horariosPRE, aes(x =hora, fill = Species)) +
  geom_bar( width =0.6,show.legend = T, size=2  )+
  
  theme_minimal(base_size = 2,  base_line_size = 2.5, base_rect_size = 0)+ 
  scale_fill_manual(values = alpha(c("green", "red"), .22)) +
  scale_y_continuous("Detecciones por hora",breaks =c(0,5,10,15,20,25,30,35,40) )+
  scale_x_continuous("",limits = c(0,24),  breaks = seq(0,24), labels = seq(0,24))+
  labs(title="Patrón de actividad 2018",fill = "",size=15,tag="")+
   
  theme(text = element_text(size = 15))+
  theme ( plot.background = element_rect(fill = "ivory1"),
          panel.background = element_rect(fill = "ivory1",colour = "ivory1",
                                          size = 0, linetype = "solid"),
          plot.tag.position = "topright",
          plot.tag = element_text(size=10),
          plot.title = element_text(size=14,hjust = 0.5),
          legend.text = element_text(size=10),
          legend.title=element_text(size=10),
          legend.position = "bottom",
          axis.text.x=element_text(size=15,color="black"),
          axis.text.y=element_text(size=10,color="black"))

PA18
###################MAPA POST#############
PA19 <- ggplot(horariosPOST, aes(x =hora, fill = Species)) +
  geom_bar( width =0.6,show.legend = T, size=2 )+
  
  theme_minimal(base_size = 2,  base_line_size = 2.5, base_rect_size = 0) + 
  
  scale_fill_manual(values = alpha(c("blue", "red"), .22)) +
  scale_y_continuous("Detecciones por hora", breaks =c(0,10,20,30,40))+
  scale_x_continuous("",limits = c(0,24),  breaks = seq(0,24), labels = seq(0,24))+
  labs(title="Patrón de actividad 2019",fill = "",size=15)+
  theme(text = element_text(size = 15))+
  theme (
    plot.background = element_rect(fill = "ivory1"),
    panel.background = element_rect(fill = "ivory1",colour = "ivory1",
                                    size = 0, linetype = "solid"),
          plot.tag.position = "topright",
          plot.tag = element_text(size=10),
          plot.title = element_text(size=14,hjust = 0.5),
          legend.text = element_text(size=10),
          legend.title=element_text(size=10),
          legend.position = "bottom",
    
          axis.text.x=element_text(size=15,color="black"),
          axis.text.y=element_text(size=12,color="black"))
PA19




ggsave("horarios2018.png",PA18,scale=5,width = 30,
       height = 30,units = "cm" ,dpi = 300, limitsize = F)






