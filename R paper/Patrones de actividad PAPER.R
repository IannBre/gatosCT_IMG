
### Determino la direccion de trabajo ###
choose.dir()

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
library(overlap)

### Patrones de actividad ------------

#Data frame --------------

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

write.csv(horarios_PREPOST,file="horariosprepost.csv")

#Archivo excel --------------
horariosPREPOSTOK <- read.csv("E:\\Análisis_R\\R\\Camaras trampa\\FOTOTRAMPEO MAPA Y PATRONES\\horarios_PREPOSTOK.csv",header=T,sep=";",stringsAsFactors=FALSE)

horariosPREPOSTOK2 <- horarios_PREPOSTOK

#Manejo de Data Frame -----------

horariosOK <- horariosPREPOSTOK2 %>%
  select(PRE_POST,Station,FechayHora,Species,Grupo)%>%
  mutate(Cuando=dmy_hm(FechayHora))%>%
  mutate(hora_redondeo=round_date(Cuando,"hour"))%>%
  mutate_at(vars(Species),factor) %>%
  group_by(PRE_POST,Station,Species,.drop=FALSE)%>%
  mutate(hora=hour(hora_redondeo))%>%
  filter(PRE_POST=='PRE'|PRE_POST=='POST')

write.csv (horariosOK, file= "patronesactividad.csv") #este lo tengo en excel

#### Archivo para Overlap -----

#cargar en excel
actividad18_21 <- read.csv("E:/Análisis_R/IMG/data/patronesdeactividad18_21.csv"
                        ,header=T,sep=";",stringsAsFactors=FALSE)

library(base)
library(help = "base")

## Ejemplos de Overlap -----

#'[ Ejemplo de vignolet] ------
#'Times of capture of large mammals in camera traps in Kerinci Seblat National Park, Indonesia.

data("kerinci")
str(kerinci) 
#Time is in days, ie. 0 to 1:
range(kerinci$Time) 
#Convert to radians: 
timeRad <- kerinci$Time * 2*pi
# Extract data for tiger and tapir for Zone3: 
spsA <- timeRad[kerinci$Zone == 3 & kerinci$Sps == 'tiger']
spsB <- timeRad[kerinci$Zone == 3 & kerinci$Sps == 'tapir'] 
# Plot the data: 
overlapPlot(spsA, spsB)
# Tapir are mainly nocturnal 
overlapPlot(spsA, spsB, xcenter="midnight")
legend('topleft', c("Tiger", "Tapir"), lty=c(1, 2), 
       col=c("black", "blue"), bty='n')

# Check sample sizes: 
length(spsA)
length(spsB)
# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap: 
(Dhats <- overlapEst(spsA, spsB) ) # or just get Dhat1 
(Dhat1 <- overlapEst(spsA, spsB, type="Dhat1") )

# Do 999 smoothed bootstrap values: 
bs <- bootstrap(spsA, spsB, 999, type="Dhat1") 
mean(bs) 
hist(bs) 
abline(v=Dhat1, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]

#'[ FIN Ejemplo de vignolet] ---------

#'[Optimal bandwidth calculation] ---
#'Calculates the optimal bandwidth for von Mises kernel density estimation 
#'for a given sample. Used internally by other functions in the package.

data(simulatedData) 
getBandWidth(tigerObs, kmax = 3)

#'[Overlap Estimation] ---
#'Calculates up to three estimates of activity pattern overlap 
#'based on times of observations for two species.

overlapEst(A, B, kmax = 3, adjust=c(0.8, 1, 4), 
           n.grid = 128, type=c("all", "Dhat1", "Dhat4", "Dhat5"))

#A:a vector of times of observations of species A in radians
#kmax: maximum value of k for optimal bandwidth estimation.
#adjust: bandwidth adjustment; either a single value used for all 3 overlap estimates, or a vector of 3 different values.
#ngrid: number of points at which to estimate density for comparison between species; smaller values give lower precision
#but run faster in simulations and bootstraps.

# Get example data: 
data(simulatedData) 
# Use defaults: 
overlapEst(tigerObs, pigObs) #da todos los Dhat
overlapEst(tigerObs, pigObs, type="Dhat4") #le pedis el Dhat4


#'[Overlap Plot] ---
#'Fits kernel density functions to two data sets and plots them,
#' shading the area corresponding to the coefficient of overlap.

# Do basic plot with defaults: 
overlapPlot(pigObs, tigerObs)
# Make it prettier: 
overlapPlot(tigerObs, pigObs, linet = c(1,1), 
            linec = c("red", "blue"), rug=TRUE, extend="lightgreen", main="Simulated data")
legend("topleft", c("Tiger", "Pig"), lty=1, col=c("red", "blue"), bg="white")

# Add vertical dotted lines to mark sunrise (05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.5, 18+47/60), lty=3)

# A plot centered on midnight:
overlapPlot(pigObs, tigerObs, xcenter = "m", rug=TRUE)
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(5.5, (18+47/60)- 24), lty=3)

#'[Overlap True] ---
#'Calculates the true coefficient of overlapping between two distributions.
data(simulatedData)

overlapTrue(tigerTrue, pigTrue)
overlapTrue(cbind(tigerTrue, pigTrue))

#sunTime ejemplo -----
#'[sunTime] ---
#'Converts a vector of clock times to "sun times", by mapping sunrise 
#'to π/2 and sunset to 3π/2. Sunrise and sunset times are determined based on the dates and locations provided. See Nouvellet et al (2012) for a discussion. Requires the maptools package.

library(sp)
library(maptools)


#Usage
sunTime(clockTime, #a vector of times of observations in radians, ie. scaled to [0,2π].
        Dates, #a POSIXct object with the dates of the observations; the time zone must be set to the time zone used for ’clockTime’.
        Coords) #a SpatialPoints object with the locations of the observations or with a single point giving a approximate location for the study area; the coordinates must be geographical coordinates, eg, WGS84, with long before lat.

#Example
# Check that sp and maptools packages are installed
if(requireNamespace("sp") && requireNamespace("maptools")) {

# Get example data:
data(simCalls)
str(simCalls)

# Convert dates to a POSIXct object with the right time zone (GMT):
Dates <- as.POSIXct(simCalls$dates, tz="GMT")

# Create a SpatialPoints object with the location
coords <- matrix(c(-3, 56), nrow=1)
Coords <- sp::SpatialPoints(coords, proj4string=sp::CRS
                            ("+proj=longlat +datum=WGS84"))

st <- sunTime(simCalls$time, Dates, Coords)
par(mfrow=2:1) 
densityPlot(st, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24), 
labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(simCalls$time, lwd=2, main="Clock time") 
par(mfrow=c(1,1))
}

# Manejo de datos GATOS 18-19 -----

#limpiar base de datos

patronesgatos <- actividad18_19%>%
  select(Station, Species, Cuando, hora_redondeo)

write.csv (patronesgatos, file= "patronesgatos.csv")

## Overlap gatos 18 y 19 grafico ----
##USAMOS RADIANES ASI QUE HAY QUE TRABAJAR EN LA TABLA DE EXCEL
#hago una columna time_2 donde copio el tiempo como numero (formato "general" em excel)

#cargar en excel
patrones_num <- read.csv("E:/Análisis_R/IMG/data/patronesradianes18-21.csv",
                         header=T,sep=";",stringsAsFactors=FALSE)

#convertir los numeros decimales a radianes
class(patrones_num$time_3)
patrones_num$time_3 <- str_replace(patrones_num$time_2, ",", ".")
patrones_num$rad <- as.numeric(patrones_num$time_3) * 2 * 3.14159265359
hora_radianes_gatos <- as.numeric(patrones_num$time_3) * 2 * 3.14159265359

#defino las especies a graficar
gato2018 <- hora_radianes_gatos [patrones_num$Species == "Gato_2018"]
gato2019 <- hora_radianes_gatos [patrones_num$Species == "Gato_2019"]
gato2021 <- hora_radianes_gatos [patrones_num$Species == "Gato_2021"]

## ahora grafico con radianes
#gatos2018
densityPlot(gato2018,
            xscale=24,
            aspect="fill",
            linewidth =c(3,3,5),
            addLegend = F,
            legendPosition="center",
            extend= "lightgrey",
            olapcol="darkgrey",
            main= "Activity patterns",
            ylim=c(0,0.12),
            add.rug = TRUE,
            xlab="Time (hs)", ylab="Density", 
            bty = "L",
            col="darkred",
            lty=1
)

#gatos2019
densityPlot(gato2019,
            add=T, col="darkgreen", lty=2)


legend ("topright", inset=c(0,0.5),
        title="Cats activity", c("2018", "2019"),
        lty=1:3, col = c("darkred","green"),
        bty="n")

#gatos2021
densityPlot(gato2021,
            add=T, col="darkblue", lty=2)


legend ("bottomleft", inset=c(0,0.5),
        title="Cats activity", c("2018", "2019", "2021"),
        lty=1:3, col = c("darkred","green","blue"),
        bty="n")


#Calculando Dhat
#ver el minimo numero de registros para saber que Dhat usar
min(length(gato2018),length(gato2019), length(gato2021))

#estimamos Dhat
gatos_delta <- overlapEst(gato2018,gato2019,gato2021)
gatos_delta


### GRAFICO solapamiento PAPER ------------

library(camtrapR)

#18vs19 ----

overlapPlot(gato2018,gato2019, xcenter="noon", linet = c(1,2), linewidth =c(1,1),
            main="", linecol = c("darkblue","darkred"), xscale = 24,
           rug=FALSE, extend = "darkgrey", bty="L", xlab="Hour", ylab="Density")

legend("topleft", c("2018","2019"), bty="o", lwd=1.7, xpd=TRUE, inset=c(.15, 0), cex=.87 
      , lty =c(1,2), col = c("darkblue","darkred"), bg="white",horiz = F)

# Add vertical dotted lines to mark sunrise (07:03) and sunset (18:39): 
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(7, 18+39/60), lty=3)

# Check sample sizes: 
length(gato2018)
length(gato2019)
# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap: 
(Dhats <- overlapEst(gato2018, gato2019) ) 
(Dhat4 <- overlapEst(gato2018, gato2019, type="Dhat4") )
# Do 999 smoothed bootstrap values: 
bs <- bootstrap(gato2018, gato2019, 1000, type="Dhat4") 
mean(bs) 
hist(bs) 
abline(v=Dhat4, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)


#Se obtienen los IC al 95% por medio del remuestreo bootstrap. Se recomienda minimo de 1000 de remuestreo. 
#El bootEst es el estimado. Ver pagina 146 
# Get confidence intervals:
bootCI(Dhat4, bs)['norm0', ]
bootCI(Dhat4, bs)['basic0', ]

#Dhat: medida no parametrica para comparar horarios de actividad. Delta va de 0 a 1. Donde 1: actividad igual y 0:actividad diferente. Ver Shmidt y Schmidt 2006 y Ridout y Linkie 2009. 
#Se debe tomar el estimador de la muestra mas pequeña. Si es menor a 5o registros, va Dhat 1. Si es mayor a 75 va Dhat 4. El Dhat 5 no se recomienda usar por su alta variabilidad que depende de la cantidad de registros utilizados

#Coeficiente de solapamiento es  0.85 CI 95% 0.8-0.89, no hay diferencias significativas en los PA.

#18vs21 -----

overlapPlot(gato2018,gato2021, xcenter="noon", linet = c(1,2), linewidth =c(1,1),
            main="", linecol = c("darkblue","darkgreen"), xscale = 24,
            rug=FALSE, extend = "darkgrey", bty="L", xlab="Hour", ylab="Density")

abline(v=c(7, 18+39/60), lty=3)

legend("topleft", c("2018","2021"), bty="o", lwd=1.7, xpd=TRUE, inset=c(.15, 0), cex=.87 
       , lty =c(1,2), col = c("darkblue","darkgreen"), bg="white",horiz = F)



length(gato2018)
length(gato2021)
# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.
# Calculate estimates of overlap: 
(Dhats <- overlapEst(gato2018, gato2021) ) 
(Dhat1 <- overlapEst(gato2018, gato2021, type="Dhat1") )
# Do 1000 smoothed bootstrap values: 
bs <- bootstrap(gato2018, gato2021, 1000, type="Dhat1") 
mean(bs) 
hist(bs) 
abline(v=Dhat1, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]

#Coeficiente de solapamiento es  0.76 CI 95% 0.64-0.84, no hay diferencias significativas en los PA.

#19vs21 -----

overlapPlot(gato2019,gato2021, xcenter="noon", linet = c(1,2), linewidth =c(1,1),
            main="", linecol = c("darkred","darkgreen"), xscale = 24,
            rug=FALSE, extend = "darkgrey", bty="L", xlab="Hour", ylab="Density")

abline(v=c(7, 18+39/60), lty=3)

legend("topleft", c("2019","2021"), bty="o", lwd=1.7, xpd=TRUE, inset=c(.15, 0), cex=.87 
       , lty =c(1,2), col = c("darkred","darkgreen"), bg="white",horiz = F)


length(gato2019)
length(gato2021)

# If the smaller sample is less than 50, Dhat1 gives the best estimates, together with
# confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.
# Calculate estimates of overlap: 
(Dhats <- overlapEst(gato2019, gato2021) ) 
(Dhat1 <- overlapEst(gato2019, gato2021, type="Dhat1") )
# Do 999 smoothed bootstrap values: 
bs <- bootstrap(gato2019, gato2021, 1000, type="Dhat1") 
mean(bs) 
hist(bs) 
abline(v=Dhat1, col='red', lwd=2)
abline(v=mean(bs), col='blue', lwd=2, lty=3)

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]

#Coeficiente de solapamiento es 0.74 CI 95% 0.60-0.81, no hay diferencias significativas en los PA.







#ggplot circular gatos -----
 
#No hay registros en 24 y si en 0. Elimina estos registros si corro reloj
#Si agrego una celda grafica más, corre bien pero no representa un reloj

horariosgatos <- ggplot(actividad18_19, aes(x=hora, fill=Species)) + 
  geom_bar(width = .9) + 
  coord_polar() +
  theme_minimal() + 
  scale_fill_manual(values = alpha(c("blue", "red"), .6)) + 
  scale_x_continuous("", limits = c(-1,24), breaks = seq(0,23), labels = seq(0,23))+
  scale_y_continuous("Detections per hour")+
  labs(fill = "",size=50)+
  theme(text = element_text(size = 20))
horariosgatos

###graficos individuales de actividad####


