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
library(circular)
library(magrittr)
library(stringr)
###para actualizar
remove.packages("htmltools")
install.packages("htmltools")
###

setwd("E:/Análisis_R/IMG")

### Patrones de actividad ------------

# Data frame GATOS 18-21 para overlap -----

patrones_num <- read.csv("E:/Análisis_R/IMG/data/patronesradianes18-21.csv",
                         header=T,sep=";",stringsAsFactors=FALSE)



#convertir los numeros decimales a radianes
class(patrones_num$time_3)
patrones_num$time_3 <- str_replace(patrones_num$time_2, ",", ".")
patrones_num$rad <- as.numeric(patrones_num$time_3) * 2 * 3.14159265359
hora_radianes_gatos <- as.numeric(patrones_num$time_3) * 2 * 3.14159265359

#me quedo con la dataframe a trabajar
circular_data <-patrones_num %>%
  select(Station, zona, etapa, zona_etapa, Species, Cuando, rad)

#defino las especies a graficar
gato2018_rad <- hora_radianes_gatos [circular_data$Species == "Gato_2018"]
gato2019_rad <- hora_radianes_gatos [circular_data$Species == "Gato_2019"]
gato2021_rad <- hora_radianes_gatos [circular_data$Species == "Gato_2021"]

## Grafico densityPlot ----
# Para 3 años sin área bajo la curva

par(mfrow=c(1,1), mar=c(4,4,1,1))

#gatos2018
densityPlot(gato2018_rad, 
            xscale=24,aspect="fill",linewidth =c(2,2,2), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "",ylim=c(0,0.12),
            add.rug = T,  xlab="Time", ylab="Density", bty = "L", col="darkblue",
            lty=1 )

#gatos2019
densityPlot(gato2019_rad,add=T, col="darkred", lty=2)


#gatos2021
densityPlot(gato2021_rad,  add=T, col="darkgreen", lty=3)

abline(v=c(7, 18+39/60), lty=2, 
       lwd = 1) #mark sunrise (07:03) and sunset (18:39)

legend ("bottomleft",  inset=c(0.15, 0.75), cex=.8, c("2018 (n=347)", "2019 (n=1004)", "2021 (n=55)"),
        lwd=1,  xpd=F, 
        lty=1:3, col = c("darkblue","darkred","darkgreen"), bty="y", bg="white")


## GRAFICO overlapPlot -------

par(mfrow=c(1,1), mar=c(4,4,1,1)) #un grafico por imagen
par(mfrow=c(1,3), mar=c(4,4,1,1)) #para tres graficos en una imagen


#### 18vs19 ----
overlapPlot(gato2018_rad,gato2019_rad, xcenter="n", linet = c(1,2), linewidth =c(1,1),
            main="", linecol = c("darkblue","darkred"), xscale = 24, 
            rug=FALSE, extend = "darkgrey", bty="L", xlab="Time", ylab="Density")

abline(v=c(7, 18+39/60), lty=2, 
       lwd = 1) #mark sunrise (07:03) and sunset (18:39)

legend("topleft", c("2018 (n=347)","2019 (n=1004)"), bty="y", #borde y/n
       lwd=1,  xpd=F, inset=c(0.1, 0.02), cex=.7 
       , lty =c(1,2), col = c("darkblue","darkred"), bg="white",horiz = F)

text(2,0.055,substitute(paste('Δ4 = 0.85\n(CI 0.81-0.9)')),cex=.8, 
     col="black", font=3,adj=0)

#paste(Delta)[4]==
#0.0055 cuando van de a tres

# Check min sample sizes: 
min(length(gato2018_rad), length(gato2019_rad)) # If the smaller sample is less than 50, Dhat1 gives the best estimates, together with confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap: 
delta <- overlapEst(gato2018_rad, gato2019_rad, type="Dhat4") 
delta #Dhat: medida no parametrica para comparar horarios de actividad. Delta va de 0 a 1. Donde 1: actividad igual y 0:actividad diferente. Ver Shmidt y Schmidt 2006 y Ridout y Linkie 2009. 

#2018
gato2018_rad <- resample(gato2018_rad, 1000) # Do 1000 smoothed bootstrap values. Pueden ser 10 mil mejor 
dim(gato2018_rad)
#2019
gato2019_rad <- resample(gato2019_rad, 1000) # Do 1000 smoothed bootstrap values: 
dim(gato2019_rad)

#overlap coeficient for interaction
G18G19 <-bootEst(gato2018_rad,gato2019_rad, adjust = c(0.8,1,4)) #siempre tiene que ser smooth = TRUE (the default), smoothed bootstrap samples are generated con valores que caigan fuera de los intervalos de mi muestra de  horarios

# Valor promedio del 1000 smoothed bootstrap values: 
BSmean <- colMeans(G18G19)
BSmean

## Get confidence intervals:
tmp <-G18G19[,1]
CI1819 <- bootCI(G18G19[1], tmp) #IC al 95%. This is perc in the output from overlap’s bootCI function. 
bootEst(gato2018_rad,gato2019_rad, type = ("Dhat4")) #El bootEst es el estimado.
sd1819 <- sd(bootEst(gato2018_rad, gato2019_rad, type=("Dhat4"))) #sd del estimado

#valores del delta ± sd
coef_est_1819 <-  0.8481468 #Dhat4
delta1819_menos <- coef_est_1819 - sd1819
delta1819_mas <- coef_est_1819 + sd1819

#CI
Conf.Int1819 <- CI1819[5,]
coef_est_1819 #delta 0.85, no hay diferencias significativas en los PA.
Conf.Int1819 #CI 95% 0.81-0.9


#### 18vs21 -----

overlapPlot(gato2018_rad,gato2021_rad, xcenter="noon", linet = c(1,3), linewidth =c(1,1),
            main="", linecol = c("darkblue","darkgreen"), xscale = 24,
            rug=FALSE, extend = "darkgrey", bty="L", xlab="Time", ylab="Density")

abline(v=c(7, 18+39/60), lty=2, 
       lwd = 1) #mark sunrise (07:03) and sunset (18:39)

legend("topleft", c("2018 (n=347)","2021 (n=55)"), bty="y", #borde y/n
       lwd=1,  xpd=T, inset=c(.1, .02), cex=.7 
       , lty =c(1,3), col = c("darkblue","darkgreen"), bg="white",horiz = F)


text(1.5,0.07,substitute(paste('Δ1=0.76\n(CI 0.66-0.85)')),cex=.8, col="black", font=3,adj=0)

# Check min sample sizes: 
    min(length(gato2018_rad), length(gato2021_rad)) # If the smaller sample is less than 50, Dhat1 gives the best estimates, together with confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.
       
 # Calculate estimates of overlap: 
    delta <- overlapEst(gato2018_rad, gato2021_rad, type="Dhat1") 
  delta #Dhat: medida no parametrica para comparar horarios de actividad. Delta va de 0 a 1. Donde 1: actividad igual y 0:actividad diferente. Ver Shmidt y Schmidt 2006 y Ridout y Linkie 2009. 
        
#2018
 gato2018_rad <- resample(gato2018_rad, 1000) # Do 1000 smoothed bootstrap values. Pueden ser 10 mil mejor 
  dim(gato2018_rad)
  
#2021
 gato2021_rad <- resample(gato2021_rad, 1000) # Do 1000 smoothed bootstrap values: 
  dim(gato2021_rad)
        
#overlap coeficient for interaction
  G18G21 <-bootEst(gato2018_rad,gato2021_rad, adjust = c(0.8,1,4)) #siempre tiene que ser smooth = TRUE (the default), smoothed bootstrap samples are generated con valores que caigan fuera de los intervalos de mi muestra de  horarios
        
# Valor promedio del 1000 smoothed bootstrap values: 
 BSmean <- colMeans(G18G21)
    BSmean
        
 ## Get confidence intervals:
  tmp <-G18G21[,1]
 CI1821 <- bootCI(G18G21[1], tmp) #IC al 95%. This is perc in the output from overlap’s bootCI function. 
  bootEst(gato2018_rad,gato2021_rad, type = ("Dhat1")) #El bootEst es el estimado.
 sd1821 <- sd(bootEst(gato2018_rad, gato2021_rad, type=("Dhat1"))) #sd del estimado
        
#valores del delta ± sd
  coef_est_1821 <-  0.7570449 #Dhat1
        delta1821_menos <- coef_est_1821 - sd1821
        delta1821_mas <- coef_est_1821 + sd1821
        
        #CI
        Conf.Int1821 <- CI1821[5,]
        coef_est_1821 #delta 0.76, hay solapamiento de patrones de actividad
        Conf.Int1821 #CI 95% 0.66-0.85

-------------------------------------        
        
#### Opcion b ----
#18 y 21
length(gato2018)
length(gato2021_rad)

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

--------------------------------------------------------------------
  
#agrego a grafico
abline(v=c(7, 18+39/60), lty=2, 
       lwd = 1) #mark sunrise (07:03) and sunset (18:39)

legend("topleft", c("2018","2019"), bty="y", #borde y/n
       lwd=1,  xpd=T, inset=c(.15, 0), cex=.75 
       , lty =c(1,3), col = c("darkblue","darkgreen"), bg="white",horiz = F)


text(12,0.01,substitute(paste('Δ1=0.76\n(0.66-0.85)')),
     cex=.75, col="black", font=3,adj=0)


#Coeficiente de solapamiento es  0.76 CI 95% 0.64-0.84, no hay diferencias significativas en los PA.


#### 19vs21 -----

overlapPlot(gato2019_rad,gato2021_rad, xcenter="noon", linet = c(2,3), linewidth =c(1,1),
            main="", linecol = c("darkred","darkgreen"), xscale = 24,
            rug=F, extend = "darkgrey", bty="L", xlab="Time", ylab="Density")


abline(v=c(7, 18+39/60), lty=2, 
       lwd = 1) #mark sunrise (07:03) and sunset (18:39)

legend("topleft", c("2019 (n=1004)","2021 (n=55)"), bty="y", #borde y/n
       lwd=1,  xpd=T, inset=c(0.05, 0.01), cex=.7 
       , lty =c(2,3), col = c("darkred","darkgreen"), bg="white",horiz = F)

text(1.5,0.075,substitute(paste('Δ1=0.74\n(CI 0.64-0.85)')),
     cex=.8, col="black", font=3,adj=0)

# Check min sample sizes: 
min(length(gato2019_rad), length(gato2021_rad)) # If the smaller sample is less than 50, Dhat1 gives the best estimates, together with confidence intervals from a smoothed bootstrap with norm0 or basic0 confidence interval.

# Calculate estimates of overlap: 
delta <- overlapEst(gato2019_rad, gato2021_rad, type="Dhat1") 
delta #Dhat: medida no parametrica para comparar horarios de actividad. Delta va de 0 a 1. Donde 1: actividad igual y 0:actividad diferente. Ver Shmidt y Schmidt 2006 y Ridout y Linkie 2009. 

#2019
gato2019_rad <- resample(gato2019_rad, 1000) # Do 1000 smoothed bootstrap values. Pueden ser 10 mil mejor 
dim(gato2019_rad)

#2021
gato2021_rad <- resample(gato2021_rad, 1000) # Do 1000 smoothed bootstrap values: 
dim(gato2021_rad)

#overlap coeficient for interaction
G19G21 <-bootEst(gato2019_rad, gato2021_rad, adjust = c(0.8,1,4)) #siempre tiene que ser smooth = TRUE (the default), smoothed bootstrap samples are generated con valores que caigan fuera de los intervalos de mi muestra de  horarios

# Valor promedio del 1000 smoothed bootstrap values: 
BSmean <- colMeans(G19G21)
BSmean

## Get confidence intervals:
tmp <-G19G21[,1]
CI1921 <- bootCI(G19G21[1], tmp) #IC al 95%. This is perc in the output from overlap’s bootCI function. 
bootEst(gato2019_rad,gato2021_rad, type = ("Dhat1")) #El bootEst es el estimado.
sd1921 <- sd(bootEst(gato2019_rad, gato2021_rad, type=("Dhat1"))) #sd del estimado
sd1921
#valores del delta ± sd
coef_est_1921 <-  0.7424682 #Dhat1
delta1921_menos <- coef_est_1921 - sd1921
delta1921_mas <- coef_est_1921 + sd1921

#CI
Conf.Int1921 <- CI1921[5,]
coef_est_1921 #delta 0.74, hay solapamiento de patrones de actividad
Conf.Int1921 #CI 95% 0.64-0.85

#### Opcion b ----------
#19 y 21
length(gato2019_rad)
length(gato2021_rad)

# Calculate estimates of overlap: 
(Dhat1 <- overlapEst(gato2019_rad, gato2021_rad, type="Dhat1") )
# Do 1000 smoothed bootstrap values: 
bs <- bootstrap(gato2019_rad, gato2021, 1000, type="Dhat1") 
mean(bs) 

# Get confidence intervals:
bootCI(Dhat1, bs)['norm0', ]
bootCI(Dhat1, bs)['basic0', ]
bootCI(Dhat1, bs)['perc', ]

---------------------------------------



####Impresion de gráfico para mejor calidad ----

dpi <- 1500
png(filename= "PRUEBA.png", width = 15, height = 15,
    units = "cm", res = dpi)
file.show("PRUEBA.png")
dev.copy(png, "PRUEBA2.png")      

dev.off()


---------------------------------------------------------------------------------
# Objeto circular para test estadistico -----

library(circular)

circular_hour <- read.csv("E:/Análisis_R/IMG/data/circulardata18_21.csv",
                         header=T,sep=";",stringsAsFactors=FALSE)

circular_hour$Horas_decimales <- as.numeric(gsub(",", ".", circular_hour$Horas_decimales))

class(circular_hour$Horas_decimales)


gato2018 <- circular_hour%>%
  select(ID,Station,Species,Tiempo,Horas_decimales)%>%
  rename(Time=Tiempo, D_Hour=Horas_decimales)%>%
  subset(circular_hour$Species == "Gato_2018")

gato2019 <- circular_hour%>%
  select(ID,Station,Species,Tiempo,Horas_decimales)%>%
  rename(Time=Tiempo, D_Hour=Horas_decimales)%>%
  subset(circular_hour$Species == "Gato_2019")

gato2021 <- circular_hour%>%
  select(ID,Station,Species,Tiempo,Horas_decimales)%>%
  rename(Time=Tiempo, D_Hour=Horas_decimales)%>%
  subset(circular_hour$Species == "Gato_2021")


#crear objeto circular
gato18_circular <- circular(gato2018$D_Hour, units="hour", template="clock24",
                            rotation="clock", zero=pi/2)

gato19_circular <- circular(gato2019$D_Hour, units="hour", template="clock24",
                            rotation="clock", zero=pi/2)

gato21_circular <- circular(gato2021$D_Hour, units="hour", template="clock24",
                            rotation="clock", zero=pi/2)
head(gato18_circular,5)
head(gato19_circular,5)
head(gato21_circular,5)

#circular plot ----

par(mfrow=c(1,3), mar=c(1,1,1,1))
#2018
plot.circular(gato18_circular)
text(x=1, y=1,labels="2018")
#2019
plot.circular(gato19_circular, stack=T)
text(x=1, y=1,labels="2019")
#2021
plot.circular(gato21_circular, stack=T)
text(x=1, y=1,labels="2021")

#diagrama de rosa con densidad de registros ----

par(mfrow=c(1,3), mar=c(1,1,1,1))

#2018
plot(density.circular(gato18_circular, bw=40), lwd=2,lty=1, 
     main="",shrink = 1.2)
points.circular(gato18_circular,stack = T)
rose.diag(gato18_circular,bins = 24, ticks = T,prop = 2.3,
          pch = 21,border = "black", col = "blue", add=T)
text(x=1, y=1,labels="2018")

#2019
plot(density.circular(gato19_circular, bw=40), lwd=2,lty=1, 
     main="",shrink = 1.2)
points.circular(gato19_circular,stack = T)
rose.diag(gato19_circular,bins = 24, ticks = T,prop = 2.3,
          pch = 21,border = "black", col = "red", add=T)
text(x=1, y=1,labels="2019")
     
#2021
plot(density.circular(gato21_circular, bw=40), lwd=2,lty=1, 
     main="",shrink = 1.3)
points.circular(gato21_circular,stack = T)
rose.diag(gato21_circular,bins = 24, ticks = T,prop = 2,
          pch = 21,border = "black", col = "green", add=T)
text(x=1, y=1,labels="2021")


#Media y mediana de los registros en tiempo

#Media circular, hora pico o acrofase
mean.circular(gato18_circular)
mean.circular(gato19_circular)
mean.circular(gato21_circular)

#mediana circular
median.circular(gato18_circular)
median.circular(gato19_circular)
median.circular(gato21_circular)

#Media negativa: se resta a 24 horas totales del dia
media.18 <- 24-6.987269
mediana.18 <- 24-15.83

#Equivalente en minutos
media_min_18 <- (media.18*15) / 0.25
tiempo_min_18 <- as.POSIXlt(media_min_18 * 3600, origin = "2018-08-08")


#Preferencia horaria: medidas de concentracion y dispersion ----
# en 0 los registros estan dispersos y en 1 concentrados. Vemos direccionalidad o agrupacion en un determinado arco de la circunferencia, preferencia de hora. 

#Longitud del vector promedio o r o LVP (hora promedio)
rho.circular(gato18_circular) #valor 0.38 es dispersion de registros

#Desvio estandar circular (homologo al desvio estandar lineal)
#Desviacion angular s
angular.deviation(gato18_circular)
#Desviacion estandar circular s0
sd.circular(gato18_circular)


#Intervalos de confianza y grafico ----- 
mle.vonmises.bootstrap.ci(gato18_circular, alpha = 0.05, 
                          reps = 1000) #significancia y remeuestreo

par(mfrow=c(1,1), mar=c(1,1,1,1))

#Graficar todo
plot.circular(gato18_circular, shrink = 1.2, stack = T)

#Graficar media y LVP (r) 
arrows.circular(mean.circular(gato18_circular), rho.circular(gato18_circular), x0=0,
                col="blue", lwd=1.9, lty=8)
#Graficar mediana
arrows.circular(median.circular(gato18_circular), rho.circular(gato18_circular), x0=0,
                col="red", lwd=1.9, lty=6)
#Obtener los IC
gato2018_CI <- mle.vonmises.bootstrap.ci(gato18_circular, 
                                         alpha = 0.05, reps = 1000)
#Graficar los CI
arrows.circular(gato2018_CI$mu.ci, x0=0, col="black", lwd=1.9, lty=6)


#Estadistica inferencial para distribucion uniforme y agrupada (von Mises) -----

#Que r sea cercano a 0 no indica que no exista unidireccionalidad (Zar, 2010)
#Hay que hacer estadistica: para spp catemerales hay tres test y para especies de actividad diurna o nocturna se hace Rayleigh test
#Hipotesis nula: la distribucion de los datos es al azar por lo tanto la distribucion es uniforme
#Hipotesis alternativa: los datos no están distribuidos al azar y muestran direccionalidad


#Rao´s spacing test
rao.spacing.test(gato18_circular, alpha = 0.05)
rao.spacing.test(gato19_circular, alpha = 0.05)
rao.spacing.test(gato21_circular, alpha = 0.05)

#Kuiper test
kuiper.test(gato18_circular, alpha = 0.05)
kuiper.test(gato19_circular, alpha = 0.05)
kuiper.test(gato21_circular, alpha = 0.05)

#Watson test 
watson.test(gato18_circular, alpha = 0.05)
watson.test(gato19_circular, alpha = 0.05)
watson.test(gato21_circular, alpha = 0.05)


# Comparacion estadistica de periodos de actividad -----

#### Watson test para dos muestras -----
#H0: la actividad de poblaciones de las dos muestras son identicas
#Ha: la actividad de poblaciones de las dos muestras son diferentes

watson.two.test(gato18_circular,gato19_circular,
                alpha=0.05)

watson.two.test(gato18_circular,gato21_circular,
                alpha=0.05)
watson.two.test(gato19_circular,gato21_circular,
                alpha=0.05)

#### Mardid-Watson-Wheeler test -----
#necesita que no existan regitros idénticos dentro de cada una de las muestras y entre muestras pq los elimina

watson.wheeler.test(list(gato18_circular,gato19_circular),
                    alpha=0.05)

#Eliminacion de duplicados desde la base original

gato18_sd <- gato2018[duplicated(gato2018$D_Hour),]
gato19_sd <- gato2019[duplicated(gato2019$D_Hour),]
gato21_sd <- gato2021[duplicated(gato2021$D_Hour),]

watson.wheeler.test(list(gato18_sd,gato19_sd),
                    alpha=0.05)

#Intervalos y coeficiente de solapamiento

#Para calcular el intervalo de actividad (95%): estima el 95% del tiempo que el individuo se mantiene activo
gato18_95 <- modal.region(gato18_circular, q=0.95, bw=5)
gato18_95$zeros

#Para intervalo de actividad nucleo (50%): intervalo en que se concentró el 50% de toda su actividad

gato18_50 <- modal.region(gato18_circular, q=0.50, bw=5)
gato18_50$zeros

#los numeros negativos deben ser restados a 24 y ser leidos de derecha a izquiera

#Lo graficamos

par(mfrow=c(1,2), mar=c(4,4,2,1))

plot(gato18_95, main = "Intervalo de actividad (95%)", ylab = "Densidad", xlab = "Horas",
     cex.main=0.8)
plot(gato18_50, main = "Intervalo de actividad nucelo (50%)", ylab = "Densidad", xlab = "Horas",
     cex.main=0.8)


# ANALISIS por zona: DENTRO DE CADA ZONA VEO QUE PASO POR AÑO ------------

#defino las zonas a graficar

#media
gato2018media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2018"]
gato2019media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2019"]
gato2021media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2021"]
#baja
gato2018baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2018"]
gato2019baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2019"]
gato2021baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2021"]
#alta
gato2018alta_rad <- hora_radianes_gatos [circular_data$zona_etapa == "alta_2018"]
gato2019alta_rad <- hora_radianes_gatos [circular_data$zona_etapa == "alta_2019"]


## Grafico densityPlot ----

# sin área bajo la curva

par(mfrow=c(1,1), mar=c(4,4,2,1))

#zona baja
#2018
densityPlot(gato2018baja_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "Low zone activity patterns",ylim=c(0,0.25),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2019
densityPlot(gato2019baja_rad,add=T, col="darkgreen", lty=2)

#2021
densityPlot(gato2021baja_rad,  add=T, col="darkblue", lty=3)

legend ("bottom", inset=c(0,0.8),cex=.82, title="Cats low zone activity", c("2018 (n=3)", "2019 (n=14)", "2021 (n=2)"),
        lty=1:3, col = c("darkred","darkgreen","darkblue"), bty="y", horiz = T)


# zona media----------------

#zona media
par(mfrow=c(1,1), mar=c(4,4,2,1))
#2018
densityPlot(gato2018media_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "Middle zone activity patterns",ylim=c(0,0.12),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2019
densityPlot(gato2019media_rad,add=T, col="darkgreen", lty=2)

#2021
densityPlot(gato2021media_rad,  add=T, col="darkblue", lty=3)

legend ("bottomleft", inset=c(0.1,0.6),cex=.82, title="Cats middle zone activity", c("2018 (n=46)", "2019 (n=21)", "2021 (n=53)"),
        lty=1:3, col = c("darkred","darkgreen","darkblue"), bty="y", horiz = F)

#zona alta----------------

#zona alta
par(mfrow=c(1,1), mar=c(4,4,2,1))
#2018
densityPlot(gato2018alta_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "High zone activity patterns",ylim=c(0,0.1),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2019
densityPlot(gato2019alta_rad,add=T, col="darkgreen", lty=2)


legend ("bottomleft", inset=c(0.15,0.7),cex=.82, title="Cats high zone activity", 
        c("2018 (n=298)", "2019 (n=969)"),
        lty=1:2, col = c("darkred","darkgreen"), bty="y", horiz = F)


# ANALISIS por año*zona: DENTRO DE CADA AÑO VEO QUE PASO POR ZONA ------------

#defino las zonas a graficar

#2018*zona
gato2018baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2018"]
gato2018media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2018"]
gato2018alta_rad <- hora_radianes_gatos [circular_data$zona_etapa == "alta_2018"]
#2019*zona
gato2019baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2019"]
gato2019media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2019"]
gato2019alta_rad <- hora_radianes_gatos [circular_data$zona_etapa == "alta_2019"]
#2021*zona
gato2021baja_rad <- hora_radianes_gatos [circular_data$zona_etapa == "baja_2021"]
gato2021media_rad <- hora_radianes_gatos [circular_data$zona_etapa == "media_2021"]

## Grafico densityPlot ----

# sin área bajo la curva

par(mfrow=c(1,1), mar=c(4,4,2,1))

#2018*baja
densityPlot(gato2018baja_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "2018 zone activity patterns",ylim=c(0,0.25),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2018*media
densityPlot(gato2018media_rad,add=T, col="darkgreen", lty=2)

#2018*alta
densityPlot(gato2018alta_rad,  add=T, col="darkblue", lty=2)

legend ("bottom", inset=c(0,0.8),cex=.7, title="2018 zone activity", c("low (n=3)", "middle (n=46)", "high (n=398)"),
        lty=1:3, col = c("darkred","darkgreen","blue"), bty="y", horiz = T)


#2019----------------
par(mfrow=c(1,1), mar=c(4,4,2,1))

#2019*baja
densityPlot(gato2019baja_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "2019 zone activity patterns",ylim=c(0,0.12),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2019*media
densityPlot(gato2019media_rad,add=T, col="darkgreen", lty=2)

#2019*alta
densityPlot(gato2019alta_rad,  add=T, col="darkblue", lty=2)

legend ("bottomright", inset=c(0.01,0.7),cex=.65, title="2019 zone activity", c("low (n=14)", "middle (n=21)", "high (n=969)"),
        lty=1:3, col = c("darkred","darkgreen","blue"), bty="y", horiz = F)




#2021----------------
par(mfrow=c(1,1), mar=c(4,4,2,1))

#2021*baja
densityPlot(gato2021baja_rad, 
            xscale=24,aspect="fill",linewidth =c(3,3,5), addLegend = F,legendPosition="center",
            extend= "lightgrey",olapcol="darkgrey", main= "2021 zone activity patterns",ylim=c(0,0.2),
            add.rug = TRUE,  xlab="Time (hs)", ylab="Density", bty = "L", col="darkred",
            lty=1 )

#2021*media
densityPlot(gato2021media_rad,add=T, col="darkgreen", lty=2)

legend ("bottom", inset=c(0,0.8),cex=.7, title="2021 zone activity", c("low (n=2)", "middle (n=53)"),
        lty=1:3, col = c("darkred","darkgreen"), bty="y", horiz = T)


#2018----------------