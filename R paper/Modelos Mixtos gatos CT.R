library(tidyverse)
library(readxl)
library(lme4)
library(ggeffects)
library(performance)
library(emmeans) #comparaciones multiples e IC
library(writexl)
library(magrittr) #para pipe %>%
library(MuMIn) #comparacion de modelos con AIC
library(glmmTMB) #para modelado
library(MASS) #para binomial negativa en glm
library(pscl) #para correro Poisson con cero inflado
library(gridExtra) #fusion de graficos
library(cowplot) #fusion de graficos
library(patchwork)

# Modelos Mixtos CT Gatos----

## Cargado de tablas

gatos_CT <- read.csv("E:/Análisis_R/IMG/data/eventosgatos18-21.csv", 
                     header=T,sep=";",stringsAsFactors=FALSE)%>%
          mutate(etapa = factor(etapa)) %>%
          mutate(tasa = eventos/esfuerzo)

#Para graficar lo que pasa sin zona alta ---
gatos_expCT <- read.csv("E:/Análisis_R/IMG/data/eventosgatos18-21.csv", 
                     header=T,sep=";",stringsAsFactors=FALSE)%>%
  mutate(etapa = factor(etapa)) %>%
  mutate(tasa = eventos/esfuerzo) %>%
  filter(zona != "alta")


  
# Analisis de abundancia de gatos CT----
 
gatos_CTalta <- gatos_CT %>% 
  filter(zona == "alta") %>%
  ggplot(aes(x = etapa, 
             y = eventos/esfuerzo, 
             col = station, 
             group = station)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+station) +
  labs(y = "", x = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

gatos_CTmedia <- gatos_CT %>% 
  filter(zona == "media") %>%
  ggplot(aes(x = etapa, 
             y = eventos/esfuerzo, 
             col = station, 
             group = station)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+station) +
  theme_bw() +
  labs(y = "N° de eventos de gatos/Esfuerzo", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

gatos_CTbaja <- gatos_CT %>% 
  filter(zona == "baja") %>%
  ggplot(aes(x = etapa, 
             y = eventos/esfuerzo, 
             col = station, 
             group = station)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+station) +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

gatosCT_plot <- (gatos_CTalta + plot_spacer()) / gatos_CTmedia / gatos_CTbaja

gatos_CTalta
gatos_CTmedia
gatos_CTbaja

gatosCT_plot

## Modelando CT ------
## Generación del modelo

gatosB_CT <- read.csv("E:/Análisis_R/IMG/data/eventosgatosB18-21.csv",  #dataframe con alta y 8 estaciones de 2021
                     header=T,sep=";",stringsAsFactors=FALSE)%>%
  mutate(etapa = factor(etapa)) %>%
  mutate(tasa = eventos/esfuerzo)
  filter(zona != "alta") #para modelar sin la zona alta


m_satB <- glmer(eventos ~ etapa*zona + (1|station) + offset(log(esfuerzo)),
               family = "poisson",
               data = gatosB_CT)

check_model(m_satB)
check_overdispersion(m_satB)
check_zeroinflation(m_satB) #Hay mas frecuencia de ceros en los datos que los esperado por Poisson: modelo inflado en zeros
check_autocorrelation(m_satB)
check_distribution(m_satB)
summary(m_satB)


#hay sobredispersion, correlacion, zeroinflado...todo...se recomienda una binomial negativa inflada en zero

#sin zona alta ny 8 estadcion de 2021
m_satB_CT_x <- glmer(eventos ~ etapa*zona + (1|station) + offset(log(esfuerzo)),
                family = "poisson",
                data = gatosB_CT_x)

check_model(m_satB_CT_x)
check_overdispersion(m_satB_CT_x)
check_zeroinflation(m_satB_CT_x) #Hay mas frecuencia de ceros en los datos que los esperado por Poisson: modelo inflado en zeros
check_autocorrelation(m_satB_CT_x)
check_distribution(m_satB_CT_x)
summary(m_satB_CT_x)

#modelos para 2018 vs 2019 camaras trampa
gatos_CT1819 <- read.csv("E:/Análisis_R/IMG/data/eventosgatosB18-21.csv",  #dataframe con alta y 8 estaciones de 2021
                      header=T,sep=";",stringsAsFactors=FALSE)%>%
  mutate(etapa = factor(etapa)) %>%
  mutate(tasa = eventos/esfuerzo) %>%
filter(etapa != "2021") #para modelar sin etapa 2021
  
#sin etapa 2021
m_sat <- glmer(eventos ~ etapa*zona + (1|station) + offset(log(esfuerzo)),
                     family = "poisson",
                     data = gatos_CT1819)

check_model(m_sat)
check_overdispersion(m_sat)
check_zeroinflation(m_sat) #Hay mas frecuencia de ceros en los datos que los esperado por Poisson: modelo inflado en zeros
check_autocorrelation(m_sat)
check_distribution(m_sat)
summary(m_sat)

#sin 2021, da con sobredispersion y autocorrelacion de variables