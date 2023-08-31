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
 
# Modelos Mixtos----
####Gatos ####

## Cargado de tablas

gatos <- read_excel("E:\\Análisis_R\\IMG\\data/gatos18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa)) %>%
  mutate (tasa = abundancia/esfuerzo)%>%
  filter(zona == "Media")

#Para graficar lo que pasa en zona Baja ---
gatos_exp <- read_excel("E:\\Análisis_R\\IMG\\data/gatos18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa)) %>%
  mutate (tasa = abundancia/esfuerzo)%>%
  filter(zona != "Alta") %>% 
  filter(zona == "Baja")

# Ver datos---
names(gatos_exp)
class(gatos_exp$abundancia)
class(gatos_exp$zona)
class(gatos_exp$transecta)
class(gatos_exp$esfuerzo)
class(gatos_exp$etapa)
#Convertir etapa as character
gatos_exp$etapa <- as.character(gatos_exp$etapa)
class(gatos_exp$etapa)
gatos_exp$abundancia

#### Grafico de gatos en BAJA----
gatos_baja_plot <- gatos_exp %>%
  ggplot(aes(x = etapa, 
             y = abundancia)) +
  geom_point(size=1.5) +
  geom_line(aes(group=abundancia))+
  labs(y = "Cat abundance", x = "Year", tag="",
       title="Low zone")+
  theme(      plot.tag.position = "topleft", 
              plot.title = element_text(size = rel(1.5),  face = "plain", hjust=0), 
              plot.title.position ="panel",
              axis.text=element_text(size=10,color="black"),
              axis.title=element_text(size=12,color="black"),
              panel.background=element_rect(color = "black"),
              strip.background=element_rect(color = "black"))+
         ylim(0,3) #no le saques el limite porque se va a valores negativos
gatos_baja_plot
    
ggsave ("cat_lowzone.png", width=10, height=10, units="cm", dpi=1500)


#### Analisis de abundancia predicha de gatos en Media----

#Ojo que la cosa cambio ahora
#Como no hay zona Alta ni baja NO se agrega al modelo la variable "zona" porque los datos del modelo son de la media unnicamente. No hay variabilidad de la zona!

## Generación del modelo
mod_gatos <- glmer(abundancia ~ etapa + (1|transecta) + offset(log(esfuerzo)),
                   family = "poisson", data = gatos)

check_overdispersion(mod_gatos)
check_model(mod_gatos)
summary(mod_gatos)

library(jtools) #resumen del MODELO con IC
summ(mod_gatos, confint = TRUE, digits = 3) # to drop p-values is p-vals=FALSE

#For exponential family models, especially logit and Poisson, you may be interested in getting the
#exponentiated coefficients rather than the linear estimates. summ can handle that!
summ(mod_gatos, confint=T, exp=T) 

#Bolker para intervalos de confianza
library(glmmTMB)
mod_gatos2 <- glmmTMB(abundancia ~ etapa +(1|transecta) + offset(log(esfuerzo)), data = gatos,
                               family=poisson, na.action = na.fail)
check_overdispersion(mod_gatos2)
summary(mod_gatos2)
confint(mod_gatos2)

AIC(mod_gatos,mod_gatos2) #dan lo mismo. Tiene sentido pq son igual y cambian en el paquete

## Likelihood Ratio Test
# el LRT es un Test de Wald mejorado. Es mejor computacionalmente.  Lo hago para el modelo y ver AIC, pvalor, devianza. 
# Método Backward Selection: Hago un MODELO QUE TENGA TODAS LAS VARIABLES Y LE SAQUO DE A UNA. En este caso lo máximo es con ETAPA y sin ETAPA porque la zona no se modela (sólo hay datos de la zona media)
mod_gatos %>% 
  update(.~.-etapa) %>% #le pido que agarre el modelo de gatos (mod_gatos), mantenga la VR (con el cosito) y le saque la etapa
  anova(mod_gatos, test = "Chisq") # La etapa influye en la devianza

#Ver predichos del modelo
gatos$predichos <- predict(mod_gatos, type= "response") 
gatos$tasa_pred <- gatos$predichos/gatos$esfuerzo


# valores predichos fixed. Con los IC predichos. 
gatos_predict_fixed <- ggpredict(mod_gatos, terms = "etapa", type = "fixed")
gatos_predict_fixed

# valores predichos random. Con los IC crudos. 
gatos_predict_random <- ggpredict(mod_gatos, terms = "etapa", type = "random")
gatos_predict_random


## Plot
windowsFonts()
library(extrafont)
font_import()
loadfonts(device="win")

#EL grafico del modelo donde le pido “fixed” me grafica los valores predictores del modelo.
#En cambio, si le pido que me grafique el crudo “random” ahí me da los valores “verdaderos”.
#Ahí me está incluyendo la incertidumbre generada por la transecta. 

##### Grafico de gatos en MEDIA----
# Random
gatos_plot_random <-
  ggpredict(mod_gatos, terms = "etapa", #calcula predichos
            type = "random") %>% # "fixed" es la incertidumbre de la parte fija de los predichos, si pongo "random" me muestra la variabilidad entre transectas
  ggplot(aes(x = x,
             y = predicted,
             group = group) )+
  geom_point(size=1.5) +
  geom_line()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .15, linewidth=.5)+

  labs(y = "Predicted probabilities of cat sightings",x = "Year", tag = "",
       title="Middle zone", subtitle ="Random effect CI" )+
  theme(plot.tag.position = "topleft", 
        plot.title = element_text(size = rel(1.5), family = "Arial", face = "plain", hjust=0), 
        plot.title.position ="panel",
        axis.text=element_text(size=10,color="black", family ="Arial"),
        axis.title=element_text(size=10,color="black", family="Arial"),
        panel.background=element_rect(color = "black"),
        strip.background=element_rect(color = "black"))

  ylim (0,20) #usar para crudos "random"

gatos_plot_random
ggsave("predicted_cat_sightings_random.png", width=10, height=10, units="cm", dpi=1500)

#Grafico para factores FIJOS
gatos_plot_fixed <-
  ggpredict(mod_gatos, terms = "etapa", #calcula predichos
            type = "fixed") %>% # "fixed" es la incertidumbre de la parte fija de los predichos, si pongo "random" me muestra la variabilidad entre transectas
  ggplot(aes(x = x,
             y = predicted,
             group = group) )+
  geom_point(size=1.5) +
  geom_line()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = .15, linewidth=.5)+
  
  labs(y = "Predicted probabilities of cat sightings",x = "Year", tag = "",
       title="Middle zone", subtitle ="Fixed effect CI" )+
  theme(plot.tag.position = "topleft", 
        plot.title = element_text(size = rel(1.5), family = "Arial", face = "plain", hjust=0), 
        plot.title.position ="panel",
        axis.text=element_text(size=10,color="black", family ="Arial"),
        axis.title=element_text(size=9,color="black", family="Arial"),
        panel.background=element_rect(color = "black"),
        strip.background=element_rect(color = "black"))

ylim (0,20) #usar para crudos "random"

gatos_plot_fixed
ggsave("predicted_cat_sightings_fixed.png", width=10, height=10, units="cm", dpi=1500)

#predicciones
#realiza una predicción (predict) de los valores de una variable de interés llamada "etapa" utilizando un modelo llamado "mod_gatos" y la función ggpredict() del paquete ggeffects.
#El argumento terms = "etapa" indica que se desea realizar la predicción para la variable "etapa". Puede ser que "etapa" represente diferentes etapas o categorías de desarrollo de los gatos.

#fixed
#El argumento type = "fixed" especifica que se deben calcular los valores de predicción para efectos fijos. 
#En el contexto de modelos mixtos o de efectos mixtos, se distingue entre efectos fijos y efectos aleatorios. Los efectos fijos se refieren a las variables predictoras que tienen valores constantes para cada unidad de observación, mientras que los efectos aleatorios representan las diferencias entre las unidades de observación. Al establecer type = "fixed", se están calculando los valores de predicción para los efectos fijos.
mydf_fixed <- ggpredict(mod_gatos, terms = "etapa", type = "fixed")
mydf_fixed

#random
#El argumento type = "random" especifica que se deben calcular los valores de predicción para efectos aleatorios. 
#En el contexto de modelos mixtos o de efectos mixtos, se distingue entre efectos fijos y efectos aleatorios. Los efectos aleatorios representan las diferencias entre las unidades de observación y capturan la variabilidad no explicada por los efectos fijos. Al establecer type = "random", se están calculando los valores de predicción para los efectos aleatorios.
mydf_random <- ggpredict(mod_gatos, terms = "etapa", type = "random")
mydf_random

## Summary

summary(mod_gatos)

## Comparaciones múltiples gatos ----

mod_gatos %>% 
  emmeans(list(pairwise~etapa), type="response" #para escala de variable respuesta es type = "response" y me da en odds
          )
  extract2(2)
  as.data.frame()%>% 
  write_xlsx("data/CM_gatos_etapa.xlsx")
  
# Intervalos de confianza

  emms_gatos <- emmeans(mod_gatos, "etapa", type="response")  #si quiero los IC en odds tengo que hacer type=response
  summary(emms_gatos) #type="response"
  confint(pairs(emms_gatos))
  
# ANOVA
  anova(mod_gatos)
  
  #'[ Duda:]
  #¿Que pasa si presento los resulados en "estimate", es decir, en la escala logaritmica? Los trabajos lo presentan en estimate. 
  #El estimate es el coeficiente beta la pendiente de la recta en escala log. 
  #LO QUE SUELE HACER, ES PRESENTAR LOS RESULTADOS COMO LOG ODD CON EL ESTIMATE Y LOS IC
  
  #'[ Para calcular los valores predichos:]
  #' la función ggpredict() del paquete ggeffects para realizar predicciones y generar gráficos predictivos basados en un modelo, puedes acceder a los valores de predicción utilizando la función predict().
  # Calcular los valores predichos para la variable explicativa "etapa"
  
valor_gatos_plot <- gatos_plot$data
  
  
#'[ Interpretación de OR]
# el ratio indica una relacion entre dos variables. Por ejemplo: un ratio de 2.67 para la abundancia de gatos entre 2018 y 2019 indica que
#la VR aumentó aproximadamente 2.67 veces entre 2018 y 2019. Si tengo un ratio de 0.18, hago la inversa 1/0.18 y digo la relacion de 2019 respecto a 2018. Es decir, que hay 5.55 veces mas de de VR en 2019 respecto a 2018.  
# o puedo decir que la abundancia de gatos en 2019 fue un 57% de la abundancia registrada en 2018. Un ratio menor a 1 es una disminucion de la abundancia. 

#'[ Si quiero graficar la variabilidad entre transectas los tres años]
#'
pd <- position_dodge2(width = 0.7, padding = 1)

ggplot(data=gatos, aes(x=etapa,y=predichos,label=transecta))+
  geom_point(aes(colour=transecta, shape=etapa),size=7, position=pd)+
  geom_line(aes(group=transecta), position=pd,linetype=2, size=0.1,alpha=0.3)+
  geom_text_repel(position=pd,force = 1,size=2, segment.alpha=0)+
  
  labs(title="F.catus",color="Transecta",shape="Etapa",tag = "A",x = "Zona",y="Número de gatos predichos por unidad de esfuerzo")+
  theme_bw() + theme_light () + 
  theme (   plot.tag.position = "topright", 
            plot.tag = element_text(size =20),
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"),
            panel.background = element_rect(fill = "grey90"),
            plot.title = element_text(size=14,face = "italic",hjust = 0.5),
            legend.text = element_text(size=7),
            legend.title=element_text(size=10),
            axis.text.x=element_text(size=10,color="black",face="bold"),
            axis.text.y=element_text(size=10,color="black",face="bold"))



## Gatos con BAJA y MEDIA---
  
## Cargado de tablas
  
  gatos_baja <- read_excel("E:\\Análisis_R\\IMG\\data/gatos18-21.xlsx") %>%
    rename(abundancia = 1) %>%
    mutate(etapa = factor(etapa)) %>%
    mutate (tasa = abundancia/esfuerzo) %>% 
    filter(zona != "Alta")
    
  ## Generación del modelo
  mod_gatosbaja <- glmer(abundancia ~ etapa + zona + (1|transecta) + offset(log(esfuerzo)),
                     family = "poisson",
                     data = gatos_baja)
  summary(mod_gatosbaja)

# Bolker para intervalos de confianza
  library(glmmTMB)
  mod_gatosbaja2 <- glmmTMB(abundancia ~ etapa + zona +(1|transecta) + offset(log(esfuerzo)), data = gatos_baja,
                        family=poisson, na.action = na.fail)
  check_overdispersion(mod_gatosbaja2)
  summary(mod_gatosbaja2)
  confint(mod_gatosbaja2)
  
## Grafico con BAJA y MEDIA -----
  mod_gatosbaja %>% 
    ggpredict(terms = c("etapa","zona"), 
              type = "fixed") %>% # "fixed" es la incertidumbre de la parte fija, si pongo "random" me muestra la variabilidad entre transectas
    ggplot(aes(x = x,
               y = predicted,
               col = group,
               group = group) )+
    geom_point(col="black") +
    geom_line(col="black")+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15,col="black")+
   
    labs(y = "Predicted cat abundance",
         x = "Year",title="", subtitle ="Fixed effect CI" )+
    theme(plot.title = element_text(size = rel(1.5), family = "Arial", face = "plain", hjust=0), 
          plot.title.position ="panel",
          axis.text=element_text(size=10,color="black", family ="Arial"),
          axis.title=element_text(size=12,color="black", family="Arial"),
          panel.background=element_rect(color = "black"),
          strip.background=element_rect(color = "black"))+
    
    
    facet_grid(~group, labeller=labeller(group = c("Media" = "Middle zone", "Baja"= "Low zone")))
  ylim (0,20)
  ggsave("predicted_cat_abundance_lowandmiddle.png", width=15, height=12, units="cm", dpi=1500)

  ## Graficos gatos PAPER merge ----
 
#Combinar los tres gráficos en una sola figura con grid.arrange()
  gatos_plots <- grid.arrange(gatos_baja_plot, gatos_plot_random,gatos_plot_fixed, nrow = 1)
  ggsave("cat_plots_sightings.png",plot=gatos_plots, width=30, height=15, units="cm", dpi=1500)
  
  
# Combinar los tres gráficos en una sola figura con plot_grid()
  gatos_plots <- plot_grid(gatos_baja_plot, gatos_plot_random,
                           gatos_plot_fixed, labels = c("A", "B", "C"), nrow = 1)
  gatos_plots
  ggsave("cat_plots_sightings.png",plot=gatos_plots, width=25, height=15, units="cm", dpi=1500)
  
  ggsave("cat_plots_sightings2.png",plot=gatos_plots, width=25, height=10, units="cm", dpi=1500)
  
  
  
  
  
  
  #### Lagartos -----
## Cargado de tablas

lagartos <- read_excel("E:\\Análisis_R\\IMG\\data/lagartos18-21.xlsx") %>% 
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))

##Generación del modelo

m_sat <- glmer(abundancia ~ etapa*zona + (1|transecta) + offset(log(esfuerzo)),
                      family = "poisson",
                      data = lagartos)
check_overdispersion(m_sat)
summary(m_sat)

## Likelihood Ratio Test

m_sat %>% 
  update(.~. -etapa:zona) %>% 
  anova(m_sat, test = "Chisq") # Quitar la interacción no disminuye signif. la devianza. Se descarta 
m_sat %>% 
  update(.~. -etapa:zona - zona) %>% # La zona influye en la devianza.
  anova(m_sat, test = "Chisq")
m_sat %>% 
  update(.~. -etapa:zona - etapa) %>% # La etapa influye en la devianza.
  anova(m_sat, test = "Chisq")

# Hago test de AIC para estar seguro
#modelos
m_sat <- glmer(abundancia ~ etapa*zona + (1|transecta) + offset(log(esfuerzo)), family = "poisson",
               data = lagartos)
mod_lagartos_interaccion <- glmer(abundancia ~ etapa + zona + (1|transecta) + offset(log(esfuerzo)), family = "poisson",
                      data = lagartos)
mod_lagartos_zona <- glmer(abundancia ~ zona + (1|transecta) + offset(log(esfuerzo)), family = "poisson",
                                  data = lagartos)
mod_lagartos_etapa <- glmer(abundancia ~ etapa + (1|transecta) + offset(log(esfuerzo)), family = "poisson",
                           data = lagartos)

### Comparacion de modelos con AIC

AIC(m_sat,mod_lagartos_interaccion, mod_lagartos_zona, mod_lagartos_etapa)


# Mejor modelo
mod_lagartos <- glmer(abundancia ~ etapa + zona + (1|transecta) + offset(log(esfuerzo)),
                      family = "poisson", data = lagartos)
#Chequeo modelo
check_overdispersion(mod_lagartos)
check_model(mod_lagartos)
summary(mod_lagartos)


## predichos
#Ver predichos del modelo
lagartos$predichos <- predict(mod_lagartos, type= "response") 
lagartos$tasa_pred <- lagartos$predichos/lagartos$esfuerzo

#fixed zona
mydf_fixed_lagartos_zona <- ggpredict(mod_lagartos, terms = "zona", type = "fixed")
mydf_fixed_lagartos_zona
#fixed etapa
mydf_fixed_lagartos_etapa <- ggpredict(mod_lagartos, terms = "etapa", type = "fixed")
mydf_fixed_lagartos_etapa

#random zona
mydf_random_lagartos_zona <- ggpredict(mod_lagartos, terms = "zona", type = "random")
mydf_random_lagartos_zona
#random etapa
mydf_random_lagartos_etapa <- ggpredict(mod_lagartos, terms = "etapa", type = "random")
mydf_random_lagartos_etapa



#Calcular las predicciones 
lagartos_predict <- ggpredict(mod_lagartos,terms = c("etapa","zona"))

# Cambiar el orden de las variables categoricas para graficar sin facet_grid
lagartos_predict$group <- relevel(lagartos_predict$group, ref = "Baja") #cambia el orden de los niveles, donde se establece "Baja" como el nuevo nivel de referencia.
#Cambiar el orden con facet_grid
lagartos_predict$group <- factor(lagartos_predict$group, levels= c("Baja","Media","Alta"))

## Plot
#'[ Graficando la vaiabilidad]
#'NO CORRER, ES SOLO DE EXPLICACION. IR AL GGPLOT DIRECTO
mod_lagartos %>% 
  ggpredict(terms = c("etapa","zona"),
            type = "random")  # "fixed" o "random"

#EL grafico del modelo donde le pido “fixed” me grafica los valores predictores del modelo.
#En cambio, si le pido que me grafique el crudo “random” ahí me da los valores “verdaderos”.
#Ahí me está incluyendo la incertidumbre generada por la transecta. 


  ggplot(lagartos_predict, aes(x = x, #grafica los fixed que son los valores predictores del modelo
             y = predicted, #grafuca los predichos del modelo
             color = group,
             group = group)  )+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2)+
  facet_grid(~group, labeller=labeller(group = c("Alta" = "High", "Media" = "Middle", "Baja"= "Low")))+
  labs(y = "Predicted probabilities of Tegu sightings",
       x = "Year",
       color = "Zone")+
  theme(axis.text=element_text(size=10,color="black", family ="Arial"),
        axis.title=element_text(size=10,color="black", family="Arial"),
        panel.background=element_rect(color = "black"),
        strip.background=element_rect(color = "black"),
        legend.position = "none")

  ggsave("predicted_Tegu_sightings.png", width=16, height=8, units="cm", dpi=1500)
  
## Comparaciones múltiples lagarto ----
## emmeans etapa
mod_lagartos %>% 
  emmeans(list(pairwise~etapa),
          type = "response")
  extract2(2)  
  as.data.frame()%>% 
  write_xlsx("data/CM_lagartos_etapa.xlsx")

#Intervalos de confianza para ETAPA
  emms_lagartos_etapa <- emmeans(mod_lagartos, "etapa", type="response")  #si quiero los IC en odds tengo que hacer type=response
  summary(emms_lagartos_etapa) #type="response"
  confint(pairs(emms_lagartos_etapa))
  
#emmeans ZONA
mod_lagartos %>% 
  emmeans(list(pairwise~zona),
          type = "response")%>%
  extract2(2)  
  as.data.frame()%>% 
  write_xlsx("data/CM_lagartos_zona.xlsx")
  
#Intervalos de confianza para ZONA
  emms_lagartos_zona <- emmeans(mod_lagartos, "zona", type="response")  #si quiero los IC en odds tengo que hacer type=response
  summary(emms_lagartos_zona) #type="response"
  confint(pairs(emms_lagartos_zona))


  ## Plot para lagarto ETAPA - este no va
  
pd <- position_dodge2(width = 0.7, padding = 1)
  
lizard_plot <- ggplot(data=lagartos, aes(x=etapa,y=predichos,label=zona))+
  geom_point(aes(color=zona, shape=etapa),size=7, position=pd)+
  geom_line(aes(group=zona, color=zona), position=pd,linetype=2, size=0.3,alpha=0.6,)+
    labs(title="Black and white Tegu lizard",color="Zone",shape="Year",tag = "A",x = "Zone",
      y="Predicted lizard abundance\nper unit effort")+
    theme_bw() + theme_light () + 
    theme (plot.tag.position = "topright", plot.tag = element_text(size =20),
              panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.background = element_rect(fill = "grey90"),
                axis.line = element_line(size = 0.5, linetype = "solid",colour = "black"),
                  plot.title = element_text(size=14,face = "italic",hjust = 0.5),
                    legend.text = element_text(size=7),legend.title=element_text(size=10),
                      axis.text.x=element_text(size=10,color="black",face="bold"),
                      axis.text.y=element_text(size=10,color="black",face="bold"),
           )
lizard_plot

ggsave ("predicted_lizard_abundance.png",lizard_plot,  width=20, height=20, units="cm", dpi=1200)


## Veamos lagartos por año
lagartos_year <- lagartos %>%
  group_by(etapa) %>%
  summarize(suma_valor = sum(abundancia))
print(lagartos_year)
plot(lagartos_year)

## Veamos lagartos por zona
lagartos_zone <- lagartos %>%
  group_by(zona) %>%
  summarize(suma_valor = sum(abundancia))
print(lagartos_zone)
plot(lagartos_zone)

## Veamos lagartos por zona y año
lagartos_year_zone <- lagartos %>%
  group_by(etapa, zona) %>%
  summarize(suma_valor = sum(abundancia))
print(lagartos_year_zone)

ggplot(lagartos_year_zone, aes(x=etapa,y=suma_valor,label=zona)+
  geom_point(aes(color=zona),size=5)+
    geom_line(aes(group=zona, color=zona), linetype=2, size=0.3,alpha=0.6)+
     geom_text(size=2))

# Ver comparacion de interaccion y grafico

#emmeans ZONA*ETAPA
m_sat %>% 
  emmeans(list(pairwise~zona*etapa),
          type = "response")

#Intervalos de confianza para ZONA
emms_lagartos_interaccion <- emmeans(m_sat, "zona","etapa", type="response")  #si quiero los IC en odds tengo que hacer type=response
summary(emms_lagartos_interaccion) #type="response"
confint(pairs(emms_lagartos_interaccion))

#grafico de emmeans zona + etapa
comparacionesR <- confint(emmeans(mod_lagartos, pairwise ~ zona+etapa, type = "response"))
plot(comparacionesR$emmeans, comparisons = TRUE)


# Modelos Lineales Generalizados No Mixtos ----
#### Lagartija ----

lagartijas <- read_excel("E:\\Análisis_R\\IMG\\data/lagartijas18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa)) %>%
  mutate(tasa = abundancia/esfuerzo)

lagartijas_plot <- lagartijas %>% 
  ggplot(aes(x = etapa, 
             y = abundancia,
             fill = etapa)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  labs(y = "N° de lagartijas", x = "") +
  theme(legend.position = "none")
lagartijas_plot

##Generación del modelo

mod_lagartija1 <- glm(abundancia ~ etapa +  offset(log(esfuerzo)),
               family = "poisson", data = lagartijas)


check_model(mod_lagartija1)
check_overdispersion(mod_lagartija1) # Varianza mayor a la esperada: hay sobredispersion
check_zeroinflation(mod_lagartija1) #Hay mas frecuencia de ceros en los datos que los esperado por Poisson: modelo inflado en zeros
check_autocorrelation(mod_lagartija1)
check_distribution(mod_lagartija1)

#### Modelo sobredispersion ---- 

#'[Poisson inflada en ceros]
#'paquete pscl
mod_zero_lag <- zeroinfl(abundancia ~ etapa +  offset(log(esfuerzo)), data=lagartijas,
                         dist = "poisson")
      # Índice de dispersión de Pearson
      pearson_dispersion <- sum(residuals(mod_zero_lag, type = "pearson")^2) / mod_zero_lag$df.residual
      pearson_dispersion #2.12 como es mayor a 1 hay sobredispersion

#'[Poisson inflada en ceros y sobredispersion]
# Ajusta el modelo zero-inflated con corrección de dispersión
mod_zero_disp_lag <- zeroinfl(abundancia ~ etapa | etapa, data = lagartijas,
                              dist = "negbin", link = "logit")
      # Índice de dispersión de Pearson
      pearson_dispersion2 <- sum(residuals(mod_zero_disp_lag, type = "pearson")^2) / mod_zero_disp_lag$df.residual
      pearson_dispersion2 #1.41 la sobredispersion disminuyo


#'[Binomial negativa'] 
#paquete MASS
mod_lagartija2 <- glm.nb(abundancia ~ etapa +  offset(log(esfuerzo)), data=lagartijas)

#paquete glmmTMB
mod_lagartija3 <- glmmTMB(abundancia ~ etapa,data=lagartijas,
                          family="nbinom2") 

check_overdispersion(mod_lagartija2) #corregida
check_zeroinflation(mod_lagartija2) #corregido
check_model(mod_lagartija2)
check_autocorrelation(mod_lagartija2)
check_distribution(mod_lagartija2)

#'[Binomial negativa'] 
#'AIC
AIC (mod_lagartija1,mod_lagartija2,mod_zero_lag,mod_zero_disp_lag) #Possion, bn, poisson zero inflado,bn zero inflado

# Mejor modelo ----
mod_lagartija <- glm.nb(abundancia ~ etapa +  offset(log(esfuerzo)), 
                        data=lagartijas)

summary(mod_lagartija)

## predichos
#Ver predichos del modelo
lagartijas$predichos <- predict(mod_lagartija, type= "response") 
lagartijastasa_pred <- lagartijas$predichos/lagartijas$esfuerzo

# valores predichos fixed 
lagartija_predict <- ggpredict(mod_lagartija, terms = "etapa", type = "fixed")
lagartija_predict


#Grafico lagartijas paper ----
lagartija_plot <-
  ggplot(lagartija_predict, aes(x = x, #grafica los fixed que son los valores predictores del modelo
                             y = predicted #predichos del modelo
                               ))+
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2)+
  labs(y =expression(paste("Predicted probabilities of ",italic(" L. wiegmannii"), "sightings")),
       x = "Year", tag="")+
  theme(
        plot.tag.position = "topleft",plot.tag = element_text(size =15),
        axis.text=element_text(size=10,color="black"),
        axis.title=element_text(size=9,color="black"),
        panel.background=element_rect(color = "black"),
        strip.background=element_rect(color = "black")  )
lagartija_plot
ggsave("predicted_L.wiegmannii_sightings.png", plot=lagartija_plot,
       width=10, height=10, units="cm", dpi=1500)

## Comparaciones múltiples de lagartija -----
## emmeans
mod_lagartija %>% 
  emmeans(list(pairwise~etapa), type = "response")
extract2(2)  
as.data.frame()%>% 
  write_xlsx("data/CM_lagartija_etapa.xlsx")

#Intervalos de confianza
emms_lagartija <- emmeans(mod_lagartija, "etapa", type="response")  #si quiero los IC en odds tengo que hacer type=response
summary(emms_lagartija) #type="response"
confint(pairs(emms_lagartija))

#### atajacaminos ----
atajacaminos <- read_excel("E:\\Análisis_R\\IMG\\data/atajacaminos18-21.xlsx") %>% 
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))
        
# Analisis de abundancia de atajacaminos----
atajacaminos_plot <- atajacaminos %>% 
  ggplot(aes(x = etapa, 
             y = abundancia,
             fill = etapa)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  labs(y = "N° de atajacaminos", x = "") +
  theme(legend.position = "none")

atajacaminos_plot

##Generación del modelo

mod_aj <- glm(abundancia ~ etapa +  offset(log(esfuerzo)),
                      family = "poisson", data = atajacaminos)


check_model(mod_aj)
check_overdispersion(mod_aj) #ok
check_zeroinflation(mod_aj) #ok
check_autocorrelation(mod_aj) #ok
check_distribution(mod_aj)  #no le des bola                  

summary(mod_aj)

## predichos
#Ver predichos del modelo
atajacaminos$predichos <- predict(mod_aj, type= "response") 
atajacaminostasa_pred <- atajacaminos$predichos/atajacaminos$esfuerzo

# valores predichos fixed 
atajacaminos_predict <- ggpredict(mod_aj, terms = "etapa", type = "fixed")
atajacaminos_predict


#Grafico atajacaminos paper ----
atajacaminos_plot <- 
  ggplot(atajacaminos_predict, aes(x = x, #grafica los fixed que son los valores predictores del modelo
                              y = predicted #predichos del modelo
                              ))+
  geom_point(size=1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2)+
  labs(y =expression(paste("Predicted probabilities of ",italic(" Caprimulgus ")~ " spp. sightings")),
       x = "Year", tag="")+
  theme(
        plot.tag.position = "topleft",plot.tag = element_text(size =15),
        axis.text=element_text(size=10,color="black"),
        axis.title=element_text(size=9,color="black"),
        panel.background=element_rect(color = "black"),
        strip.background=element_rect(color = "black")  )
atajacaminos_plot
ggsave("predicted_nightjars_sightings.png", plot = atajacaminos_plot,
       width=10, height=10, units="cm", dpi=1500)

## Comparaciones múltiples de atajacaminos -----
## emmeans
mod_aj %>% 
  emmeans(list(pairwise~etapa), type = "response")
extract2(2)  
as.data.frame()%>% 
  write_xlsx("data/CM_atajacaminos_etapa.xlsx")

#Intervalos de confianza
emms_aj <- emmeans(mod_aj, "etapa", type="response")  #si quiero los IC en odds tengo que hacer type=response
summary(emms_aj) #type="response"
confint(pairs(emms_aj))

## Graficos fauna GLM plot ----


#Combinar dos gráficos en una sola figura con grid.arrange()
GLM_plots_sightings <- grid.arrange(lagartija_plot, atajacaminos_plot, nrow = 1)

ggsave("GLM_plots_sighting.png",plot=GLM_plots_sightings, width=30, height=15, units="cm", dpi=1500)

# Combinar los tres gráficos en una sola figura con plot_grid()
GLM_plots_sightings <- plot_grid(lagartija_plot, atajacaminos_plot, labels = c("A", "B"), nrow = 1)
GLM_plots_sightings
ggsave("GLM_plots_sighting.png",plot=GLM_plots_sightings, width=20, height=10, units="cm", dpi=1500)

