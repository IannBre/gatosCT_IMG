library(tidyverse)
library(readxl)
library(lme4)
library(ggeffects)
library(performance)
library(emmeans)
library(writexl)
library(magrittr)



# Gatos -----

## Cargado de tablas

gatos <- read_excel("data/gatos18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa)) %>%
  mutate (tasa = abundancia/esfuerzo)%>%
  filter(zona == "Media")


## Generación del modelo
mod_gatos <- glmer(abundancia ~ etapa + (1|transecta) + offset(log(esfuerzo)),
                   family = "poisson",
                          data = gatos)
check_overdispersion(mod_gatos)
check_model(mod_gatos)

## Likelihood Ratio Test

mod_gatos %>% 
  update(.~.-etapa) %>% 
  anova(mod_gatos, test = "Chisq") # La etapa influye en la devianza

## Plot
mod_gatos %>% 
  ggpredict(terms = "etapa", 
            type = "fixed") %>%
  ggplot(aes(x = x,
             y = predicted,
             group = group) 
         )+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15)+
  labs(y = "Abundancia de gatos predicha\npor unidad de esfuerzo",
       x = "Etapa")+
  theme_bw()

## Comparaciones múltiples

mod_gatos %>% 
  emmeans(list(pairwise~etapa),
          type = "response")%>%
  extract2(2) %>% 
  as.data.frame()%>% 
  write_xlsx("data/CM_gatos_etapa.xlsx")


# Lagartos -----

## Cargado de tablas

lagartos <- read_excel("data/lagartos18-21.xlsx") %>% 
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))

##Generación del modelo

m_sat <- glmer(abundancia ~ etapa*zona + (1|transecta) + offset(log(esfuerzo)),
                      family = "poisson",
                      data = lagartos)
check_overdispersion(m_sat)

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

mod_lagartos <- glmer(abundancia ~ etapa + zona + (1|transecta) + offset(log(esfuerzo)),
               family = "poisson",
               data = lagartos)
check_overdispersion(mod_lagartos)
check_model(mod_lagartos)

## Plot

mod_lagartos %>% 
  ggpredict(terms = c("etapa","zona"),
            type = "fixed") %>%
  ggplot(aes(x = x,
             y = predicted,
             color = group,
             group = group) 
  )+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2)+
  facet_grid(~group)+
  labs(y = "Abundancia de lagartos predicha\npor unidad de esfuerzo",
       x = "Etapa",
       color = "Zona")+
  theme_bw()

## Comparaciones múltiples

mod_lagartos %>% 
  emmeans(list(pairwise~etapa),
          type = "response")%>%
  extract2(2) %>% 
  as.data.frame()%>% 
  write_xlsx("data/CM_lagartos_etapa.xlsx")

mod_lagartos %>% 
  emmeans(list(pairwise~zona),
          type = "response")%>%
  extract2(2) %>% 
  as.data.frame()%>% 
  write_xlsx("data/CM_lagartos_zona.xlsx")

