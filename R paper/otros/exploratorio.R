library(tidyverse)
library(readxl)
library(patchwork)

# cargar tablas----
gatos_exp <- read_excel("E:\\Análisis_R\\IMG\\data/gatos18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa)) %>%
  mutate (tasa = abundancia/esfuerzo)%>%
  filter(zona != "Alta") 
  

atajacaminos <- read_excel("data/atajacaminos18-21.xlsx") %>% 
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))

lagartijas <- read_excel("data/lagartijas18-21.xlsx") %>%
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))

lagartos <- read_excel("data/lagartos18-21.xlsx") %>% 
  rename(abundancia = 1) %>%
  mutate(etapa = factor(etapa))

# Analisis de abundancia de gatos----
gatos_plot <- gatos_exp %>%
  ggplot(aes(x = etapa, 
             y = tasa, 
             col = transecta, 
             group = transecta)) +
  geom_point() +
  geom_path() +
  geom_text(data = data.frame(zona = "Baja",
                              transecta = unique(gatos_exp$transecta[which(gatos_exp$zona == "Baja")]),
                              tasa = seq(0.04,0.06,length.out=5)),
            aes(x = "2021",
                label = transecta)) +
  geom_text(data = data.frame(zona = "Media",
                              transecta = unique(gatos_exp$transecta[which(gatos_exp$zona == "Media")]),
                              tasa = seq(0.04,0.06,length.out=6)),
            aes(x = "2021",
                label = transecta)) +
  facet_wrap(~zona) +
  labs(y = "N° de gatos / Esfuerzo", x = "", title = "Zona") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))
gatos_plot

# Analisis de abundancia de gatos en BAJA----
gatos_plot2 <- gatos_exp %>%
  ggplot(aes(x = etapa, 
             y = abundancia)) +
  geom_point() +
  geom_line(aes(group=abundancia))+

  labs(y = "Cat abundance per unit effort", x = "Year") +
  theme(axis.text=element_text(size=8,color="black", family ="Arial"),
        axis.title=element_text(size=10,color="black", family="Arial"))+
  theme_bw()+
  ylim(0,30)
gatos_plot2

# Analisis de abundancia de lagartos----
lagartos_alta <- lagartos %>% 
  filter(zona == "Alta") %>%
  ggplot(aes(x = etapa, 
             y = abundancia/esfuerzo, 
             col = transecta, 
             group = transecta)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+transecta) +
  labs(y = "", x = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

lagartos_media <- lagartos %>% 
  filter(zona == "Media") %>%
  ggplot(aes(x = etapa, 
             y = abundancia/esfuerzo, 
             col = transecta, 
             group = transecta)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+transecta) +
  theme_bw() +
  labs(y = "N° de lagartos/Esfuerzo", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

lagartos_baja <- lagartos %>% 
  filter(zona == "Baja") %>%
  ggplot(aes(x = etapa, 
             y = abundancia/esfuerzo, 
             col = transecta, 
             group = transecta)) +
  geom_point() +
  geom_path() +
  facet_grid(~zona+transecta) +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

lagartos_plot <- (lagartos_alta +plot_spacer()) / lagartos_media / lagartos_baja

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

# Analisis de abundancia de lagartijas----
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
