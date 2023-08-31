
#1) ATAJOS ----
#'[] 
#'[ Antes que nada, atajos de R:]
#'[ Run todo el documento: CTRL + ALT + E]
#'[ Flechita de asignacion: CTRL + guion medio]
#'[ PIPE: CTRL + SHIFT + M hay que tener tidyverse]
#'[ Fijar el escritorio de trabajo: CTRL + SHIFT + H]


#2) DIRECTORIO Y LIBRERIA DE PAQUETES ----

setwd("E:\\Análisis_R\\IMG")
choose.dir()

conda install -c r r-xlsx
library(xlsx)
library(readxl) #lee directo en formato xslx
library(tidyverse) #hace de todo, necesito para %>%
library(Matrix) #lo pide para lme4
library(lme4)   #GLMM
library(MuMIn)  #AIC.Para ranking de modelos y determinar cual es el mejor para explicar los resultados
library(ggeffects) #para GLM graficos
library(performance) #para analisis de GLM (chequear sobredispersion, etc)
library(emmeans) #comparacion de grupos en modelos GLM
library(ggplot2) #para graficar con ggplot
library(forcats) #para cambiar orden pre y post en summary
library(ggrepel) #para alinear texto dentro de graficos
library(magrittr)#intervalos de confianza en ggplot2 NOLO RESOLVI
library(see) #para correr el check model grafico predictivo
library(patchwork) #para correr el check model graficos complementarios
library(glmmADMB) #para modelos lineales mixtos

#3)TEORÍA: Modelos lineales------
#'[Antes de arrancar: ¿Que tenes que saber?]-------

#'[Modelos Lineales GENERALES]: OJO! NO son GENERALIZADOS. Tienen VR con distribucion normal estiman parametros por cuadrados minimos / nmaxima verosimilitud
#usan paquete lme4 con funcion lmer o paquete nlme con funcion lme
#'[Paquete library(lme4)][Funcion lmer] #Este paquete no informa los grados de libertad ni el p valor, solo informa el estadistico t.
library(lmerTest) #Prueba de Wald.Es necesario correr la libreria lmerTest para obtener los p-valores
#'[Paquete library(nlme)][Funcion lme]:si los GL residualos son pequeños (<50) los p valores no son confiables


#'[Modelos Lineales Generalizados]:
#'[Análisis de devianza]: deviance(modelo) 

#'[Modelos Lineales Generalizados Mixtos]:

#'[Análisis de devianza en GLMM]
#'usa máxima solicitud restringida. 
#'[Significancia de GLMM]:
#'[Para significancia con más de 2 niveles por tratamiento]:Note that
#'if every factor of your glmer model only has two levels, the p-values in Anova
#'results (type III Wald chisquare test) will be the same as the Wald Z-test in the summary of your glmer model.
# usando la funcion drop o anova o mixed

#'[P valores en GLMM]: The lme4::lmer function doesn’t provide p-values for coefficients. 
#'The lme4::glmer provides p-values for coefficients (but not for factors/effects) using the Wald Z-test, 
#'which is not a very good method. So for lmer models, or glmer models which have factor(s) with more than 2 level, 
#'we need some methods to obtain the p-values for the effects/variables. We can use the car::Anova or other packages (e.g., pbkrtest , afex)
#'to obtain p-values. 

#### 3.1) No hay convergencia de datos, ¿que hago? ####
#'[Cuando NO convergen los datos, ¿Porque no convergen los datos?]
#'Son problemas de cálculo. Problemas de estimar los efectos aleatorios. En GLM
#'la expresion para maximizar la verosimilitud de un modelo es una integral que se puede evaluar exactamente.
#'EN los GLMM, esa integral es sobre el espacio de efectos aleatorios. En este caso
#'la integral debe ser aproximada, es mucho mas arduo y compuacionalmente exigente por lo que 
#'puede no alcanzarse la convergencia. Cuanas mas variables de efectos aleatorios hayan en el modelo 
#'y cuantos mas niveles tengan, mas se va a complicar la convergencia
#'
#'[¿Como tratar la no convergencia de datos?]
#'Hay distintos métodos de estimacion y distintos paquetes. Se pueden variar estos metodos. Para que converga hay que tener una cierta cantidad limitada de repticiones, etc.
#' #'[1) Cuadratura de Gauss-Hermite (QGH):] glmer (R:lme4,lme4a) o glmmML (R:glmmML). Consiste en aproximar la integral
#' con el área de un cierto número de rectángulos (o prismas) estratégicamente ubicados.
#' Se puede aumentar el número de puntos de cuadratura (mayor precision en las estimaciones)
#' [2) Aproximacion de Laplace:] Es un caso particular de (QGH), menos preciso pero más rapido. glmer (R:lme4,lme4a) y glmmTMB
#' [3) Cuasi-verosimilitud penalizada (PQL):] sesgo para Poisson y binomial si la media es menor a 5. glmmPQL (R: MASS)
#' [4) Algoritmo Monte Carlo Cadenas de Markov (MCMC):] Método bayesiano. Varios paquetes: MCMCglmm (R:MCMCglmm) y mas

#### 3.2) Supuestos de un modelo y su corroboracion ####

#'[1) Independencia entre las observaciones]
#'No debe existir correalación entre observaciones (no se cumple para medidads repetidas)
#'En los GLMM no se cumple y ahi tenemos que introducir el factor aleatorio
#'[2) Normalidad]
#'Para cada nivel de X, la variable Y tiene una distribución normal. Hago QQplot y Prueba de Shapiro Wilk. 
#'[3) Dispersion]
#'Podemos correr el performance, Dharma o hacerlo manual. Se ajusta a otra distribución si da sobre o subdispersion.  
#'[4) Homocedasticidad de la varianza]
#'Las varianzas de las subpoblaciones son iguales. Grafico de dispersion de residuos estandarizados vs predichos.
#Pruebe de Levene. Si p-valor es mayor a 0,05, varianza homogenea. Si es menor, hay heterocedasticidad.
#'Si no se cumple el supuesto, se puede modelar la varianza. 
#'No hay mocedasticidad en Poisson pero var y media deben ser iguales
#'[5) Linealidad]
#'Para Variable Explicatoria CUANTITATIVA. Grafico dispersion de residuos est. vs predichos. 

#'[Con check model, ¿Que gráficos me importa ver?: 
#'El observados vs predichos (density vs gatos) donde deben tener un buen ajuste
#'En el Influential Obs veo outliers: en este caso me los muestra en rojo con el numero de fila
#'Normalidad de residuos: es el QQplot de normalidad. Si se desvia mucho, rechazo modelo
#'Homogeneidad de varianza: buscamos una linea lo mas horizontal posible pero Poisson o Bnegativa no predicen orizontalidad de la varianza. Permiten heterocedasticidad. ]
#'[Dispersion de los datos]
#'[¿Que pasa cuando tengo varias opciones para un mismo tipo de variable respuesta?]
#'[Bueno, ahi tenemos que decir que distribucion se ajusta mejor a mis datos. Para]
#'[una variable de conteo voy a elegir:]
#'[  - DISTRIBUCION POISSON: si NO hay sobredispersion de datos]
#'[  - DISTRIBUCION BINOMIAL NEGATIVA: si los datos esta sobredispersos para una Poisson]

#### 3.3) Variabilidad explicada por el modelo: devianza ####

#'[Análisis de devianza en GLM]
#'Deviance is a measure of error; lower deviance means better fit to data.
#Cuando voy construyendo un modelo digo que cada uno de los predictores que aplico
#me explican algo de la variabilidad total. La devianza es la VARIABILIDAD NO EXPLICADA
#por mis predictores. Puede haber otros factores en juego y también juega el propio azar 
#en esa devianza. La Residual deviance es la devianza, la Null deviance es la del modelo nulo

#'[El valor que da deviance es el que no se puede explicar]
#'entonces tengo que hacer deviance - 100 y eso es lo que si se explica
deviance(AJm1) #es la Residual= 13.69422

#4) CARGA DE DATOS ----------
#'[Carga de datos que voy a analizar]
 #### 4.1) Datos GATOS ####

#'[Base de datos]
# cargar archivo de datos .csv o xlsx
gatos <- read.csv("E:\\Análisis_R\\IMG\\data\\gatos18-21.csv", header = TRUE , sep = ";")

#Controlo el tipo de datos
#'[Siempre corroborar la clase de datos que tenemos por si hay que convertir]
class(gatos$gatos)
class(gatos$zona)
class(gatos$transecta)
class(gatos$etapa)
class(gatos$transecta_num)
class(gatos$transecta_name_nro)

#Convertir etapa as character
gatos$etapa <- as.character(gatos$etapa)
class(gatos$etapa)
gatos$transecta_num <- as.character(gatos$transecta_num)
class(gatos$transecta_num)

#Sólo me interesa lo que pasa en zona baja y media
#'[Selecciono filas de interes]
gatos <- gatos[gatos$zona=="Media"| gatos$zona=="Baja",]

#5) ANALISIS EXPLORATORIO ----
#'[Analizamos datos gatos]
 #### 5.1) Exploracion GATOS####

#Grafico de cajas
gatos %>% #paip: le digo a ggplot que use la tabla gatos
  ggplot(aes(y = gatos, x = etapa, fill= etapa)) + ##entre dos variables cuantitativas lo mejor es geompoint y con cuali geobox
  geom_boxplot() #Any outliers that we plot are simply values that are more extreme than those calculated minima and maxima

#Grafico de puntos por etapa pareando zonas
plot1 <- ggplot(data=gatos, aes(x=zona, y=gatos, colour=transecta))+
  geom_point(size=2)+
facet_grid (. ~ etapa) + labs(y="gatos avistados", x="zona de la isla")
plot1

#Grafico de puntos por zona pareando etapa
plot2 <- ggplot(data=gatos, aes(x=etapa, y=gatos, colour=transecta))+
  geom_point(size=2) +
facet_grid (. ~ zona) + labs(y="gatos avistados", x="año")
plot2

#Regresion Multiple: Para más de una Variable Explicatoria de tipo cualitativa - CORRELACION Y COLINEALIDAD
library(GGally)
ggpairs(gatos)

#6) MODELOS ----------
#'[Modelar datos: construir modelos con predictor CUALITATIVO]
#'[GLMM gatos]
#'[Como la variable respuesta es una variable de CONTEO, voy a construir modelos con distribucion Poisson]


#### 6.1) Modelando Gatos con ZONA + ETAPA (fijos) y TRANSECTA (aleatorio) ####

Gatos01 <- glmer(gatos~ etapa + zona +(1|transecta), data = gatos, #Warning en convergencia
                   family = "poisson", na.action = na.fail)

summary(Gatos01)
check_overdispersion(Gatos01)
check_zeroinflation(Gatos01)
check_model(Gatos01)


#Probar con zero inflado y binomial negativa
install.packages("R2admb")
install.packages("devtools")
devtools::install_github('bbolker/glmmadmb') #da unos warnings

install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source") #este instala bien

library(glmmADMB)
help ("glmmADMB")

#In glmmADMB all grouping variables in random effects must be factors
class(gatos$transecta)
#Convertir transecta as factor
gatos$transecta <- as.factor(gatos$transecta)
class(gatos$transecta)

#Correr modelos de glmmADMB con variable aleatorio como factor
Gatos02 <- glmmadmb(gatos ~ etapa + zona +(1|transecta), 
                     family = "poisson",zeroInflation=TRUE, data=gatos)

Gatos03 <- glmmadmb(gatos ~ etapa + zona +(1|transecta),family="nbinom", data=gatos)

summary(Gatos02)
summary(Gatos03)
summary(Gatos01)

library(performance)
check_overdispersion(Gatos02) #no se puede aplicar
check_zeroinflation(Gatos02)#ok
check_overdispersion(Gatos03)#no se puede aplicar
check_zeroinflation(Gatos03)#ok

AIC(Gatos01,Gatos02,Gatos03) #porque da mejor Gatos01 si da warning en convergencia?

#Plantear modelos con interaccion de etapa*zona

Gatos04 <- glmer(gatos ~ etapa*zona +(1|transecta), 
                    family = "poisson", data=gatos, na.action = na.fail)

summary(Gatos04)


#'[Seleccion de modelos]
AIC(Gatos01,Gatos02,Gatos03,Gatos04)
#rankeo automatico
library(MuMIn)
library(glmmTMB)
#con poisson
dredge(glmmTMB(gatos ~ etapa*zona +(1|transecta), family="poisson", data=gatos, na.action = na.fail))

#con bnegativa
dredge(glmmTMB(gatos ~ etapa*zona +(1|transecta), family="nbinom2", data=gatos, na.action = na.fail))


#'[Conclusion: Modelos Gatos01 lo pruebo con otros paquetes ya que fue el mejkor por AIC y dredge]

#Por AIC y Dredge:
Gatos06 <- glmmTMB(gatos~ etapa + zona +(1|transecta), data = gatos, 
                 family = "poisson", na.action = na.fail)
summary(Gatos06)
check_overdispersion(Gatos06)
check_zeroinflation(Gatos06)
#El error del intercept y la zonamedia es ENORME


#'[Seleccion de modelos]
AIC(Gatos01,Gatos06) #dan iguales

Gatos07 <- glmmadmb(gatos~ etapa + zona +(1|transecta), data = gatos, 
                   family = "poisson") #da error


#'[CONCLUSION]
AIC(Gatos01,Gatos06) #son los mejores y dan iguales
#Gatos01 tira un warning de convergencia y Gatos06 tiene el Error Standard muy alto en dos casos




#6.bis) MODELANDO un MEDIDAS REPETIDAS CON FACTOR FIJO Y FACTOR ALEATORIO ANIDADO ----

#'[Nested random effects:] occur when a lower level factor appears 
#'only within a particular level of an upper level factor]
#'[For example, pupils within classes at a fixed point in time.] (1|zona/transecta)


#'[Crossed random effects:] means that a given factor appears in more
#'than one level of the upper level factor]
#'[For example, there are pupils within classes measured over several years.]


library(lme4)


Gatos08<-glmer(gatos~etapa + zona + (1|zona/transecta), data=gatos, family="poisson") #esto esta mal porque me toma las dos como aleatorias
#este no porque toma a zona como fijo y aleatorio
summary(Gatos08)

Gatos09 <- glmmTMB (gatos~etapa + zona + (1|zona/transecta), data=gatos, family="poisson",
                    na.action = na.fail) 
#este no porque toma a zona como fijo y aleatorio

Gatos10 <- glmmTMB (gatos~etapa + zona + (zona|transecta), data=gatos, family="poisson",
                    na.action = na.fail) 
#este toma a zona como fijo y transecta como aleatorio anidado en zona
Gatos11<-glmer(gatos~etapa + zona + (zona|transecta), data=gatos, family="poisson",
               na.action = na.fail)
#este toma a zona como fijo y transecta como aleatorio anidado en zona


summary(Gatos06)
summary(Gatos09)
summary(Gatos11)
  
AIC(Gatos06,Gatos08,Gatos09,Gatos10,Gatos11,Gatos12)  

#comparacion multiple
emmeans(Gatos11, pairwise ~ etapa + zona, type="response")


#### 6.2) Modelando GATOS con ZONA (fijo) y etapa (aleatorio) ####
#Modelando con GLM mixto. Voy a analizar si hay diferencias entre ZONAS
Gatos0 <- glmer(gatos ~ zona+(1|etapa), data = gatos, family = "poisson", 
                     na.action = na.fail)

#'[Interpretación]: gatos como variable númerica, zona como factor variable
#etapa como factor fijo, na.action para que indique en los casos que faltan valores. 

#'[Analisis de Gatos0] El modelo no converge. No resulta confiable. 
#'Lo vemos en el summary
summary(Gatos0)

#'[Interpretación]: el valor estimate compara zona baja vs media. Un valor 
#'mayor representa mayor probabilidad de encontrar gatos en dicha zona. Pero como el ERROR es muuuuy grande,
#'el modelo no resulta confiable, no permite el analisis por zona. 
 
#Modelo que no convergió: usa por defecto 1 punto de integracion
#'[Cuadratura de Gauss-Hermite (QGH): glmer y/o glmmML]
library(lme4)
Gatos0 <- glmer(gatos ~ zona+(1|etapa), data = gatos,family = "poisson", 
                     na.action = na.fail)
summary(Gatos0) #no converge

#'[Modelos que convergen]

#Modelo con Laplace: modelo converge pero el ES es enorme
#'[Aproximacion de Laplace: glmer y/o glmmTMB]
library(glmmTMB) #paquete de Bolker. No me tira devianza
Gatos1 <- glmmTMB(gatos ~ zona+(1|etapa), family="poisson", data = gatos)
summary(Gatos1)

#Usamos cuadratura de Gauss-Hermite con 4 o 5 puntos de integracion: modelo converge con ES grande
#'[Cuadratura de Gauss-Hermite (QGH): glmer y/o glmmML]
Gatos2<- glmer(gatos ~ zona+(1|etapa), data = gatos,
               family = "poisson",na.action = na.fail,nAGQ = 4) #con 5 da mas error Std.

#'[Significancia del efecto fijo en GLMM]
summary(Gatos2)#como son dos niveles de trat (bajo, medio), 
#la prueba del summary es valida.Si fueran mas de 2, no. Debeia usarse anova
confint(Gatos2) #IC

#'[Para significancia de GLMM con más de 2 niveles por tratamiento]:Note that
#'if every factor of your glmer model only has two levels, the p-values in Anova
#'results (type III Wald chisquare test) will be the same as the Wald Z-test in the summary of your glmer model.

#'[The Likelihood Ratio Test (LRT) is generally preferred over Wald tests of fixed effects in mixed models]
#There are several R functions which can be used for the LRT. Two of these are drop1() and anova().
library(car)
anova(Gatos2) #lo uso cuando tengo factores con mas de dos niveles y quiero ver el p-valor
library(lme4)
drop1(Gatos2, test="Chisq") #This requires the test parameter be set to "Chisq"

#'[Modelos que restan probar pero no son los más utilizados]
#Probar con PQL
#'[Cuasi-verosimilitud penalizada: glmmPQL (Rpaquete MASS)]
library(MASS) #paquete para glmmPOL
#Probar con MCMC
library(MCMCglmm) #bayesiano 


#'[Seleccion de modelos]
AIC(Gatos1,Gatos2)

####6.2.1) Remodelado de GATOS por sobredispersion ####
#'[Corroboro supuestos]
check_model(Gatos2)
check_overdispersion(Gatos2) #hay SOBREDISPERSION

#'[Modelo sobredispersion con Binomial Negativa]: uso lme4 (glmer.nb); glmmTMB (family="nbinom2")
#' o libreria MAS (que es exclusiva para binomial negativa (glm.nb) 
#'[Cuadratura de Gauss-Hermite (QGH): glmer y/o glmmML]

library(lme4) #da error. Revisar sintaxis para Mixtos
Gatos3 <- glmer.nb(gatos ~ zona+(1|etapa), data = gatos) #es binomial negativa

library (MASS) #da error. Revisar sintaxis para Mixtos
Gatos4 <- glm.nb(gatos ~ zona+(1|etapa), data = gatos) #es binomial negativa




library(glmmTMB) #corre bien
Gatos5 <- glmmTMB(gatos ~ zona+(1|etapa),family="nbinom2", data=gatos) #es binomial negativa

summary(Gatos5)
confint(Gatos5)
library(lme4)
drop1(Gatos5, test="Chisq")

#Pruebo con poisson y zero inflado
gatos$etapa <- as.factor(gatos$etapa)
class(gatos$etapa)
Gatos5bis <- glmmadmb(gatos ~ zona+(1|etapa),family="poisson",zeroInflation = TRUE, data=gatos) #poisson con zero inflado

summary(Gatos5bis)
AIC (Gatos5, Gatos5bis)

####6.2.2) Supuestos de modelo GATOS zona####
#'[Corroboro supuestos: dispersion.
#'[Normalidad, homocedasticidad y linealidad con residuos]
check_model(Gatos5)
check_overdispersion(Gatos5)

#Residuos
residuoscrudos=residuals(Gatos5)
residuoscrudos
residuosstd <- rstandard(Gatos5) #no se puede calcular con glmmTMB
residuosstd
#evaluemos graficamente los supuestos
e<-resid(Gatos5) # residuos
re<-rstandard(Gatos5) #residuos estandarizados
recrudos <-residuals(Gatos5) #residuos crudos
pre<-predict(Gatos5) #predichos
plot(pre, recrudos, xlab="Predichos", ylab="Residuos crudos",main="RE vs PRED - Modelo 4" )
abline(0,0)
abline(h = -2, v = 0, col = "red", lty = 3)
abline(h = 2, v = 0, col = "red", lty = 3)

#QQPlot para normalidad
library(car)
qqPlot(residuoscrudos)
shapiro.test(e) #NORMALIDAD analitica
#Homocedasticidad (solo con CUALITATIVAS)

### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(Gatos5)
leveneTest(gatos ~ zona+(1|etapa),family="nbinom2", data = gatos) 

#'[Variabilidad NO explicada por el modelo]
deviance(Gatos5) #Ver si está bien usar deviance en GLMM

####6.2.3) Seleccion de mejor modelo GATOS zona####

#'[Seleccion de modelos]
AIC(Gatos0,Gatos1,Gatos2,Gatos5)
#rankeo automatico
library(MuMIn)
dredge(glmmTMB(gatos ~ zona+(1|etapa), family="nbinom2", data=gatos, na.action = "na.fail"))


#### 6) Modelo Zero Inflado #######

check_zeroinflation(Gatos5) #binomial negativa
check_zeroinflation(Gatos2) #poisson


summary(Gatos5)

install.packages("R2admb")
install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos", getOption("repos")), type="source")

library(glmmADMB)

####6.2.4) Comparacion de variables del modelo GATOS zona ####
#'[Comparacion múltiple]
library(emmeans)
emmeans(Gatos5, list(pairwise ~ zona),type = "response") 
#'[Otro paquete para comparaciones múltiples]
library(multcompView)
library(multcomp)
multCompTukey <- emmeans(Gatos5, pairwise ~ zona)
summary(multCompTukey)
confint(multCompTukey)
plot(multCompTukey$emmeans, comparisons = TRUE)
cld(multCompTukey$emmeans)   # Asigna numeros a los tratamientos 
# Grafico presentable
pred_modelo<-as.data.frame(multCompTukey$emmeans)
ggplot(pred_modelo, aes(x=zona, y=emmean)) +
  geom_point(aes(colour = zona)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, colour = zona), width = 0.2) +
  theme_classic()

####6.3) Modelado de GATOS con ETAPA (fijo) y TRANSECTA (aleatorio)####
#'[Modeo gatos sin zona, por etapa]
#Modelo GLMM sin analizar diferencia entre zonas
Gatos6 <-  glmer(gatos ~ etapa + (1|transecta), data = gatos,family = "poisson", 
                      na.action = na.fail)

#'[Interpretación]: gatos como variable númerica, etapa como factor fijo
#transecta como factor aleatorio na.action para que indique en los casos que faltan valores. 

summary(Gatos6) #OJO ACA CON TOMAR P COMO SIGNIFICACNCIA
confint(Gatos6, devtol=1e-9) #intervalos de confianza 
#tolerance for fitted deviances less than baseline (supposedly minimum) deviance.

#'[Analisis de Gatos6] El modelo converge. Resulta confiable. 
#'Lo vemos en el summary VER SIGNIFICANCIA

#resumen- Fixed effects:
               #Estimate Std. Error z value Pr(>|z|)
#(Intercept)  -0.7803     1.0647  -0.733   0.4636
#etapa2019    -0.9808     0.3027  -3.240   0.0012 **
#etapa2021    -1.4917     0.3689  -4.043 5.27e-05 ***

#'[Interpretación]: el valor estimate compara 2018 vs 2019 y vs 2021. 
#'Un valor menor en 2019 y 2021, representa menor probabilidad de encontrar gatos 
#'en esos años respecto a 2018. El error es pequeño asi que el modelo es confiable. 

#'[Mas detalle: el intercept es la media de la abundancia de gatos para el 2018]
#'[el valor esta en logaritmo y para sacar el valor debo aplicar exponencial]

#2018, 2019, 2021 respectivamente
exp (-0.7803) #0.4582685
exp (-0.9808) #0.375011
exp (-1.4917) #0.2249898

#'[que el intercept de significativo quiere decir que es distinto de 0]
#'[es decir, la abundancia del 2018 no es nula]
#'[El estimate da un valor de tasa de cambio respecto al Intercept. Son pendientes]
#'[La pendiente de la media de la abundancia de 2019 y 2021 es negativa respecto a 2018]
#'[mientras mas grande es el valor del estimate mayor es la diferencia]
#'[el valor p dice que la pendiente es distinta de cero. Si da 0 (p valor no significativo)
#'[quiere decir que no hay diferencia entre las comparaciones]

####6.3.1) Supuestos de modelo GATOS etapa####
#'[Corroboro supuestos: dispersion.
#'[Normalidad, homocedasticidad y linealidad con residuos]
#'#'[Posterior Predictive Check:  ajuste de modelo; colinealidad; outliers; normalidad, homocedasticidad]
check_model(Gatos6)
check_overdispersion(Gatos6)
check_zeroinflation(Gatos6)

#'[Residuos]

#Residuos
residuoscrudos=residuals(Gatos6)
residuoscrudos
residuosstd <- rstandard(Gatos6) 
residuosstd
#evaluemos graficamente los supuestos
e<-resid(Gatos6) # residuos
re<-rstandard(Gatos6) #residuos estandarizados
recrudos <-residuals(Gatos6) #residuos crudos
pre<-predict(Gatos6) #predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo 6" )
abline(0,0)
abline(h = -2, v = 0, col = "red", lty = 3)
abline(h = 2, v = 0, col = "red", lty = 3)

#QQPlot para normalidad
library(car)
qqPlot(residuoscrudos)
shapiro.test(e) #NORMALIDAD analitica
#Homocedasticidad (solo con CUALITATIVAS)

### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(Gatos6)
leveneTest(gatos ~ etapa + (1|transecta), data = gatos, family = "poisson") 


###6.3.2) COMPARO TODOS LOS MODELOS

AIC (Gatos6,Gatos5,Gatos01,Gatos06)

####6.3.2) Analizo modelo seleccionada de GATOS etapa####

#Modelo GLMM sin analizar diferencia entre zonas
Gatos6 <-  glmer(gatos ~ etapa + (1|transecta), data = gatos,family = "poisson", 
                 na.action = na.fail)

#Bolker para intervalos de confianza y modelo con offset
library(glmmTMB)
mod_gatos2 <- glmmTMB(abundancia ~ etapa +(1|transecta) + offset(log(esfuerzo)), data = gatos,
                      family=poisson, na.action = na.fail)

#'[Prueba de Wald]
library(lmerTest)
#'Con un n grande esta prueba va a dar practicamente lo mismo que el test de devianza
#'Con muestras muy chicas hay diferencias
#'La prueba de Wald no se recomienda con n bajo
#'
#'[Test de devianza o cociente de verosimilitudes (Likelihood ratio test, LRT)]
#'[Puede usarse ANOVA o drop1]
#'[ANOVA]
#'En GLM cuando uno hace un ANOVA del modelo para ver si es significativo
#'el efecto del tratamiento, el ANOVA hace un test de devianzas (diferencia un delta de devianzas)
library(car)
anova(Gatos6)

#'[drop1]
library(lme4)
drop1(Gatos6)

#'[Análisis de devianza en GLMM: Variabilidad NO explicada por el modelo]
#GLMM: usa máxima solicitud restringida. Ver los libritos para GLMM que están en 
#inglés. Y ahí ver que suplanta a la devianza.
deviance(Gatos6) #da 13.04325


####6.3.3) GRÁFICO predictivo del modelo GATO etapa seleccionado####
#'[En este caso, el grafico de ggpredict nos va a mostrar las abundancias]
#'[MEDIAS predichas para cada grupo]

ggpredict(Gatos6, terms = c("etapa") #Predictores a graficar ENTRE COMILLAS
) %>% #pipeamos para que nos ponga el objeto creado de ggpredict en plot
  plot(add.data = T) +
  ylim (0,5) #acomodo margen del eje y para ver los IC (dejo fuera los outliers?)

#Outliers: un outlier en Y va a bajar el ajuste del modelo ya que vamos a tener mucho residuo ajustado a ese dato

#CAMBIAR NOMBRE DE FILA Y COLUMNA PARA TENER BIEN EL GRAFICO


####6.3.4) Comparacion de variables del modelo GATOS etapa####
#'[ Si queremos ver en NUMEROS lo ploteado por el grafico podemos usar la funcion]
#'[ emmeans:]

emmeans(Gatos6, list(pairwise ~ etapa), #Le pido que me haga comparaciones multiples respecto a una variable
        type = "response") #Arg 3: Me muestra los valores detransformados

#'[ La funcion emmeans usada de esta forma me devuelve 2 TABLAS:]
#'[ La primera me muestra las medias predichas por el modelo para cada grupo]
#'[ La segunda me muestra las comparaciones multiples]
#'[Interpretacion de lo que me está diciendo el emmeans]
#'Si divido la media del etapa2018 sobre etapa2019 me da valor de 2.67. 
#'Esto quiere decir que la abundancia media de 2018 es 2.67 veces que la abundancia 
#'media de 2019. Entonces en ratio: si es igual a 1, la media de 2018 y 2019 es igual.
#'Si es mayor a 1,  numerador (2018) es mayor que denominador (2019). 
#'Voy a encontrar 2.67 gatos en el 2018 que en 2019. Hay casi 3 veces mas de gatos
#'en 2018 que en 2019. Si ratio es menor a 1, numerador es menor a denominador.
#'Si ratio es más, menos o 1, saco la dirección del efecto y el tamaño del efecto. 
#'El p valor me dice si esa diferencia es significativa o no. 

#7) GRAFICO DEL MODELO ----
####7.1) Grafico de GATOS por ETAPA ####
#'[gráfico de gatos con modelo seleccionado]

#crear columna predichos con valores detransformados
gatos$predichos <- predict(mod.gatos.2, type= "response") 

#graficos ggplot por etapa
#'[Lo bueno de ggplot es que puedo armar el grafico como se me cante]
#'[tengo que aprender a diseñarlo piola]

#defino como voy a querer mostrar las transectas
p.alineado <- position_dodge2(width = 0.9, padding = 0) #alineado horizontal
p.aleatorio <- position_jitter(w=0.5, h=0.5)
#'[agregar en aes si quiero usarlo]
#'#position = p.alineado

#Grafico con facet_wrap etapa
graficogatos <- ggplot(gatos,aes(etapa,predichos,col=transecta))+
  geom_point(size=3, position = p.alineado)+
  facet_wrap(~etapa, scales ="free_x") +
  labs(color="Transect",
       x ="Year",y="Predicted counts of F. catus") +
  theme(strip.text.x= element_blank(), plot.title=element_text(size=14,face="italic",hjust=0.5))
graficogatos

####7.2) Otros diseño graficos ####

#Grafico con facet_wrap transecta
graficogatos <- ggplot(gatos,aes(transecta_num,predichos,col=transecta_name_nro))+
  geom_point(size=3,position = p.alineado)+
  facet_wrap(~etapa) +
  labs(color="Transect (nº)",x ="Transect",
       y="Predicted counts of F. catus")
graficogatos

#♠ Grafico con todos los años
ggplot(data=gatos, aes(x=etapa,y=predichos,label=transecta,col=transecta, shape=etapa))+
  geom_point(size=3,position = p.alineado) +
  labs(title="F. catus",subtitle="", color="Transect",shape="Year"
       ,tag="",x ="Year",y="Predicted counts") +
  theme(plot.title=element_text(size=14,face="italic",hjust=0.5))

#8) PRESENTACIÓN DE RESULTADOS -----
####8.1) Presentar GATOS por ETAPA ####

#'[Grafico final con estimaciones segun modelo]
#Escala logaritmica
library(emmeans)
comp1<-emmeans(mod.gatos.2, pairwise ~ etapa)
confint(comp1) #magnitud del efecto
resumen_anidado<-as.data.frame(comp1$emmeans)
ggplot(resumen_anidado, aes(x=etapa, y=emmean, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos gatos") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+
  geom_point(shape=15, size=4) + 
  ggtitle("Comparacion de gatos por etapa", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.01")

#En variable respuesta no logaritmica
#Ojo, acá: para graficar ymin e ymax tengo que hacer la exponencial de esos valores de escala log
comp2<-emmeans(mod.gatos.2, pairwise ~ etapa, type = "response")
confint(comp2) #magnitud del efecto
resumen_anidado2<-as.data.frame(comp2$emmeans)
ggplot(resumen_anidado2, aes(x=etapa, y=rate, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos gatos (log)") +
  geom_errorbar(aes(ymin=rate-SE, ymax=rate+SE), color="blue", width=0.2)+ #ver si está bien ymax y min
  geom_point(shape=15, size=4) + 
  ggtitle("Comparacion de gatos por etapa (log)", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.01")

#Grafico con más diseño - retocar para que corra bien
ggplot (data=gatos, 
        aes(x=etapa,y=predichos, label=transecta))+ 
  geom_point(aes(colour=etapa, shape=etapa),size=5, )+
  labs(title="Predicted counts of F. catus",subtitle="", color="Year",shape="Year"
       ,tag="A",x ="Year",y="Predicted counts")+
  theme( plot.tag.position = "topleft",
         plot.tag = element_text(size=20),
         panel.border=element_blank(),
         panel.grid.major=element_line(),panel.grid.minor=element_blank(),
         panel.background= element_rect(fill="grey90"),
         plot.title=element_text(size=14,face="italic",hjust=0.5),
         plot.subtitle = element_text(size = 14, hjust=0.66, vjust = 7.5),
         legend.text=element_text(size=9),
         legend.title=element_text(size=12),
         axis.line=element_line(size=0.5,linetype="solid",colour="black"),
         axis.text.x=element_text(size=10,color="black",face="bold"),
         axis.text.y=element_text(size=10,color="black",face="bold"))
  ylim (0,20) #acomodo margen de y 


#geom_line(aes(group=transecta)) #lineas que unen las transectas por año
#geom_text(position = "jitter")  #agrega el texto en grafico pero se superpone

#geom_ribbon(aes(ymin = 0, ymax = 15), alpha = 0.1) #VER COMO AGREGAR INTERVALOS AL GRAFICO
#geom_text_repel(position=p.alineado,point.padding=0.2, size=2,force =1, segment.alpha=0, na.rm=F) #para agregar texto en puntos del grafico   



#grafico bloxplot
#'[Si lo quiero presentar como boxplot tengo que aprender a diseñarlo piola]

boxplot(gatos$predichos~gatos$etapa) #grafico boxplot para 2018,2019 y 2021 con valores predichos
boxplot(gatos$gatos~gatos$etapa) #grafico boxplot para 2018,2019 y 2021 con valores reales



#Datos FAUNA SILVESTRE - AJm----
#9)Datos ATAJACAMINOS ----

#'[2.2a) Datos atajacaminos]
# cargar archivo de datos .csv o xlsx
atajacaminos <- read.csv("E:\\Análisis_R\\IMG\\atajacaminos18-21.csv", 
                         header = TRUE , sep = ";")
atajacaminos <- read_excel ("E:\\Análisis_R\\IMG\\atajacaminos18-21.xlsx")


#Controlo el tipo de datos
#'[Siempre corroborar la clase de datos que tenemos por si hay que convertir]
class(atajacaminos$Atajacaminos)
class(atajacaminos$dia)
class(atajacaminos$etapa)
class(atajacaminos$transecta)

#'[convertir numericos o integer a caracteres pq son variables categoricas]
atajacaminos$dia <- as.character(atajacaminos$dia) 
class(atajacaminos$dia)
atajacaminos$etapa <- as.character(atajacaminos$etapa)
class(atajacaminos$etapa)

#10) Análisis exploratorio AtJ ----
#'[Analizamos datos atajacaminos]

atajacaminos %>% #paip: le digo a ggplot que use la tabla atajacaminos
  ggplot(aes(y = Atajacaminos, x = etapa, fill= etapa)) + ##entre dos variables cuantitativas lo mejor es geompoint y con cuali geobox
  geom_boxplot()

#11) Modelos AtJ ----
#'[GLM atajacaminos]
#'[ Como la variable respuesta es una variable de CONTEO, vamos a] 
#'[ construir modelos con distribucion Poisson.]

AJm1 <- glm(Atajacaminos ~ etapa, family = poisson, data = atajacaminos) 


#FUNCION SUMMARY Y COEFICIENTES
#'[Segundo paso de la receta: correr la funcion summary a ver que da:]

summary(AJm1)
confint(AJm1) #intervalos de confianza

#'[Como interpreto esto: el intercept es la media de la abundancia de atajacaminos para el 2018]
#'[el valor esta en logaritmo y para sacar el valor debo aplicar exponencial]

#2018, 2019, 2021 respectivamente
exp (1.7636) #5.8334
exp (0.8014) #2.228659
exp (0.6642) #1.942936

#'[que el intercept de significativo quiere decir es distinto de 0]
#'[es decir, la abundancia del 2018 no es nula ]
#'[El estimate da un valor de tasa de cambio respecto al Intercept. Son pendientes]
#'[La pendiente de la media de la abundancia de 2019 y 2021 es positiva respecto a 2018]
#'[mientras mas grande es el valor del estimate mayor es la diferencia]
#'[el valor p dice que la pendiente es distinta de cero. Si da 0 (p valor no significativo) quiere decir que no hay diferencia entre las comparaciones]



#12) Supuestos AtJ ----

#Corroborar supuestos
#'[Posterior Predictive Check:  ajuste de modelo; colinealidad; outliers; normalidad, homocedasticidad]
check_model(AJm1)

#Sobredispersión AtJ
#'[¿Que pasa cuando tengo varias opciones para un mismo tipo de variable respuesta?]
#'[Bueno, ahi tenemos que decir que distribucion se ajusta mejor a mis datos. Para]
#'[una variable de conteo voy a elegir:]
#'[  - DISTRIBUCION POISSON: si NO hay sobredispersion de datos]
#'[  - DISTRIBUCION BINOMIAL NEGATIVA: si los datos esta sobredispersos para una Poisson]

#Uso función del paquete performance
check_overdispersion(AJm1)

#'[Residuos]

#Residuos
residuoscrudos=residuals(AJm1)
residuoscrudos
residuosstd <- rstandard(AJm1) 
residuosstd
#evaluemos graficamente los supuestos
e<-resid(AJm1) # residuos
re<-rstandard(AJm1) #residuos estandarizados
recrudos <-residuals(AJm1) #residuos crudos
pre<-predict(AJm1) #predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo AJm1" )
abline(0,0)
abline(h = -2, v = 0, col = "red", lty = 3)
abline(h = 2, v = 0, col = "red", lty = 3)

#QQPlot para normalidad
library(car)
qqPlot(residuoscrudos)
shapiro.test(e) #NORMALIDAD analitica
#Homocedasticidad (solo con CUALITATIVAS)

### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(AJm1)
leveneTest(Atajacaminos ~ etapa, family = poisson, data = atajacaminos) 

#13) Analizo modelo seleccionado de AtJ-----

#'[Prueba de Wald]
library(lmerTest)

#'[Test de devianza o cociente de verosimilitudes (Likelihood ratio test, LRT)]
#'[Puede usarse ANOVA o drop1]
#'[ANOVA]
library(car)
anova(AJm1)
#'[drop1]
library(lme4)
drop1(Gatos6)

#'[Análisis de devianza en GLM: Variabilidad NO explicada por el modelo]
#'[El valor que da deviance es el que no se puede explicar]
#'entonces tengo que hacer deviance - 100 y eso es lo que si se explica
deviance(AJm1) #es la Residual= 13.69422

#Analizo el residual deviance
#'[El residual deviance no puede irse mucho mayor que 1,]
#'[puede ir hasta 1.5 maximo. Hago 13.694/15 (grado de libertad)]
#'[1] 0.9129333 entonces NO HAY SOBREDISPERSION


#14) GRÁFICO predictivo de AJm----
#'[En este caso, el grafico de ggpredict nos va a mostrar las abundancias]
#'[MEDIAS predichas para cada grupo]

ggpredict(AJm1, terms = c("etapa"))  %>%  
  plot(add.data = T) + ylim (0,20)

#CAMBIAR NOMBRE DE FILA Y COLUMNA PARA TENER BIEN EL GRAFICO


#15) Comparacion de variables del modelo AJm ------

#'[Comparacion múltiple]
library(emmeans)
emmeans(AJm1, list(pairwise ~ etapa), type = "response") 


#'[Otro paquete para comparaciones múltiples]
library(multcompView)
library(multcomp)
multCompTukey <- emmeans(AJm1, pairwise ~ etapa)
summary(multCompTukey)
confint(multCompTukey)
plot(multCompTukey$emmeans, comparisons = TRUE)
cld(multCompTukey$emmeans)   # Asigna numeros a los tratamientos 
# Grafico presentable
pred_modelo<-as.data.frame(multCompTukey$emmeans)
ggplot(pred_modelo, aes(x=etapa, y=emmean)) +
  geom_point(aes(colour =etapa)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, colour = etapa), width = 0.2) +
  theme_classic()




#16) GRAFICO DEL MODELO AjM ----

#crear columna predichos con valores detransformados
atajacaminos$predichos <- predict(AJm1, type= "response") 

#graficos ggplot por etapa
#defino como voy a querer mostrar las replicas (dias)
p.alineado <- position_dodge2(width = 0.9, padding = 0) #alineado horizontal
p.aleatorio <- position_jitter(w=0.5, h=0.5)
 #'[agregar en aes si quiero usarlo]
 #'#position = p.alineado

#♠ Grafico con todos los años
ggplot(atajacaminos, aes(x=etapa,y=predichos,col=etapa))+
  geom_point(size=3) +
  labs(color="Year",x ="Year",y="Predicted counts of Caprimulgus spp.") 
  
# Otros graficos
ggplot (data=atajacaminos, 
        aes(x=etapa,y=predichos))+ #label=dia para numero de replicas
          geom_point(aes(colour=etapa, shape=etapa),size=5, )+
          labs(title="Predicted counts of Caprimulgus spp.",subtitle="", color="Year",shape="Year"
               ,tag="A",x ="Year",y="Predicted counts of Caprimulgus spp.")+
           theme( plot.tag.position = "topleft",
                  plot.tag = element_text(size=20),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(),panel.grid.minor=element_blank(),
                  panel.background= element_rect(fill="grey90"),
                  plot.title=element_text(size=14,face="italic",hjust=0.5),
                  plot.subtitle = element_text(size = 14, hjust=0.66, vjust = 7.5),
                  legend.text=element_text(size=9),
                  legend.title=element_text(size=12),
                  axis.line=element_line(size=0.5,linetype="solid",colour="black"),
                  axis.text.x=element_text(size=10,color="black",face="bold"),
                  axis.text.y=element_text(size=10,color="black",face="bold"))+
            ylim (0,20) #acomodo margen de y 
            
            

#En variable respuesta no logaritmica. TYPE RESPONSE
#Ojo, acá: para graficar ymin e ymax tengo que hacer en escala response
comp1<-emmeans(AJm1, pairwise ~ etapa, type = "response")
confint(comp1) #magnitud del efecto
resumen_AJm1<-as.data.frame(comp1$emmeans)
ggplot(resumen_AJm1, aes(x=etapa, y=response, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos atajacaminos") +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), color="blue", width=0.2)+ #ver si está bien ymax y min
  geom_point(shape=15, size=4) + 
  ggtitle("Comparacion de atajacaminos por etapa", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.05")



#Datos FAUNA SILVESTRE - Lagartos----

#17)Datos lagarto ----

#'[2.2a) Datos lagarto]
# cargar archivo de datos .csv o xlsx
lagartos <- read.csv("E:\\Análisis_R\\IMG\\lagarto18-21.csv", header = TRUE , sep = ";")

#Controlo el tipo de datos
class(lagartos$lagartos)
class(lagartos$zona)
class(lagartos$etapa)
class(lagartos$transecta)
#'[convertir variables categoricas]
lagartos$etapa <- as.character(lagartos$etapa)
class(lagartos$etapa)

#18) ANALISIS EXPLORATORIO lagartos ----
#'[Analizamos datos lagartos]

#Libreria para descripcion de datos generales y de los tratamientos
library(pastecs)
summary(lagartos)
stat.desc(lagartos)

#Regresion Multiple: Para más de una VE de tipo cualitativa - CORRELACION Y COLINEALIDAD
library(GGally)
ggpairs(lagartos)

##Estadistica descriptiva
library(Rmisc)
summarySE(lagartos, measurevar="lagartos", groupvars="etapa")

#Para ver media y varianza de la VR por variable explicativa
library(doBy) #la relacion de la media y la varianza debe ser 1 a 1 para ser Poisson
summaryBy(lagartos~etapa, data=lagartos, FUN=c(mean, var, length))
summaryBy(lagartos~zona, data=lagartos, FUN=c(mean, var, length))

####18.1) Grafico descriptivo ####
par(mar = c(5,4,4,2))

#Grafico de perfiles descriptivo 
ggplot(lagartos, aes(zona,lagartos)) + facet_grid(. ~etapa) + 
  geom_jitter(aes(colour=transecta), size=2, height = 0.01, width = 0.4) +
  labs(x="zona", y="nro de lagartos")

#Para verlo graficamente:
library(ggplot2)
ggplot(lagartos, aes(x=etapa, y=lagartos, colour=etapa)) +
  geom_jitter(height = 0.01, width = 0.1) +
  xlab("year") + ylab("avistaje lagartos")

#### boxplot: CUALITATIVAS 
box <- ggplot(lagartos, aes(x=etapa, y=lagartos))+        
  geom_boxplot()+ 
  stat_summary(fun=mean, geom="point", shape=19, size=4,color="black")+
  theme_bw()
box
### Para una sola Variable Explicatoria de tipo cualitativa 
plot(lagartos ~ etapa, data=lagartos)  #grafico simple

library(ggplot2) #grafico cheto
grafico1 <-ggplot(lagartos,aes(x=etapa, y=lagartos))+
  geom_point(aes(), colour= "deepskyblue", size=4)+
  geom_smooth(method=lm,se=T,fullrange=T) +
  xlab("Year") +  ylab("Encuentros T.mearianae") +
  ggtitle("Avistajes de lagarto por etapa")
grafico1

#Otro grafico diseño..
lagartos %>% 
  ggplot(aes(y=lagartos,x=etapa,fill= etapa)) +
  geom_boxplot()

#por zona
#cambio el orden de los niveles y grafico
lagartos$zona<-factor(lagartos$zona,levels=c("Baja","Media", "Alta"))

lagartos %>% 
  ggplot(aes(y=lagartos,x=zona,fill=zona)) +
  facet_grid(. ~etapa)+
  geom_boxplot()

#por transecta
lagartos %>% 
  ggplot(aes(y=lagartos,x=transecta,fill=transecta)) +
  geom_boxplot()

#19) CONSTRUIR MODELOS y SUMMARY: LAGARTOS ----
  #'[GLMM lagartos]
  #'[En este caso tengo que hacer un modelo que tenga las dos variables zona y etapa]
  
####19.1) Modelo lagartos con 2 FIJOS y 1 ALEATORIO####

#Formula del GLMM: y~x1 +x2 +(x1 | x3) donde y= VR, x=VE, (|)= distingue las variables aleatorias.
#Ejemplo:  lmer (Reaction~Days + (Days|Subject)). Models Reaction time as a function of Days, with the Subject as a random effect
#Para poisson (link="log")

#'[Resolver]
#'[¿porque no puedo cambiar el intercept y porque no veo los 6 niveles y hay solo 5?]
#resolver el intercept. Me toma 2018 ¿puedo hacer un mixto con mis dos variables categoricas=
Lagartos1 <- glmer(lagartos~ etapa + zona +(1|transecta), data = lagartos,
                        family = "poisson", na.action = na.fail)



lagartos$zona = relevel(factor(lagartos$zona), ref = "Baja") # set Baja as Intercept pero como baja es character hay que pasarlo a factor

library(lmerTest) #no funciona el Wald en glmer. Parece que funciona para lmer que es con NORMALIDAD
summary(Lagartos1) #no tengo p-valores confiables con glmer
anova(Lagartos1) #no informa p-valores
drop1(Lagartos1,test="Chisq" ) #no tengo p-valores confiables con glmer
deviance(Lagartos1)

#'[Otra opción de paquete: Modelo con glmmTMB]

Lagartos2<- glmmTMB(lagartos ~ zona + etapa +(1|transecta), data = lagartos,
                      family=poisson, na.action = na.fail)
library(lmerTest) #No funciona
summary(Lagartos2) #Que hago con los p-valores?
anova(Lagartos2) #no single-model anova() method for glmmTMB
drop1(Lagartos2,test="Chisq" )
confint(Lagartos2) #intervalos de confianza
deviance(Lagartos2) #me da nula
check_overdispersion(Lagartos2)


#### 19.2 Modelo con INTERACCION #####
Lagartos2bis <- glmer(lagartos~ etapa*zona +(1|transecta), data = lagartos,
                   family = "poisson", na.action = na.fail)


summary(Lagartos2bis)
check_overdispersion(Lagartos2bis)
check_zeroinflation(Lagartos2bis)



#### 19.2) Modelo lagartos por ZONA ####

Lagartos3 <- glmer(lagartos ~ zona+(1|etapa), data = lagartos, 
                   family = "poisson", na.action = na.fail)

summary(Lagartos3)
confint(Lagartos3) #intervalos de confianza
deviance(Lagartos3) #devianza
check_overdispersion(Lagartos3) #sobredispersion

#Usando Dharma: test para determinar si el parametro de dispersion difiere de 1
library(DHARMa)
testDispersion(Lagartos3)
#SI la dispersion no es igual a 1 y tengo sobredispersion voy por Binomail negativa y si tengo subdispersion voy por Conway MAxwell (que tambien hace sobre dipersion)

check_model(Lagartos3) #supuestos del modelo

#### 19.3) Modelo lagartos por ETAPA ####

Lagartos4 <-  glmer(lagartos ~ etapa + (1|transecta), data = lagartos, 
                    family = "poisson", na.action = na.fail)

summary(Lagartos4)
confint(Lagartos4, devtol=1e-9) #intervalos de confianza 
#tolerance for fitted deviances less than baseline (supposedly minimum) deviance.

check_overdispersion(Lagartos4) 
check_model(Lagartos4) 


#20) Comparacion de modelos lagartos ----
AIC(Lagartos1,Lagartos2,Lagartos3,Lagartos4,Lagartos2bis)

#el modelo que tiene dos categoricas es el mejor
plot(Lagartos1)

#21) Supuestos de modelo lagartos seleccionado ----
check_model(Lagartos1)
check_overdispersion(Lagartos1)

#'[Residuos]

#Residuos
residuoscrudos=residuals(Lagartos1)
residuoscrudos
residuosstd <- rstandard(Lagartos1) 
residuosstd
#evaluemos graficamente los supuestos
e<-resid(Lagartos1) # residuos
re<-rstandard(Lagartos1) #residuos estandarizados
recrudos <-residuals(Lagartos1) #residuos crudos
pre<-predict(Lagartos1) #predichos
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo AJm1" )
abline(0,0)
abline(h = -2, v = 0, col = "red", lty = 3)
abline(h = 2, v = 0, col = "red", lty = 3)

#QQPlot para normalidad
library(car)
qqPlot(residuoscrudos)
shapiro.test(e) #NORMALIDAD analitica
#Homocedasticidad (solo con CUALITATIVAS)

### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(Lagartos1)
leveneTest() 

#22) Analizo modelo seleccionado de Lagarto-----


#'[Test de devianza o cociente de verosimilitudes (Likelihood ratio test, LRT)]
#'[Puede usarse ANOVA o drop1]
#'[ANOVA]
library(car)
anova(Lagartos1)
#'[drop1]
library(lme4)
drop1(Lagartos1)

#'[Análisis de devianza en GLMM: Variabilidad NO explicada por el modelo]
deviance(Lagartos1) 


#23) Comparacion de variables en modelo Lagarto ----


#'[Comparaciones múltiples]
library(emmeans)
emmeans(Lagartos1, pairwise ~ etapa, type="response")
emmeans(Lagartos1, pairwise ~ zona, type="response")
emmeans(Lagartos1, pairwise ~ etapa + zona, type="response")


##24)GRAFICO DEL MODELO Lagarto
  ####24.1) GRÁFICO POR ZONA####
  
  par(mfrow = c(2, 3)) # Permite colocar varios graficos en una misma pantalla.
  par(mfrow = c(1, 1)) #Volver a particionar la pantalla en 1
  
  ggpredict(Lagartos3,terms = c("zona")) %>% 
    plot(add.data = T) +
    ylim (0,5) #acomodo margen del eje y para ver los IC (dejo fuera los outliers?)
  
  emmeans(Lagartos3, list(pairwise ~ zona), type = "response") 
  
#Gggplot lagartos por ZONA
  #crear columna predichos
  lagartos$predichos <- predict(Lagartos3, type= "response") 

  #defino como voy a querer mostrar las transectas
  p.alineado <- position_dodge2(width = 0.9, padding = 0) #alineado horizontal
  p.aleatorio <- position_jitter(w=0.5, h=0.5)
  
  #Grafico con facet_wrap ZONA
  graficoglagartos1 <- ggplot(lagartos,aes(zona,predichos,col=transecta))+
    geom_point(size=3, position = p.alineado)+
    facet_wrap(~zona, scales ="free_x") +
    labs(color="Transect",
         x ="Zone",y="Predicted counts of T.merianae") +
    theme(strip.text.x= element_blank(), plot.title=element_text(size=14,face="italic",hjust=0.5))
  graficolagartos1
  
 #### 24.2) GRÁFICO POR ETAPA #####

  ggpredict(Lagartos4, 
            terms = c("etapa") #Predictores a graficar ENTRE COMILLAS
  ) %>% #pipeamos para que nos ponga el objeto creado de ggpredict en plot
    plot(add.data = T) +
    ylim (0,5) #acomodo margen del eje y para ver los IC (dejo fuera los outliers?)

#Gráfico en ggplot lagartos ETAPA
  
  #crear columna predichos con valores detransformados
  lagartos$predichos <- predict(Lagartos4, type= "response") 
  
  #graficos ggplot por etapa
  #defino como voy a querer mostrar las transectas
  p.alineado <- position_dodge2(width = 0.9, padding = 0) #alineado horizontal
  p.aleatorio <- position_jitter(w=0.5, h=0.5)
  
  #Grafico con facet_wrap etapa
  graficoglagartos2 <- ggplot(lagartos,aes(etapa,predichos,col=transecta))+
    geom_point(size=3, position = p.alineado)+
    facet_wrap(~etapa, scales ="free_x") +
    labs(color="Transect",
         x ="Year",y="Predicted counts of T.merianae") +
    theme(strip.text.x= element_blank(), plot.title=element_text(size=14,face="italic",hjust=0.5))
  graficolagartos2

  #Datos FAUNA SILVESTRE - Lagartijas----
#25)Datos LAGARTIJA ----
  
#'[2.2a) Datos atajacaminos]
# cargar archivo de datos .csv o xlsx
  lagartijas <- read.csv("E:\\Análisis_R\\IMG\\lagartijas18-21.csv", 
                           header = TRUE , sep = ";")
  
#Controlo el tipo de datos
#'[Siempre corroborar la clase de datos que tenemos por si hay que convertir]
  class(lagartijas$Lagartija)
  class(lagartijas$dia)
  class(lagartijas$etapa)
  class(lagartijas$transecta)
  
#'[convertir numericos o integer a caracteres pq son variables categoricas]
  lagartijas$dia <- as.character(lagartijas$dia) 
  class(lagartijas$dia)
  lagartijas$etapa <- as.character(lagartijas$etapa)
  class(lagartijas$etapa)

#26) ANALISIS EXPLORATORIO LgJ ---
#'[Analizamos datos LgJ]
lagartijas %>% 
    ggplot(aes(y = Lagartija, x = etapa, fill= etapa)) + 
    geom_boxplot()

#27) CONSTRUIR MODELOS: LgJ ---
#'[GLM lagartijas]

Lgj1 <- glm(Lagartija ~ etapa, family = poisson, data = lagartijas)

#FUNCION SUMMARY Y COEFICIENTES
#'[Segundo paso de la receta: correr la funcion summary a ver que da:]

summary(Lgj1)
confint(Lgj1) #intervalos de confianza

#2018,2019,2021 respectivamente
exp () #
exp () #
exp () #

#'[El valor que da deviance es el que no se puede explicar]
#'entonces tengo que hacer deviance - 100 y eso es lo que si se explica
deviance(Lgj1) #es la Residual= 13.69422

#28) Remodelado por sobredispersion del modelo--

#Uso función del paquete performance
check_overdispersion(Lgj1)

#SI la dispersion no es igual a 1 y tengo sobredispersion voy por Binomail Negativa
#y si tengo subdispersion voy por Conway MAxwell (que tambien hace sobre dipersion)

#Corroborar supuestos
#'[Posterior Predictive Check]
check_model(Lgj1)



#29) NUEVO MODELO PARA CORREGIR SOBREDISPERION LgJ----
#Modelando sobredispersion
#'[GLM con Binomial Negativa]: uso lme4 (glmer.nb); glmmTMB (family="nbinom2")
#' o libreria MAS (que es exclusiva para binomial negativa (glm.nb) 

library(glmmTMB) #paquete de Bolker. No me tira devianza
Lgj2 <- glmmTMB(Lagartija ~ etapa, family="nbinom2", data = lagartijas)
summary(Lgj2)
df.residual(Lgj2)
confint(Lgj2)

library (MASS) #paquete para glm binomial negativa
Lgj3 <- glm.nb(Lagartija ~ etapa, data = lagartijas)
summary(Lgj3)
confint(Lgj3)
check_overdispersion(Lgj3) #sin overdispersion
check_model(Lgj3)

library(lme4)
Lgj3 <- glmer.nb(Lagartija ~ etapa, data = lagartijas)

#30) Selección de mejor modelo-------
AIC(Lgj1,Lgj2,Lgj3) #está bien que el 2 y el 3 sean iguales porque son el mismo modelo con un paquete para binomial negativa que es distinto


#31)Supuestos del Modelo--------
#'[1) Independencia entre las observaciones]
#'No debe existir correalación entre observaciones (no se cumple para medidads repetidas)
#'En los GLMM no se cumple y ahi tenemos que introducir el factor aleatorio
#'[2) Normalidad]
#'Para cada nivel de X, la variable Y tiene una distribución normal. Hago QQplot y Prueba de Shapiro Wilk. 
#'[3) Dispersion]
#'Podemos correr el performance, Dharma o hacerlo manual. Se ajusta a otra distribución si da sobre o subdispersion.  
#'[4) Homocedasticidad de la varianza]
#'Las varianzas de las subpoblaciones son iguales. Grafico de dispersion de residuos estandarizados vs predichos.
#Pruebe de Levene. Si p-valor es mayor a 0,05, varianza homogenea. Si es menor, hay heterocedasticidad.
#'Si no se cumple el supuesto, se puede modelar la varianza. 
#'No hay mocedasticidad en Poisson pero var y media deben ser iguales
#'[5) Linealidad]
#'Para Variable Explicatoria CUANTITATIVA. Grafico dispersion de residuos est. vs predichos. 


### Corroborar los supuestos
#'[Vamos a manejarnos con los residuos crudos, residuos estandarizados y predichos]
#'[Evaluemos Normalidad, Homocedasticidad y Linealidad con residuos]
#'Residuos:
residuoscrudos=residuals(Lgj3)
residuoscrudos
residuosstd <- rstandard(Lgj3)
residuosstd

#Supuestos linealidad, homocedasticidad
e<-resid(Lgj3) # residuos
re<-rstandard(Lgj3) #residuos estandarizados
pre<-predict(Lgj3) #predichos
par(mfrow = c(1, 3))
plot(pre, re, xlab="Predichos", ylab="Residuos estandarizados",main="RE vs PRED - Modelo Lgj3" )
abline(0,0)
abline(h = -2, v = 0, col = "red", lty = 3)
abline(h = 2, v = 0, col = "red", lty = 3)

#'[1) Independencia entre las observaciones]
#Hay independencia. 
#'[2) Normalidad]
#QQPlot
library(car)
qqPlot(residuoscrudos)
qqnorm(e, main = "QQplot -Modelo Lgj3")
qqline(e)
shapiro.test(e) #NORMALIDAD analitica
#Cuanto más cercano a 1, mayor es la evidencia de normalidad. Cuando el p-valor
#es mayor a 0,05 no hay evidencias significativas para rechazar la normalidad.

#'[3) Dispersion]
##Dispersion por distintos métodos
#Calcule la dispersion del modelo aditivo completo.  
(dispersion<-sum(resid(Lgj3, type="pearson")^2/Lgj3$df.residual))
#Usando Dharma: test para determinar si el parametro de dispersion difiere de 1
library(DHARMa)
testDispersion(Lgj3)
#Con paquete Performance
check_overdispersion(Lgj3)

#'[4) Homocedasticidad de la varianza]
#'#Residuos
rp1 <- residuals(Lgj3, type="pearson")
ajust1 <- fitted(Lgj3) #predichos en escala de Y
#Grafico de residuos vs predichos
plot(ajust1, rp1,xlab="predichos", ylab= "Residuos pearson", 
     main="Grafico de RP vs PRED modelo 1", cex.main=.8)
abline(0,0)
#Evaluar Homocedasticidad SOLO CUANDO LA VARIABLE ES CUALITATIVA
### Prueba de Levene para homogeneidad de varianzas
library(car)
leveneTest(Lagartija ~ etapa, data = lagartijas) #Prueva de Levene planteando el modelo
leveneTest(Lgj3) #p-valor>0,05 No hay evidencias para rechazar el supuesto de homogeneidad de
#varianzas o que no hay evidencia de heterogeneidad de varianzas.
# p-valor<0,05 Hay evidencias para rechazar el supuesto de homogeneidad de varianzas

#'[5) Linealidad]
#'#No aplica en ese caso (cualitativa)


#'[Comprobación de todos los supuestos]
check_model(Lgj3)



#32) GRÁFICO----
#'[En este caso, el grafico de ggpredict nos va a mostrar las abundancias]
#'[MEDIAS predichas para cada grupo]

ggpredict(Lgj3, 
          terms = c("etapa") #Predictores a graficar ENTRE COMILLAS
) %>% #pipeamos para que nos ponga el objeto creado de ggpredict en plot
  plot(add.data = T) 
  ylim (0,20) #acomodo margen de y para ver los IC

emmeans(Lgj3, list(pairwise ~ etapa), 
        type = "response") 



# Gráfico en ggplot 

#crear columna predichos con valores detransformados
lagartijas$predichos <- predict(Lgj3, type= "response") 

#graficos ggplot por etapa
#defino como voy a querer mostrar las replicas (dias)
p.alineado <- position_dodge2(width = 0.9, padding = 0) #alineado horizontal
p.aleatorio <- position_jitter(w=0.5, h=0.5)

#♠ Grafico con todos los años
ggplot(lagartijas, aes(x=etapa,y=predichos,col=etapa))+
  geom_point(size=3) +
  labs(color="Year",x ="Year",y="Predicted counts of L. wiegmannii.") 

#Graficas el +/-
#En variable respuesta Log
comp3<-emmeans(Lgj3, pairwise ~ etapa) #está en log
confint(comp3) #magnitud del efecto
resumen2_Lgj3<-as.data.frame(comp3$emmeans)
ggplot(resumen2_Lgj3, aes(x=etapa, y=emmean, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos L.wiegmanni (log)") +
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), color="blue", width=0.2)+ #ver si está bien ymax y min
  geom_point(shape=15, size=3) + 
  ggtitle("Comparacion de lagartijas por etapa (log)", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.01")


#En variable respuesta no logaritmica. EXPONENCIAL
#Ojo, acá: para graficar ymin e ymax tengo que hacer la exponencial de esos valores de escala log
comp2<-emmeans(Lgj3, pairwise ~ etapa) #está en log
confint(comp2) #magnitud del efecto
exp (1.57) #2018
exp (6.29) #2019
exp (13.43) #2021
resumen_Lgj3<-as.data.frame(comp2$emmeans)
resumen_Lgj3$exponencial <-exp(resumen_Lgj3$emmean)
ggplot(resumen_Lgj3, aes(x=etapa, y=exponencial, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos L.wiegmanni") +
  geom_errorbar(aes(ymin=exponencial-SE, ymax=exponencial+SE), color="blue", width=0.2)+ #ver si está bien ymax y min
  geom_point(shape=15, size=4) + 
  ggtitle("Comparacion de gatos por etapa (log)", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.01")

#En variable respuesta no logaritmica. TYPE RESPONSE
#Ojo, acá: para graficar ymin e ymax tengo que hacer en escala response
comp4<-emmeans(Lgj3, pairwise ~ etapa, type = "response")
confint(comp4) #magnitud del efecto
resumen3_Lgj3<-as.data.frame(comp4$emmeans)
ggplot(resumen3_Lgj3, aes(x=etapa, y=response, colour=etapa)) + 
  labs(x="Year") + labs(y="Conteos L.wiegmanni") +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), color="blue", width=0.2)+ #ver si está bien ymax y min
  geom_point(shape=15, size=4) + 
  ggtitle("Comparacion de lagartijas por etapa", "Media +/- error estandar")+
  annotate("text", x=1.5, y=9, label = "p=<0.05")
