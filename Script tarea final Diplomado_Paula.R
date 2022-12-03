# ----------------------------------------------------------
# Tarea diplomado final - Script final contine análisis Componentes principales
# Dra. Paula Celis Plá 
# 30 de noviembre 2022
# Diplomado en Análisis de Datos con R e Investigación reproducible para Biociencias
# ----------------------------------------------------------

## Remover objetos de la sesión de trabajo
rm(list = ls())

# Habilita Librerias
library(stats)
library(graphics)
library(psych)
library(readxl)
library(agridat)
library(tidyr)
library(dplyr)
library(ggplot2)

## Transforma variables a factores
Datos_Proyecto <- read_excel("Datos_Proyecto.xlsx")
summary(Datos_Proyecto)
Datos_Proyecto$Seasons <- as.factor(Datos_Proyecto$Seasons)
Datos_Proyecto$time <- as.factor(Datos_Proyecto$time)
summary(Datos_Proyecto)

## Histogramas con etiquetas y títulos
p1 <- ggplot(Datos_Proyecto, aes(Chla)) + geom_histogram(bins = 8, color ="blue", fill="blue")+ labs(title="Histograma de Clorofila a", x="Clorofila a", y="Frecuencia") 
p2 <- ggplot(Datos_Proyecto, aes(Chlc)) + geom_histogram(bins = 8, color ="red", fill="red")+ labs(title="Histograma de Clorofila c", x="Clorofila c", y="Frecuencia")
p3 <- ggplot(Datos_Proyecto, aes(Car)) + geom_histogram(bins = 8, color ="green", fill="green")+ labs(title="Histograma de Carotenos", x="Carotenos", y="Frecuencia")
p4 <- ggplot(Datos_Proyecto, aes(PC)) + geom_histogram(bins = 8, color ="yellow", fill="yellow")+ labs(title="Histograma de Compuestos fenólicos", x="Compuestos fenólicos", y="Frecuencia")
p5 <- ggplot(Datos_Proyecto, aes(DPPH)) + geom_histogram(bins = 8, color ="brown", fill="brown")+ labs(title="Histograma de actividad antioxidante", x="Actividad antioxidante", y="Frecuencia")
p6 <- ggplot(Datos_Proyecto, aes(Temperature)) + geom_histogram(bins = 8, color ="pink", fill="pink")+ labs(title="Histograma de Temperatura", x="Temperature", y="Frecuencia")
p7 <- ggplot(Datos_Proyecto, aes(pH)) + geom_histogram(bins = 8, color ="blue", fill="blue")+ labs(title="Histograma de pH", x="pH", y="Frecuencia")
p8 <- ggplot(Datos_Proyecto, aes(Salinity)) + geom_histogram(bins = 8, color ="red", fill="red")+ labs(title="Histograma de Salinidad", x="Salinidad", y="Frecuencia")
p9 <- ggplot(Datos_Proyecto, aes(PAR)) + geom_histogram(bins = 8, color ="green", fill="green")+ labs(title="Histograma de Radiación PAR", x="PAR", y="Frecuencia")
gridExtra::grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
gridExtra::grid.arrange(p6, p7, p8, p9, ncol = 2)

## 4. Datos balanceados y tablas de frecuencia  ## Los datos estan balanceados
str(Datos_Proyecto)
knitr::kable(table(Datos_Proyecto$Seasons, Datos_Proyecto$time), caption = "Tabla de contingencia")

## 5. Relación entre variables cuantitativas y factores ## No se incluiran las variables Seasons, time, y Replicate porque son variables categóricas.
summary(Datos_Proyecto)

## 5. Graficas de correlación de variables continuas (pearson)
pairs.panels(Datos_Proyecto[,3:11], method = "pearson", hist.col = "blue", density = TRUE, font=4) 
pairs.panels(Datos_Proyecto[,3:7], method = "pearson", hist.col = "blue", density = TRUE, font=2) #Se muestran correlación entre las primeras 4 variables continuas
pairs.panels(Datos_Proyecto[,8:11], method = "pearson", hist.col = "blue", density = TRUE, font=2)#Se muestra correlación entre las segundas 4 variables continuas

## 5. Relación entre variables continuas y factores (boxplot)
ggplot(Datos_Proyecto, aes(x= Seasons, y=Chla))+geom_boxplot(fill="olivedrab1")+labs(title = "BoxPlot", x= "Seasons", y= "Clorofila a")
ggplot(Datos_Proyecto, aes(x= Seasons, y=Chlc))+geom_boxplot(fill="red")+labs(title = "BoxPlot", x= "Seasons", y= "Clorofila c")
ggplot(Datos_Proyecto, aes(x= Seasons, y=Car))+geom_boxplot(fill="blue")+labs(title = "BoxPlot", x= "Seasons", y= "Carotenos")
ggplot(Datos_Proyecto, aes(x= Seasons, y=PC))+geom_boxplot(fill="green")+labs(title = "BoxPlot", x= "Seasons", y= "Compuestos fenólicos")
ggplot(Datos_Proyecto, aes(x= Seasons, y=DPPH))+geom_boxplot(fill="brown")+labs(title = "BoxPlot", x= "Seasons", y= "DPPH")
ggplot(Datos_Proyecto, aes(x= Seasons, y=Temperature))+geom_boxplot(fill="blue")+labs(title = "BoxPlot", x= "Seasons", y= "Temperatura")
ggplot(Datos_Proyecto, aes(x= Seasons, y=pH))+geom_boxplot(fill="green")+labs(title = "BoxPlot", x= "Seasons", y= "pH")
ggplot(Datos_Proyecto, aes(x= Seasons, y=Salinity))+geom_boxplot(fill="brown")+labs(title = "BoxPlot", x= "Seasons", y= "Salinity")
ggplot(Datos_Proyecto, aes(x= Seasons, y=PAR))+geom_boxplot(fill="yellow")+labs(title = "BoxPlot", x= "Seasons", y= "Radiación PAR")

## 6. Identificación si existen errores, datos faltantes o error atípico
# En la variable Salinidad, existe poca dispersión de los datos en las estaciones de primavera y verano, se registran datos muy similares,
# por ellos se observó poca dispersión.
# No existen datos faltantes para cada variable.
# Los errores típicos en las variables se identifican a continuación;
# Variable Chlc : para la estación winter se registra un outlier Variable
# Car : para la estación summer se registra un outlier Variable PC : se
# registran 2 valores en Spring Variable Temperature: 3 valores en spring
# Variable pH : 5 valores en spring Variable Salinity: 3 valores en
# spring, 2 en summer y 1 en winter Variable PAR: 1 en autumn, 1 en spring y 1 en summer

## 7. Resumen de los datos con tablas y estadística descriptiva
Datos_Proyecto <- read_excel("Datos_Proyecto.xlsx" , sheet= 1)
head(Datos_Proyecto)
select(Datos_Proyecto, Chla, Chlc, Car, DPPH)
Datos_tab <- Datos_Proyecto %>% group_by(Seasons) %>% summarize(n = n(), Promedio_Chla = mean(Chla), Maximo_Chla = max(Chla), Promedio_Chlc = mean(Chlc), Maximo_Chlc = max(Chlc), Promedio_Car = mean(Car), Maximo_Car = max(Car), Promedio_PC = mean(PC), Maximo_PC = max(PC), Promedio_DPPH = mean(DPPH), Maximo_DPPH = max(DPPH), Promedio_Temperature = mean(Temperature), Maximo_Temperature = max(Temperature),Promedio_pH = mean(pH), Maximo_pH = max(pH), Promedio_Salinity = mean(Salinity), Maximo_Salinity = max(Salinity), Promedio_PAR = mean(PAR), Maximo_PAR = max(PAR))
Datos_tab
knitr::kable(Datos_tab, caption = "Tabla de medidas resumen")

Datos_tab2 <- Datos_Proyecto %>% group_by(time) %>% summarize(n = n(), Promedio_Chla = mean(Chla), Maximo_Chla = max(Chla), Promedio_Chlc = mean(Chlc), Maximo_Chlc = max(Chlc), Promedio_Car = mean(Car), Maximo_Car = max(Car), Promedio_PC = mean(PC), Maximo_PC = max(PC), Promedio_DPPH = mean(DPPH), Maximo_DPPH = max(DPPH), Promedio_Temperature = mean(Temperature), Maximo_Temperature = max(Temperature),Promedio_pH = mean(pH), Maximo_pH = max(pH), Promedio_Salinity = mean(Salinity), Maximo_Salinity = max(Salinity), Promedio_PAR = mean(PAR), Maximo_PAR = max(PAR))
Datos_tab2
knitr::kable(Datos_tab2, caption = "Tabla de medidas resumen")

## 8. Utiliza Paquetes para importar datos a R como readxl o similar y paquetes tidyr, dplyr, ggplot2
messy <- read_excel("Datos_Proyecto.xlsx")
Datos_Proyecto$Seasons <- as.factor(Datos_Proyecto$Seasons)
Datos_Proyecto$time <- as.factor(Datos_Proyecto$time)
summary(Datos_Proyecto)
summary(messy)

## 9. Proponer hipótesis y realiza análisis estadístico de los datos, incluye evaluación de supuestos si corresponde

# Hipótesis Nula: No exista una estructura de grupos separados por las variables categoricas estacionalidad y día. 
# Hipótesis alternativa: Existe una estructura de grupos separados por las variables categoricas estacionalidad y día. 

# Habilita Librerias
library(readxl)
library(ggplot2)
library(dplyr)
library(knitr)
library(pander)
library(psych) # Graficas de correlación
library(factoextra) # distancia euclideana
library(vegan) # 	Community Ecology Package: Ordination, Diversity and Dissimilarities
library(dendextend) # extiende opciones de visualización

# Importar datos proyecto.
datos_PCA <- read_excel("Datos_Proyecto.xlsx", sheet = 1)
summary(datos_PCA)
datos_PCA$Seasons <- as.factor(datos_PCA$Seasons)
datos_PCA$time <- as.factor(datos_PCA$time)
head(datos_PCA[,3:11]) %>% pander(caption ="Variables ecofisiologicas y ambientales en Lessonia spicata")
str(datos_PCA)

datos_PCA_mat <- as.matrix(datos_PCA[,-c(1:2)])
str(datos_PCA_mat)

# Correlación entre variables
pairs.panels(datos_PCA_mat[,1:5], method = "pearson")
pairs.panels(datos_PCA_mat[,6:9], method = "pearson")

# Realiza PCA
PCA_Lesso <- prcomp(datos_PCA_mat, scale = TRUE)
PCA_Lesso

# Varianza explicada
get_eigenvalue(PCA_Lesso)
fviz_eig(PCA_Lesso)

# Grafica por sitio
fviz_pca_ind(PCA_Lesso, repel = TRUE, habillage = datos_PCA$Seasons,addEllipses = TRUE, pointsize = 3)
fviz_pca_var(PCA_Lesso)
fviz_pca_biplot(PCA_Lesso, repel = TRUE, habillage = datos_PCA$Seasons,addEllipses = TRUE,pointsize = 3)

# Crea nuevas variables estandarizadas
val_estandarizado <- datos_PCA %>%
  select(Chla, Chlc, Car, PC, DPPH, Temperature, pH, Salinity, PAR) %>%
  mutate(Chla1 = (Chla - mean(Chla)) / sd(Chla),Chlc1 = (Chlc - mean(Chlc)) / sd(Chlc), Car1 = (Car - mean(Car)) / sd(Car),
         PC1 = (PC - mean(PC)) / sd(PC), DPPH1 = (DPPH - mean(DPPH)) / sd(DPPH), Temperature1 = (Temperature - mean(Temperature)) / sd(Temperature), pH1 = (pH - mean(pH)) / sd(pH), Salinity1 = (Salinity - mean(Salinity)) / sd(Salinity), PAR1 = (PAR - mean(PAR)) / sd(PAR))

# Calcula matriz de distancia
dist_euclidea <- dist(val_estandarizado[10:18]) #distancia euclidiana 

# Realiza PERMANOVA
permanova <- adonis2(dist_euclidea ~ Seasons*time , method = "bray", data=datos_PCA, permutations=999)
permanova <- adonis2(dist_euclidea ~ Seasons:time , method = "bray", data=datos_PCA, permutations=999)
permanova %>% pander()
dist_euclidea <- stats::dist(val_estandarizado[10:18], method = "euclidean")

## 10. Presenta, interpreta resultados y realiza conclusión
Cita algun articulo para la estandarización 




