#instalar paquetes
install.packages("reshape2")
install.packages("dplyr")
install.packages("rlang")
install.packages("ggcorrplot")

# Cargar librerias
library(foreign)
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggcorrplot)

# Leer archivos DBF y CSV
def21 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2021/defun21.dbf")
def20 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2020/defun20.dbf")
def19 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2019-2015/defunciones_base_datos_2019/DEFUN19.dbf")
def18 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2019-2015/defunciones_base_datos_2018/DEFUN18.dbf")
def17 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2019-2015/defunciones_base_datos_2017/DEFUN17.dbf")
def16 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2019-2015/defunciones_base_datos_2016/DEFUN16.dbf")
def15 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2019-2015/defunciones_base_datos_2015/DEFUN15.dbf")
def14 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2014-2010/defunciones_base_datos_2014_dbf/DEFUN14.dbf")
def13 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2014-2010/defunciones_base_datos_2013_dbf/DEFUN13.dbf")
def12 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2014-2010/defunciones_base_datos_2012_dbf/DEFUN12.dbf")
def11 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2014-2010/defunciones_base_datos_2011_dbf/DEFUN11.dbf")
def10 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2014-2010/defunciones_base_datos_2010_dbf/DEFUN10.dbf")
def09 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2009-2005/defunciones_base_datos_2009_dbf/DEFUN09.dbf")
def08 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2009-2005/defunciones_base_datos_2008_dbf/DEFUN08.dbf")
def07 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2009-2005/defunciones_base_datos_2007_dbf/DEFUN07.dbf")
def06 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2009-2005/defunciones_base_datos_2006_dbf/DEFUN06.dbf")
def05 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2009-2005/defunciones_base_datos_2005_dbf/DEFUN05.dbf")
def04 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2004-2000/defunciones_base_datos_2004_dbf/DEFUN04.dbf")
def03 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2004-2000/defunciones_base_datos_2003_dbf/DEFUN03.dbf")
def02 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2004-2000/defunciones_base_datos_2002_dbf/DEFUN02.dbf")
def01 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2004-2000/defunciones_base_datos_2001_dbf/DEFUN01.dbf")
def00 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2004-2000/defunciones_base_datos_2000_dbf/DEFUN00.dbf")
causadef <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/mortalidad/2021/LISTAMEX.dbf")

#mostrar datos
#cambiar df segun el año que se quiera visualizar 
head(def21)
tail(def21)
dim(def21)
str(def21)
colSums(is.na(def21))
summary(def21)

causadef

#eliminar columnas no necesarias
m21 = subset(def21, select = c(ENT_REGIS, LISTA_MEX, SEXO, EDAD, DERECHOHAB))
m21
m2021 <- m21 %>% mutate(EDAD = ifelse(EDAD < 4001, 0, EDAD))
m2021 <- m2021 %>% mutate(EDAD = ifelse(EDAD != 0, EDAD - 4000, 0))
m2021
unique(m2021$EDAD)
summary(m2021)

#ya que cada fila representa una muerte se hace el calculo de muertes totales por año
muertes21 <- nrow(def21)
muertes20 <- nrow(def20)
muertes19 <- nrow(def19)
muertes18 <- nrow(def18)
muertes17 <- nrow(def17)
muertes16 <- nrow(def16)
muertes15 <- nrow(def15)
muertes14 <- nrow(def14)
muertes13 <- nrow(def13)
muertes12 <- nrow(def12)
muertes11 <- nrow(def11)
muertes10 <- nrow(def10)
muertes09 <- nrow(def09)
muertes08 <- nrow(def08)
muertes07 <- nrow(def07)
muertes06 <- nrow(def06)
muertes05 <- nrow(def05)
muertes04 <- nrow(def04)
muertes03 <- nrow(def03)
muertes02 <- nrow(def02)
muertes01 <- nrow(def01)
muertes00 <- nrow(def00)

#tomamos datos totales de los censos
poblacion20 <- 126014024L
poblacion10 <- 112336539L
poblacion00 <- 98785000L

#crear df de muertes por año
años <- c(2000:2021)
muertes <- c(muertes00, muertes01, muertes02, muertes03, muertes04, muertes05, muertes06, muertes07, muertes08, muertes09, muertes10, muertes11, muertes12, muertes13, muertes14, muertes15, muertes16, muertes17, muertes18, muertes19, muertes20, muertes21)
df_muertes <- data.frame(AÑO = años, MUERTES = muertes)
df_muertes

#calcular la tasa de mortalidad 
# Crear vectores de años y tasa de mortalidad
AÑO <- seq(2000, 2021, 1)
tasas <- rep(0, length(AÑO))
df_tasamort <- data.frame(AÑO, tasas)

tasas[1] = df_muertes$MUERTES[1] / 98785000 * 1000
tasas[2] = df_muertes$MUERTES[2] / 100105000 * 1000
tasas[3] = df_muertes$MUERTES[3] / 101494000 * 1000
tasas[4] = df_muertes$MUERTES[4] / 102890000 * 1000
tasas[5] = df_muertes$MUERTES[5] / 104272000 * 1000
tasas[6] = df_muertes$MUERTES[6] / 105669000 * 1000
tasas[7] = df_muertes$MUERTES[7] / 107155000 * 1000
tasas[8] = df_muertes$MUERTES[8] / 108745000 * 1000
tasas[9] = df_muertes$MUERTES[9] / 110405000 * 1000
tasas[10] = df_muertes$MUERTES[10] / 112095000 * 1000
tasas[11] = df_muertes$MUERTES[11] / 113749000 * 1000
tasas[12] = df_muertes$MUERTES[12] / 115367000 * 1000
tasas[13] = df_muertes$MUERTES[13] / 116936000 * 1000
tasas[14] = df_muertes$MUERTES[14] / 118454000 * 1000
tasas[15] = df_muertes$MUERTES[15] / 119936000 * 1000
tasas[16] = df_muertes$MUERTES[16] / 121348000 * 1000
tasas[17] = df_muertes$MUERTES[17] / 122715000 * 1000
tasas[18] = df_muertes$MUERTES[18] / 124042000 * 1000
tasas[19] = df_muertes$MUERTES[19] / 125328000 * 1000
tasas[20] = df_muertes$MUERTES[20] / 126578000 * 1000
tasas[21] = df_muertes$MUERTES[21] / 127792000 * 1000
tasas[22] = df_muertes$MUERTES[22] / 126705138 * 1000
df_tasamort <- data.frame(AÑO, tasas)
df_tasamort

#----------------------------------------------------------tasa mortalidad-----------------------------------------------------------

# Crear grafica para la tasa de mortalidad
ggplot(df_tasamort, aes(x=AÑO, y=tasas)) +
  
  # Agregar la capa de línea con una línea más gruesa y de color azul oscuro
  geom_line(color="skyblue", size=1.5) +
  
  # Agregar un sombreado debajo de la línea
  geom_ribbon(aes(ymin=0, ymax=tasas), fill="#0072B2", alpha=0.05) +
  
  # Personalizar los ejes y el título con una fuente más grande
  labs(x="Año", y="Tasa de mortalidad (por 1000 habitantes)", title="Tasa de mortalidad por año",
       subtitle="Fuente: Instituto Nacional de Estadística y Geografía (INEGI)") +
  theme(plot.title=element_text(size=18, face="bold"),
        plot.subtitle=element_text(size=14, face="italic"),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=1)) +
  
  # Cambiar el color y la forma del punto de 2021 y agregar una leyenda
  geom_point(data=df_tasamort[df_tasamort$AÑO==2021,], aes(x=AÑO, y=tasas), color="#E69F00", size=3, shape=17,
             show.legend = TRUE) +
  scale_color_manual(values=c("#0072B2", "#E69F00")) +
  guides(color=guide_legend(title=NULL)) +
  
  # Cambiar el tamaño del gráfico
  theme(plot.margin = margin(30, 30, 30, 30, "pt"))

# Crear para las muertes
ggplot(df_muertes, aes(x=AÑO, y=MUERTES)) +
  
  # Agregar la capa de línea con una línea más gruesa y de color azul oscuro
  geom_line(color="skyblue", size=1.5) +
  
  # Agregar un sombreado debajo de la línea
  geom_ribbon(aes(ymin=0, ymax=MUERTES), fill="#0072B2", alpha=0.05) +
  
  # Personalizar los ejes y el título con una fuente más grande
  labs(x="Año", y="Muertes", title="Muertes por año 2000-2021",
       subtitle="Fuente: Instituto Nacional de Estadística y Geografía (INEGI)") +
  theme(plot.title=element_text(size=18, face="bold"),
        plot.subtitle=element_text(size=14, face="italic"),
        axis.title=element_text(size=16, face="bold"),
        axis.text=element_text(size=14),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        panel.border=element_rect(color="black", fill=NA, size=1)) +
  
  # Cambiar el color y la forma del punto de 2021 y agregar una leyenda
  geom_point(data=df_muertes[df_muertes$AÑO==2021,], aes(x=AÑO, y=MUERTES), color="#E69F00", size=3, shape=17,
             show.legend = TRUE) +
  scale_color_manual(values=c("#0072B2", "#E69F00")) +
  guides(color=guide_legend(title=NULL)) +
  
  # Cambiar el tamaño del gráfico
  theme(plot.margin = margin(30, 30, 30, 30, "pt"))


#----------------------------------------------------------2021-----------------------------------------------------------

# Crear tabla de frecuencia por DERECHOHAB
tabla_dh <- table(m2021$DERECHOHAB)

# Convertir tabla en data frame
df_dh <- data.frame(DERECHOHAB = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$DERECHOHAB, xlab = "Tipo Derecho habiente", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 0.05*max(df_dh$Frecuencia), labels = df_dh$Frecuencia, cex = 0.8)
title(main = "Muertes segun su servicio de salud", col.main = "darkblue")

# Crear tabla de frecuencia por enfermedad
tabla_frecuencias <- table(m2021$LISTA_MEX)
df_frecuencias <- as.data.frame(tabla_frecuencias)
df_frecuencias

#ver las 10 enfermedades con mas muertes
tabla_frec <- table(m2021$LISTA_MEX)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(LISTA_MEX = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Causa de muerte", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Causantes de más muertes en México", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por EDAD
tabla_dh <- table(m2021$EDAD)

# Convertir tabla en data frame
df_dh <- data.frame(EDAD = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$EDAD, xlab = "Edad", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
title(main = "Muertes segun su edad", col.main = "darkblue")

#ver las 10 edades con mas muertes
tabla_frec <- table(m2021$EDAD)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(EDAD = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Edad", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Más muertes por edad", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por Entidad federativa
tabla_dh <- table(m2021$ENT_REGIS)

# Convertir tabla en data frame
df_dh <- data.frame(ENT_REGIS = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$ENT_REGIS, xlab = "Entidad Federativa", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 1000, labels = df_dh$Frecuencia, cex = 0.8, pos = 3)
title(main = "Muertes segun su estado", col.main = "darkblue")

#----------------------------------------------------------2020----------------------------------------------------------------
#eliminar columnas no necesarias
m20 = subset(def20, select = c(ENT_REGIS, LISTA_MEX, SEXO, EDAD, DERECHOHAB))
m20
m2020 <- m20 %>% mutate(EDAD = ifelse(EDAD < 4001, 0, EDAD))
m2020 <- m2020 %>% mutate(EDAD = ifelse(EDAD != 0, EDAD - 4000, 0))
m2020
unique(m2020$EDAD)
summary(m2020)

# Crear tabla de frecuencia por DERECHOHAB
tabla_dh <- table(m2020$DERECHOHAB)

# Convertir tabla en data frame
df_dh <- data.frame(DERECHOHAB = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$DERECHOHAB, xlab = "Tipo Derecho habiente", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 0.05*max(df_dh$Frecuencia), labels = df_dh$Frecuencia, cex = 0.8)
title(main = "Muertes segun su servicio de salud", col.main = "darkblue")

# Crear tabla de frecuencia por enfermedad
tabla_frecuencias <- table(m2020$LISTA_MEX)
df_frecuencias <- as.data.frame(tabla_frecuencias)
df_frecuencias

#ver las 10 enfermedades con mas muertes
tabla_frec <- table(m2020$LISTA_MEX)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(LISTA_MEX = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Causa de muerte", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Causantes de más muertes en México", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por EDAD
tabla_dh <- table(m2020$EDAD)

# Convertir tabla en data frame
df_dh <- data.frame(EDAD = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$EDAD, xlab = "Edad", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
title(main = "Muertes segun su edad", col.main = "darkblue")

#ver las 10 edades con mas muertes
tabla_frec <- table(m2020$EDAD)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(EDAD = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Edad", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Más muertes por edad", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por Entidad federativa
tabla_dh <- table(m2020$ENT_REGIS)

# Convertir tabla en data frame
df_dh <- data.frame(ENT_REGIS = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$ENT_REGIS, xlab = "Entidad Federativa", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 1000, labels = df_dh$Frecuencia, cex = 0.8, pos = 3)
title(main = "Muertes segun su estado", col.main = "darkblue")
#----------------------------------------------------------2019----------------------------------------------------------------
#eliminar columnas no necesarias
m19 = subset(def19, select = c(ENT_REGIS, LISTA_MEX, SEXO, EDAD, DERECHOHAB))
m19
m2019 <- m19 %>% mutate(EDAD = ifelse(EDAD < 4001, 0, EDAD))
m2019 <- m2019 %>% mutate(EDAD = ifelse(EDAD != 0, EDAD - 4000, 0))
m2019
unique(m2019$EDAD)
summary(m2019)

# Crear tabla de frecuencia por DERECHOHAB
tabla_dh <- table(m2019$DERECHOHAB)

# Convertir tabla en data frame
df_dh <- data.frame(DERECHOHAB = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$DERECHOHAB, xlab = "Tipo Derecho habiente", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 0.05*max(df_dh$Frecuencia), labels = df_dh$Frecuencia, cex = 0.8)
title(main = "Muertes segun su servicio de salud", col.main = "darkblue")

# Crear tabla de frecuencia por enfermedad
tabla_frecuencias <- table(m2019$LISTA_MEX)
df_frecuencias <- as.data.frame(tabla_frecuencias)
df_frecuencias

#ver las 10 enfermedades con mas muertes
tabla_frec <- table(m2019$LISTA_MEX)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(LISTA_MEX = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Causa de muerte", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Causantes de más muertes en México", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por EDAD
tabla_dh <- table(m2019$EDAD)

# Convertir tabla en data frame
df_dh <- data.frame(EDAD = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$EDAD, xlab = "Edad", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
title(main = "Muertes segun su edad", col.main = "darkblue")

#ver las 10 edades con mas muertes
tabla_frec <- table(m2019$EDAD)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(EDAD = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Edad", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Más muertes por edad", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por Entidad federativa
tabla_dh <- table(m2019$ENT_REGIS)

# Convertir tabla en data frame
df_dh <- data.frame(ENT_REGIS = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$ENT_REGIS, xlab = "Entidad Federativa", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 1000, labels = df_dh$Frecuencia, cex = 0.8, pos = 3)
title(main = "Muertes segun su estado", col.main = "darkblue")
#----------------------------------------------------------2010-----------------------------------------------------------

#eliminar columnas no necesarias
m10 = subset(def10, select = c(ENT_REGIS, LISTA_MEX, SEXO, EDAD, DERECHOHAB))
m10
m2010 <- m10 %>% mutate(EDAD = ifelse(EDAD < 4001, 0, EDAD))
m2010 <- m2010 %>% mutate(EDAD = ifelse(EDAD != 0, EDAD - 4000, 0))
m2010
unique(m2010$EDAD)
summary(m2010)

# Crear tabla de frecuencia por DERECHOHAB
tabla_dh <- table(m2010$DERECHOHAB)

# Convertir tabla en data frame
df_dh <- data.frame(DERECHOHAB = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$DERECHOHAB, xlab = "Tipo Derecho habiente", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 0.05*max(df_dh$Frecuencia), labels = df_dh$Frecuencia, cex = 0.8)
title(main = "Muertes segun su servicio de salud", col.main = "darkblue")

# Crear tabla de frecuencia por enfermedad
tabla_frecuencias <- table(m2010$LISTA_MEX)
df_frecuencias <- as.data.frame(tabla_frecuencias)
df_frecuencias

#ver las 10 enfermedades con mas muertes
tabla_frec <- table(m2010$LISTA_MEX)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(LISTA_MEX = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Causa de muerte", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Causantes de más muertes en México", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por EDAD
tabla_dh <- table(m2010$EDAD)

# Convertir tabla en data frame
df_dh <- data.frame(EDAD = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$EDAD, xlab = "Edad", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
title(main = "Muertes segun su edad", col.main = "darkblue")

#ver las 10 edades con mas muertes
tabla_frec <- table(m2010$EDAD)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(EDAD = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Edad", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Más muertes por edad", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por Entidad federativa
tabla_dh <- table(m2010$ENT_REGIS)

# Convertir tabla en data frame
df_dh <- data.frame(ENT_REGIS = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$ENT_REGIS, xlab = "Entidad Federativa", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 1000, labels = df_dh$Frecuencia, cex = 0.8, pos = 3)
title(main = "Muertes segun su estado", col.main = "darkblue")

#----------------------------------------------------------2000-----------------------------------------------------------
#eliminar columnas no necesarias
m00 = subset(def00, select = c(ENT_REGIS, LISTA_MEX, SEXO, EDAD, DERECHOHAB))
m00
m2000 <- m00 %>% mutate(EDAD = ifelse(EDAD < 4001, 0, EDAD))
m2000 <- m2000 %>% mutate(EDAD = ifelse(EDAD != 0, EDAD - 4000, 0))
m2000
unique(m2000$EDAD)
summary(m2000)

# Crear tabla de frecuencia por DERECHOHAB
tabla_dh <- table(m2000$DERECHOHAB)

# Convertir tabla en data frame
df_dh <- data.frame(DERECHOHAB = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$DERECHOHAB, xlab = "Tipo Derecho habiente", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 0.05*max(df_dh$Frecuencia), labels = df_dh$Frecuencia, cex = 0.8)
title(main = "Muertes segun su servicio de salud", col.main = "darkblue")

# Crear tabla de frecuencia por enfermedad
tabla_frecuencias <- table(m2000$LISTA_MEX)
df_frecuencias <- as.data.frame(tabla_frecuencias)
df_frecuencias

#ver las 00 enfermedades con mas muertes
tabla_frec <- table(m2000$LISTA_MEX)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(LISTA_MEX = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Causa de muerte", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Causantes de más muertes en México", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por EDAD
tabla_dh <- table(m2000$EDAD)

# Convertir tabla en data frame
df_dh <- data.frame(EDAD = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$EDAD, xlab = "Edad", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
title(main = "Muertes segun su edad", col.main = "darkblue")

#ver las 10 edades con mas muertes
tabla_frec <- table(m2000$EDAD)
top_10 <- head(sort(tabla_frec, decreasing = TRUE), 10)
df_top10 <- data.frame(EDAD = names(top_10), Frecuencia = as.numeric(top_10))
top_10

bar <- barplot(top_10, col = "skyblue", ylim = c(0, max(top_10) + 30000), 
               xlab = "Edad", ylab = "Número de muertes")
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, top_10 + 1000, labels = top_10, cex = 0.8, pos = 3)
title(main = "Más muertes por edad", col.main = "darkblue", cex.main = 1.2)

# Crear tabla de frecuencia por Entidad federativa
tabla_dh <- table(m2000$ENT_REGIS)

# Convertir tabla en data frame
df_dh <- data.frame(ENT_REGIS = names(tabla_dh), Frecuencia = as.numeric(tabla_dh))

# Crear gráfico de barras
bar <- barplot(df_dh$Frecuencia, names.arg = df_dh$ENT_REGIS, xlab = "Entidad Federativa", ylab = "Número de muertes", 
               col = "skyblue", ylim = c(0, max(df_dh$Frecuencia) + 30000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, df_dh$Frecuencia + 1000, labels = df_dh$Frecuencia, cex = 0.8, pos = 3)
title(main = "Muertes segun su estado", col.main = "darkblue")