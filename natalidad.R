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
dfnac10 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2010/NACIM10.dbf")
dfnac11 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2011/NACIM11.dbf")
dfnac12 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2012/NACIM12.dbf")
dfnac13 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2013/NACIM13.dbf")
dfnac14 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2014/NACIM14.dbf")
dfnac15 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2015/NACIM15.dbf")
dfnac16 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2016/NACIM16.dbf")
dfnac17 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2017/NACIM17.dbf")
dfnac18 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2018/NACIM18.dbf")
dfnac19 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2019/NACIM19.dbf")
dfnac20 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2020/NACIM20.dbf")
dfnac21 <- read.dbf("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/natalidad/2021/nacim21.dbf")
pob20 <- read.csv("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/pob20.csv")
pob10 <- read.csv("C:/Users/pasha/Documents/Ibero/Ciencia de Datos/datasets-master/pob10.csv")

# mostrar datos
head(dfnac10)
tail(dfnac10)
dim(dfnac10)
head(dfnac11)
tail(dfnac11)
dim(dfnac11)
head(dfnac12)
tail(dfnac12)
dim(dfnac12)
head(dfnac13)
tail(dfnac13)
dim(dfnac13)
head(dfnac14)
tail(dfnac14)
dim(dfnac14)
head(dfnac15)
tail(dfnac15)
dim(dfnac15)
head(dfnac16)
tail(dfnac16)
dim(dfnac16)
head(dfnac17)
tail(dfnac17)
dim(dfnac17)
head(dfnac18)
tail(dfnac18)
dim(dfnac18)
head(dfnac19)
tail(dfnac19)
dim(dfnac19)
head(dfnac20)
tail(dfnac20)
dim(dfnac20)
head(dfnac21)
tail(dfnac21)
dim(dfnac21)
head(pob20)
tail(pob20)
dim(pob20)

colSums(is.na(dfnac10))
summary(dfnac10)
str(dfnac10)

#limpiar el dataframe poblacion 2010
pob10$Edad <- gsub(" a\xf1os", "", pob10$Edad)
pob10$Edad <- gsub(" A\xf1os", "", pob10$Edad)
pob10$Edad <- gsub(" A\xf1o", "", pob10$Edad)
pob10$Edad <- gsub("  y m<e1>s", "", pob10$Edad)
pobl10 <- pob10[!grepl("De \\d+ a \\d+", pob10$Edad), ]
pobl10 <- pobl10[!grepl("85 y m<e1>s", pobl10$Edad), ]

head(pobl10)
tail(pobl10)

#creando un dataframe solo para mujeres de poblacion
pobm10 <- subset(pobl10, select = c("Cve_geo", "Entidad", "Edad", "Mujer"))
#convertir los datos de la columna mujeres en entero
pobm10$Mujer <- as.integer(gsub(",","",pobm10$Mujer))
str(pobm10)
pobm10 <- pobm10[pobm10$Entidad == " Total", ]
pobm10 <- subset(pobm10, select = c("Edad", "Mujer"))
names(pobm10) <- c("Edad", "Mujeres")
rownames(pobm10) <- NULL
row.names(pobm10) <- 1:nrow(pobm10)
pobm10 <- pobm10[-1,]
row.names(pobm10) <- 1:nrow(pobm10)
pobm10 <- pobm10[-42,]
pobm10

#limpiar el dataframe poblacion 2020
pob20$Edad <- gsub(" a\xf1os", "", pob20$Edad)
pob20$Edad <- gsub(" A\xf1os", "", pob20$Edad)
pob20$Edad <- gsub("  y m<e1>s", "", pob20$Edad)
pobl20 <- pob20[!grepl("De \\d+ a \\d+", pob20$Edad), ]
pobl20 <- pobl20[!grepl("85 y m<e1>s", pobl20$Edad), ]

head(pobl20)
tail(pobl20)

#creando un dataframe solo para mujeres de poblacion
pobm20 <- subset(pobl20, select = c("Cve_geo", "Entidad", "Edad", "Mujeres"))
#convertir los datos de la columna mujeres en entero
pobm20$Mujeres <- as.integer(gsub(",","",pobm20$Mujeres))
str(pobm20)
pobm20 <- pobm20[pobm20$Entidad == " Total", ]
pobm20 <- subset(pobm20, select = c("Edad", "Mujeres"))
rownames(pobm20) <- NULL
row.names(pobm20) <- 1:nrow(pobm20)
pobm20 <- pobm20[-1,]
row.names(pobm20) <- 1:nrow(pobm20)
pobm20 <- pobm20[-42,]
pobm20

#-------------------------------------------------------AÑO2021--------------------------------------------------------------#
#conservando columnas que se utilizarán
df21 = subset(dfnac21, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df21$ENT_REGIS <- as.integer(df21$ENT_REGIS)
head(df21)
str(df21)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df21[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 230000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df21[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
              col = "skyblue", ylim = c(0, 120000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.8)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df21[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 280000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.50,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df21[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1100000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df21[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 700000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df21[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 700000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df21[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df21[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df21[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado21 <- aggregate(df21[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                          "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                 by = list(EDAD_MADN = df21$EDAD_MADN, EDAD_PADN = df21$EDAD_PADN, ENT_REGIS = df21$ENT_REGIS),
                                 FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado21) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado21)
tail(nac_por_edad_estado21)

#comprobar el total de nacimientos
suma_nacimientos21 <- sum(nac_por_edad_estado21$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos21

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer21 <- subset(nac_por_edad_estado21, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer21)
nacimientos_por_edad21 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer21, sum)
head(nacimientos_por_edad21)

# imprimir resultado
nacimientos_por_edad21

#tasa de especifica de fecundidad para 2021
# fusionar ambos dataframes en función de la edad
tasaf21 <- cbind(nacimientos_por_edad21, pobm20$Mujeres)
names(tasaf21) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf21['Tasa especifica de Fecundidad'] = (tasaf21['NACIMIENTOS'] / tasaf21['MUJERES']) * 1000
tasaf21 <- tasaf21[-42,]
tasaf21
#crear grafico
plot(tasaf21$EDAD, tasaf21$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2021", 
     main = "Tasa Especifica de Fecundidad 2021", col.main = "darkblue")

escol_tasaf21 <- merge(tasaf21, df21, by = "EDAD_MADN")
tail(escol_tasaf21)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf21[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                          "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf21[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))
#------------------------------------------------------------AÑO 2021-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2020-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df20 = subset(dfnac20, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df20$ENT_REGIS <- as.integer(df20$ENT_REGIS)
head(df20)
str(df20)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df20[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 230000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df20[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 100000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.8)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df20[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 220000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.50,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df20[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 900000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df20[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 610000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df20[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 550000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df20[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df20[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1500000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df20[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado <- aggregate(df20[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                          "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                 by = list(EDAD_MADN = df20$EDAD_MADN, EDAD_PADN = df20$EDAD_PADN, ENT_REGIS = df20$ENT_REGIS),
                                 FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado)
tail(nac_por_edad_estado)

#comprobar el total de nacimientos
suma_nacimientos <- sum(nac_por_edad_estado$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer20 <- subset(nac_por_edad_estado, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer20)
nacimientos_por_edad <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer20, sum)
head(nacimientos_por_edad)

# imprimir resultado
nacimientos_por_edad

#tasa de especifica de fecundidad para 2021
# fusionar ambos dataframes en función de la edad
tasaf20 <- cbind(nacimientos_por_edad, pobm20$Mujeres)
names(tasaf20) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf20['Tasa especifica de Fecundidad'] = (tasaf20['NACIMIENTOS'] / tasaf20['MUJERES']) * 1000
tasaf20 <- tasaf20[-42,]
tasaf20

#crear grafico
plot(tasaf20$EDAD, tasaf20$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2020", 
     main = "Tasa Especifica de Fecundidad 2020", col.main = "darkblue")

escol_tasaf20 <- merge(tasaf20, df20, by = "EDAD_MADN")
tail(escol_tasaf20)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf20[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                 "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf20[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2020-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2019-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df19 = subset(dfnac19, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df19$ENT_REGIS <- as.integer(df19$ENT_REGIS)
head(df19)
str(df19)




#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df19[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 270000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df19[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 130000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df19[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 260000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df19[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df19[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df19[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 700000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df19[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1500000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df19[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df19[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado19 <- aggregate(df19[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                          "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                 by = list(EDAD_MADN = df19$EDAD_MADN, EDAD_PADN = df19$EDAD_PADN, ENT_REGIS = df19$ENT_REGIS),
                                 FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado19) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado19)
tail(nac_por_edad_estado19)

#comprobar el total de nacimientos
suma_nacimientos19 <- sum(nac_por_edad_estado19$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos19

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer19 <- subset(nac_por_edad_estado19, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer19)
nacimientos_por_edad19 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer19, sum)
head(nacimientos_por_edad19)

# imprimir resultado
nacimientos_por_edad19

#tasa de especifica de fecundidad para 2019
# fusionar ambos dataframes en función de la edad
tasaf19 <- cbind(nacimientos_por_edad19, pobm10$Mujeres)
names(tasaf19) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf19['Tasa especifica de Fecundidad'] = (tasaf19['NACIMIENTOS'] / tasaf19['MUJERES']) * 1000
tasaf19 <- tasaf19[-42,]
tasaf19

#crear grafico
plot(tasaf19$EDAD, tasaf19$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2019", 
     main = "Tasa Especifica de Fecundidad 2019", col.main = "darkblue")

escol_tasaf19 <- merge(tasaf19, df19, by = "EDAD_MADN")
tail(escol_tasaf19)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf19[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf19[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2019-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2018-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df18 = subset(dfnac18, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df18$ENT_REGIS <- as.integer(df18$ENT_REGIS)
head(df18)
str(df18)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df18[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 280000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df18[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 135000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df18[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 260000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df18[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df18[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 820000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df18[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 725000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df18[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1600000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df18[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1900000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df18[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado18 <- aggregate(df18[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df18$EDAD_MADN, EDAD_PADN = df18$EDAD_PADN, ENT_REGIS = df18$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado18) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado18)
tail(nac_por_edad_estado18)

#comprobar el total de nacimientos
suma_nacimientos18 <- sum(nac_por_edad_estado18$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos18

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer18 <- subset(nac_por_edad_estado18, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer18)
nacimientos_por_edad18 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer18, sum)
head(nacimientos_por_edad18)

# imprimir resultado
nacimientos_por_edad18

#tasa de especifica de fecundidad para 2018
# fusionar ambos dataframes en función de la edad
tasaf18 <- cbind(nacimientos_por_edad18, pobm10$Mujeres)
names(tasaf18) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf18['Tasa especifica de Fecundidad'] = (tasaf18['NACIMIENTOS'] / tasaf18['MUJERES']) * 1000
tasaf18 <- tasaf18[-42,]
tasaf18

#crear grafico
plot(tasaf18$EDAD, tasaf18$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2018", 
     main = "Tasa Especifica de Fecundidad 2018", col.main = "darkblue")

escol_tasaf18 <- merge(tasaf18, df18, by = "EDAD_MADN")
tail(escol_tasaf18)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf18[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf18[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2018-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2017-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df17 = subset(dfnac17, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df17$ENT_REGIS <- as.integer(df17$ENT_REGIS)
head(df17)
str(df17)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df17[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df17[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 150000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df17[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 280000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df17[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df17[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 840000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df17[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 740000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df17[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df17[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1900000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df17[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado17 <- aggregate(df17[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df17$EDAD_MADN, EDAD_PADN = df17$EDAD_PADN, ENT_REGIS = df17$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado17) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado17)
tail(nac_por_edad_estado17)

#comprobar el total de nacimientos
suma_nacimientos17 <- sum(nac_por_edad_estado17$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos17

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer17 <- subset(nac_por_edad_estado17, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer17)
nacimientos_por_edad17 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer17, sum)
head(nacimientos_por_edad17)

# imprimir resultado
nacimientos_por_edad17

#tasa de especifica de fecundidad para 2017
# fusionar ambos dataframes en función de la edad
tasaf17 <- cbind(nacimientos_por_edad17, pobm10$Mujeres)
names(tasaf17) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf17['Tasa especifica de Fecundidad'] = (tasaf17['NACIMIENTOS'] / tasaf17['MUJERES']) * 1000
tasaf17 <- tasaf17[-42,]
tasaf17

#crear grafico
plot(tasaf17$EDAD, tasaf17$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2017", 
     main = "Tasa Especifica de Fecundidad 2017", col.main = "darkblue")

escol_tasaf17 <- merge(tasaf17, df17, by = "EDAD_MADN")
tail(escol_tasaf17)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf17[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf17[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))
#------------------------------------------------------------AÑO 2017-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2016-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df16 = subset(dfnac16, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df16$ENT_REGIS <- as.integer(df16$ENT_REGIS)
head(df16)
str(df16)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df16[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 320000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df16[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 150000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df16[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 290000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df16[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df16[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 850000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df16[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 750000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df16[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df16[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df16[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado16 <- aggregate(df16[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df16$EDAD_MADN, EDAD_PADN = df16$EDAD_PADN, ENT_REGIS = df16$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado16) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado16)
tail(nac_por_edad_estado16)

#comprobar el total de nacimientos
suma_nacimientos16 <- sum(nac_por_edad_estado16$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos16

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer16 <- subset(nac_por_edad_estado16, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer16)
nacimientos_por_edad16 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer16, sum)
head(nacimientos_por_edad16)

# imprimir resultado
nacimientos_por_edad16

#tasa de especifica de fecundidad para 2016
# fusionar ambos dataframes en función de la edad
tasaf16 <- cbind(nacimientos_por_edad16, pobm10$Mujeres)
names(tasaf16) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf16['Tasa especifica de Fecundidad'] = (tasaf16['NACIMIENTOS'] / tasaf16['MUJERES']) * 1000
tasaf16 <- tasaf16[-42,]
tasaf16

#crear grafico
plot(tasaf16$EDAD, tasaf16$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2016", 
     main = "Tasa Especifica de Fecundidad 2016", col.main = "darkblue")

escol_tasaf16 <- merge(tasaf16, df16, by = "EDAD_MADN")
tail(escol_tasaf16)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf16[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf16[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2016-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2015-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df15 = subset(dfnac15, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df15$ENT_REGIS <- as.integer(df15$ENT_REGIS)
head(df15)
str(df15)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df15[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 320000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df15[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 160000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df15[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 310000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df15[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1200000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df15[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 870000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df15[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 780000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df15[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1800000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df15[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2100000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df15[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado15 <- aggregate(df15[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df15$EDAD_MADN, EDAD_PADN = df15$EDAD_PADN, ENT_REGIS = df15$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado15) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado15)
tail(nac_por_edad_estado15)

#comprobar el total de nacimientos
suma_nacimientos15 <- sum(nac_por_edad_estado15$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos15

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer15 <- subset(nac_por_edad_estado15, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer15)
nacimientos_por_edad15 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer15, sum)
head(nacimientos_por_edad15)

# imprimir resultado
nacimientos_por_edad15

#tasa de especifica de fecundidad para 2015
# fusionar ambos dataframes en función de la edad
tasaf15 <- cbind(nacimientos_por_edad15, pobm10$Mujeres)
names(tasaf15) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf15['Tasa especifica de Fecundidad'] = (tasaf15['NACIMIENTOS'] / tasaf15['MUJERES']) * 1000
tasaf15 <- tasaf15[-42,]
tasaf15

#crear grafico
plot(tasaf15$EDAD, tasaf15$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2015", 
     main = "Tasa Especifica de Fecundidad 2015", col.main = "darkblue")

escol_tasaf15 <- merge(tasaf15, df15, by = "EDAD_MADN")
tail(escol_tasaf15)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf15[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf15[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2015-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2014-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df14 = subset(dfnac14, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df14$ENT_REGIS <- as.integer(df14$ENT_REGIS)
head(df14)
str(df14)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df14[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 350000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df14[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 160000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df14[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 320000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df14[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1400000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df14[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df14[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 850000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df14[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df14[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2100000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df14[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado14 <- aggregate(df14[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df14$EDAD_MADN, EDAD_PADN = df14$EDAD_PADN, ENT_REGIS = df14$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado14) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado14)
tail(nac_por_edad_estado14)

#comprobar el total de nacimientos
suma_nacimientos14 <- sum(nac_por_edad_estado14$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos14

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer14 <- subset(nac_por_edad_estado14, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer14)
nacimientos_por_edad14 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer14, sum)
head(nacimientos_por_edad14)

# imprimir resultado
nacimientos_por_edad14

#tasa de especifica de fecundidad para 2014
# fusionar ambos dataframes en función de la edad
tasaf14 <- cbind(nacimientos_por_edad14, pobm10$Mujeres)
names(tasaf14) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf14['Tasa especifica de Fecundidad'] = (tasaf14['NACIMIENTOS'] / tasaf14['MUJERES']) * 1000
tasaf14 <- tasaf14[-42,]
tasaf14

#crear grafico
plot(tasaf14$EDAD, tasaf14$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2014", 
     main = "Tasa Especifica de Fecundidad 2014", col.main = "darkblue")

escol_tasaf14 <- merge(tasaf14, df14, by = "EDAD_MADN")
tail(escol_tasaf14)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf14[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf14[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2014-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2013-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df13 = subset(dfnac13, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df13$ENT_REGIS <- as.integer(df13$ENT_REGIS)
head(df13)
str(df13)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df13[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 360000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df13[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 180000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df13[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 320000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df13[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df13[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 950000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df13[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 820000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df13[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df13[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df13[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado13 <- aggregate(df13[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df13$EDAD_MADN, EDAD_PADN = df13$EDAD_PADN, ENT_REGIS = df13$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado13) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado13)
tail(nac_por_edad_estado13)

#comprobar el total de nacimientos
suma_nacimientos13 <- sum(nac_por_edad_estado13$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos13

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer13 <- subset(nac_por_edad_estado13, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer13)
nacimientos_por_edad13 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer13, sum)
head(nacimientos_por_edad13)

# imprimir resultado
nacimientos_por_edad13

#tasa de especifica de fecundidad para 2013
# fusionar ambos dataframes en función de la edad
tasaf13 <- cbind(nacimientos_por_edad13, pobm10$Mujeres)
names(tasaf13) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf13['Tasa especifica de Fecundidad'] = (tasaf13['NACIMIENTOS'] / tasaf13['MUJERES']) * 1000
tasaf13 <- tasaf13[-42,]
tasaf13

#crear grafico
plot(tasaf13$EDAD, tasaf13$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2013", 
     main = "Tasa Especifica de Fecundidad 2013", col.main = "darkblue")

escol_tasaf13 <- merge(tasaf13, df13, by = "EDAD_MADN")
tail(escol_tasaf13)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf13[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf13[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2013-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2012-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df12 = subset(dfnac12, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df12$ENT_REGIS <- as.integer(df12$ENT_REGIS)
head(df12)
str(df12)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df12[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 360000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df12[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 180000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df12[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 380000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df12[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1400000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df12[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 950000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df12[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 830000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df12[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df12[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df12[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado12 <- aggregate(df12[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df12$EDAD_MADN, EDAD_PADN = df12$EDAD_PADN, ENT_REGIS = df12$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado12) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado12)
tail(nac_por_edad_estado12)

#comprobar el total de nacimientos
suma_nacimientos12 <- sum(nac_por_edad_estado12$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos12

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer12 <- subset(nac_por_edad_estado12, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer12)
nacimientos_por_edad12 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer12, sum)
head(nacimientos_por_edad12)

# imprimir resultado
nacimientos_por_edad12

#tasa de especifica de fecundidad para 2012
# fusionar ambos dataframes en función de la edad
tasaf12 <- cbind(nacimientos_por_edad12, pobm10$Mujeres)
names(tasaf12) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf12['Tasa especifica de Fecundidad'] = (tasaf12['NACIMIENTOS'] / tasaf12['MUJERES']) * 1000
tasaf12 <- tasaf12[-42,]
tasaf12

#crear grafico
plot(tasaf12$EDAD, tasaf12$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2012", 
     main = "Tasa Especifica de Fecundidad 2012", col.main = "darkblue")

escol_tasaf12 <- merge(tasaf12, df12, by = "EDAD_MADN")
tail(escol_tasaf12)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf12[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf12[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2012-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2011-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df11 = subset(dfnac11, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df11$ENT_REGIS <- as.integer(df11$ENT_REGIS)
head(df11)
str(df11)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df11[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 360000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df11[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 180000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df11[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 380000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df11[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1400000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df11[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 950000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df11[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 830000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df11[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df11[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df11[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado11 <- aggregate(df11[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df11$EDAD_MADN, EDAD_PADN = df11$EDAD_PADN, ENT_REGIS = df11$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado11) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado11)
tail(nac_por_edad_estado11)

#comprobar el total de nacimientos
suma_nacimientos11 <- sum(nac_por_edad_estado11$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos11

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer11 <- subset(nac_por_edad_estado11, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer11)
nacimientos_por_edad11 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer11, sum)
head(nacimientos_por_edad11)

# imprimir resultado
nacimientos_por_edad11

#tasa de especifica de fecundidad para 2011
# fusionar ambos dataframes en función de la edad
tasaf11 <- cbind(nacimientos_por_edad11, pobm10$Mujeres)
names(tasaf11) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf11['Tasa especifica de Fecundidad'] = (tasaf11['NACIMIENTOS'] / tasaf11['MUJERES']) * 1000
tasaf11 <- tasaf11[-42,]
tasaf11

#crear grafico
plot(tasaf11$EDAD, tasaf11$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2011", 
     main = "Tasa Especifica de Fecundidad 2011", col.main = "darkblue")

escol_tasaf11 <- merge(tasaf11, df11, by = "EDAD_MADN")
tail(escol_tasaf11)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf11[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf11[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2011-------------------------------------------------------------------------#

#------------------------------------------------------------AÑO 2010-------------------------------------------------------------------------#

#conservando columnas que se utilizarán
df10 = subset(dfnac10, select = c(ENT_REGIS, EDAD_MADN, EDAD_PADN, EDOCIV_MAD, ESCOL_MAD, ESCOL_PAD, ACT_MAD, ACT_PAD))
# convertir la columna ENT_REGIS a números
df10$ENT_REGIS <- as.integer(df10$ENT_REGIS)
head(df10)
str(df10)


#Mostrar cuantos nacimientos por entidad hay
tabla_ent_regis <- table(df10[,1])
print(tabla_ent_regis)
bar <- barplot(tabla_ent_regis, names.arg = 1:32, xlab = "Entidad Federativa", ylab = "Nacimientos", col = "skyblue", ylim = c(0, 360000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_ent_regis + 0.5, labels = tabla_ent_regis, pos = 3)
title(main = "Nacimientos por entidad federativa", col.main = "darkblue")

#Mostrar cuantos nacimientos por edad de madre hay
tabla_edadm <- table(df10[,2])
tabla_edadm_df <- data.frame(edad = 10:51, nacimientos = tabla_edadm)
print(tabla_edadm_df)
bar <- barplot(tabla_edadm_df$nacimientos.Freq, names.arg = tabla_edadm_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               col = "skyblue", ylim = c(0, 180000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadm_df$nacimientos.Freq + 0.05*max(tabla_edadm_df$nacimientos.Freq), labels = tabla_edadm_df$nacimientos.Freq, cex = 0.7)
title(main = "Nacimientos por edad de la madre", col.main = "darkblue")

#mostrar cuantos nacimientos por edad de padre hay
tabla_edadp <- table(df10[,3])
tabla_edadp_df <- data.frame(edad = 10:74, nacimientos = tabla_edadp)
print(tabla_edadp_df)
bar <- barplot(tabla_edadp_df$nacimientos.Freq, names.arg = tabla_edadp_df$nacimientos.Var1, xlab = "Edad", ylab = "Nacimientos", 
               main = "Nacimientos por edad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 380000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edadp_df$nacimientos.Freq + 0.05*max(tabla_edadp_df$nacimientos.Freq), labels = tabla_edadp_df$nacimientos.Freq, cex = 0.52,
     pos = 1)


#mostrar cuantos nacimientos por estado civil de la madre hay
tabla_edociv <- table(df10[,4])
tabla_edociv <- data.frame(EDO_CIV = 1:7, nacimientos = tabla_edociv)
print(tabla_edociv)
bar <- barplot(tabla_edociv$nacimientos.Freq, names.arg = tabla_edociv$nacimientos.Var1, xlab = "Estado Civil", ylab = "Nacimientos", 
               main = "Nacimientos por estado civil de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 1400000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_edociv$nacimientos.Freq + 0.05*max(tabla_edociv$nacimientos.Freq), labels = tabla_edociv$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar de la madre hay
tabla_escolm <- table(df10[,5])
tabla_escolm <- data.frame(ESCOL_MAD = 1:9, nacimientos = tabla_escolm)
print(tabla_escolm)
bar <- barplot(tabla_escolm$nacimientos.Freq, names.arg = tabla_escolm$nacimientos.Var1, xlab = "Escolaridad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 950000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolm$nacimientos.Freq + 0.05*max(tabla_escolm$nacimientos.Freq), labels = tabla_escolm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por grado escolar del padre hay
tabla_escolp <- table(df10[,6])
tabla_escolp <- data.frame(ESCOL_PAD = 1:9, nacimientos = tabla_escolp)
print(tabla_escolp)
bar <- barplot(tabla_escolp$nacimientos.Freq, names.arg = tabla_escolp$nacimientos.Var1, xlab = "Escolaridad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por escolaridad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 830000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_escolp$nacimientos.Freq + 0.05*max(tabla_escolp$nacimientos.Freq), labels = tabla_escolp$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral de la madre hay
tabla_actm <- table(df10[,7])
tabla_actm <- data.frame(ACT_MAD = 1:3, nacimientos = tabla_actm)
print(tabla_actm)
bar <- barplot(tabla_actm$nacimientos.Freq, names.arg = tabla_actm$nacimientos.Var1, xlab = "Actividad de la madre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad de la madre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2000000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actm$nacimientos.Freq + 0.05*max(tabla_actm$nacimientos.Freq), labels = tabla_actm$nacimientos.Freq, cex = 0.8)


#mostrar cuantos nacimientos por actividad laboral del padre hay
tabla_actp <- table(df10[,8])
tabla_actp <- data.frame(ACT_PAD = 1:3, nacimientos = tabla_actp)
print(tabla_actp)
bar <- barplot(tabla_actp$nacimientos.Freq, names.arg = tabla_actp$nacimientos.Var1, xlab = "Actividad del padre", ylab = "Nacimientos", 
               main = "Nacimientos por actividad del padre", col.main = "darkblue", col = "skyblue", ylim = c(0, 2300000))
grid(nx = NA, ny = NULL, lwd = 1, lty = 1, col = "gray")
text(bar, tabla_actp$nacimientos.Freq + 0.05*max(tabla_actp$nacimientos.Freq), labels = tabla_actp$nacimientos.Freq, cex = 0.8)

#mapa de calor de correlacion por columnas
corr_matrix <- cor(df10[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD")])
ggcorrplot(corr_matrix, type = "lower",
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion entre las variables") + 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradientn(colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), na.value = "gray", limits = c(0, 1))
corr_matrix


#generando un dataframe desglosando todos los campos
nac_por_edad_estado10 <- aggregate(df10[, c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                            "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD")],
                                   by = list(EDAD_MADN = df10$EDAD_MADN, EDAD_PADN = df10$EDAD_PADN, ENT_REGIS = df10$ENT_REGIS),
                                   FUN = function(x) sum(!is.na(x)))

# Asignar nombres a las columnas del nuevo data frame
names(nac_por_edad_estado10) <- c("EDAD_MADN", "EDAD_PADN", "ENT_REGIS", "EDOCIV_MAD", 
                                  "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD", "ACT_PAD", "NACIMIENTOS")
head(nac_por_edad_estado10)
tail(nac_por_edad_estado10)

#comprobar el total de nacimientos
suma_nacimientos10 <- sum(nac_por_edad_estado10$NACIMIENTOS, na.rm = TRUE)
suma_nacimientos10

#creando un dataframe solo para mujeres de nacimientos
nac_por_edad_estado_mujer10 <- subset(nac_por_edad_estado10, select = c("ENT_REGIS", "EDAD_MADN", "NACIMIENTOS"))
head(nac_por_edad_estado_mujer10)
nacimientos_por_edad10 <- aggregate(NACIMIENTOS ~ EDAD_MADN, data = nac_por_edad_estado_mujer10, sum)
head(nacimientos_por_edad10)

# imprimir resultado
nacimientos_por_edad10

#tasa de especifica de fecundidad para 2010
# fusionar ambos dataframes en función de la edad
tasaf10 <- cbind(nacimientos_por_edad10, pobm10$Mujeres)
names(tasaf10) <- c("EDAD_MADN", "NACIMIENTOS", "MUJERES")
# dividir la columna de nacimientos de cada edad entre la columna de mujeres de esa edad
tasaf10['Tasa especifica de Fecundidad'] = (tasaf10['NACIMIENTOS'] / tasaf10['MUJERES']) * 1000
tasaf10 <- tasaf10[-42,]
tasaf10

#crear grafico
plot(tasaf10$EDAD, tasaf10$'Tasa especifica de Fecundidad', type = "l", lwd = 4, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad 2010", 
     main = "Tasa Especifica de Fecundidad 2010", col.main = "darkblue")

escol_tasaf10 <- merge(tasaf10, df10, by = "EDAD_MADN")
tail(escol_tasaf10)

#mapa de calor de correlacion por columnas entre todas las variables
corr_matrix <- cor(escol_tasaf10[c("EDAD_MADN", "EDAD_PADN", "EDOCIV_MAD", "ESCOL_MAD", "ESCOL_PAD", "ACT_MAD","ACT_PAD", 
                                   "NACIMIENTOS", "Tasa especifica de Fecundidad", "ENT_REGIS")])
corr_matrix
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion todas las variables con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#mapa de calor de correlacion en el df tasa de fecundidad
# Calcular matriz de correlación
corr_matrix <- cor(tasaf10[c("EDAD_MADN", "NACIMIENTOS", "Tasa especifica de Fecundidad", "MUJERES")])
#mostrar grafico
ggcorrplot(corr_matrix, type = "lower",
           colors = c("#6D9EC1", "#FFFFFF", "#E6842A"), 
           lab = TRUE, 
           lab_size = 3.5, 
           ggtheme = ggplot2::theme_gray) + 
  ggtitle("Relacion Edad con tasa especifica de Fecundidad") + 
  theme(plot.title = element_text(hjust = 0.5))

#------------------------------------------------------------AÑO 2010-------------------------------------------------------------------------#



#fecundidad por año
# Definir límites del eje y
ylim <- range(tasaf21$'Tasa especifica de Fecundidad', tasaf20$'Tasa especifica de Fecundidad', 
              tasaf19$'Tasa especifica de Fecundidad', tasaf18$'Tasa especifica de Fecundidad',
              tasaf17$'Tasa especifica de Fecundidad', tasaf16$'Tasa especifica de Fecundidad',
              tasaf15$'Tasa especifica de Fecundidad', tasaf14$'Tasa especifica de Fecundidad', 
              tasaf13$'Tasa especifica de Fecundidad', tasaf12$'Tasa especifica de Fecundidad',
              tasaf11$'Tasa especifica de Fecundidad', tasaf10$'Tasa especifica de Fecundidad')

# Crear gráfico de líneas
plot(tasaf21$EDAD, tasaf21$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
     col="skyblue", xlab = "Edad", ylab = "Tasa especifica de Fecundidad", 
     main = "Tasa Especifica de Fecundidad 2010-2021", col.main = "darkblue", ylim = ylim)

lines(tasaf20$EDAD, tasaf20$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="red")

lines(tasaf19$EDAD, tasaf19$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="green")

lines(tasaf18$EDAD, tasaf18$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="orange")

lines(tasaf17$EDAD, tasaf17$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="purple")

lines(tasaf16$EDAD, tasaf16$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="brown")

lines(tasaf15$EDAD, tasaf15$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="yellow2")

lines(tasaf14$EDAD, tasaf14$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="gray")

lines(tasaf13$EDAD, tasaf13$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="pink")

lines(tasaf12$EDAD, tasaf12$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="cyan")

lines(tasaf11$EDAD, tasaf11$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="tomato")

lines(tasaf10$EDAD, tasaf10$'Tasa especifica de Fecundidad', type = "l", lwd = 2, 
      col="darkslategray")

legend("topright", c("2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"),
       col = c("skyblue", "red", "green","orange", "purple", "brown", "yellow2", "gray", "pink", "cyan", "tomato", "darkslategray"),
       lty = 1, bty = "n")

#dataframe de los nacimientos totales
row <- 2010:2021
suma_nacimientos <- c(suma_nacimientos10, suma_nacimientos11, suma_nacimientos12, suma_nacimientos13, suma_nacimientos14, 
                      suma_nacimientos15, suma_nacimientos16, suma_nacimientos17, suma_nacimientos18, suma_nacimientos19, 
                      suma_nacimientos, suma_nacimientos21)

nac_totales <- data.frame(AÑO=row, NACIMIENTOS=suma_nacimientos)
nac_totales

ggplot(nac_totales, aes(x = AÑO, y = NACIMIENTOS)) +
  geom_ribbon(aes(ymin = 0, ymax = NACIMIENTOS), fill = "skyblue", alpha = 0.2) +
  geom_line(color = "#00BFC4", lwd = 2) +
  geom_point(size = 4, color = "#00BFC4") +
  geom_hline(yintercept = mean(nac_totales$NACIMIENTOS), color = "darkgray", lty = 2, lwd = 1) +
  geom_text(aes(label = NACIMIENTOS), nudge_y = 10000, color = "black", fontface = "bold") +
  labs(title = "Número de nacimientos por año",
       x = "Año",
       y = "Número de nacimientos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold"))


