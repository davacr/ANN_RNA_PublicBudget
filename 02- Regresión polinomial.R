#################################################################################################
#Realizar regresión polinommial para interpolar los datos faltantes
#Realizado por David Valle Cruz
#26 de julio de 2021 13:00 h
#################################################################################################
library(readxl) #Cargar librería para cargar archivos de Excel
library(dplyr)

hojas <- excel_sheets("DATOS MX.xlsx") #Mostrar los nombres de las hojas de cálculo del libro de Excel
length(hojas)

df <- read_excel("DATOS MX.xlsx", hojas[6], col_names = FALSE )
datos <- df[c(-1, -2, -3, -4)]
datos <- t(datos)
datos <- as.data.frame(datos)
colnames(datos)[1] <- "Año"
colnames(datos)[2] <- "Valor"

library(ggplot2)
ggplot(datos, aes(x=Año, y=Valor)) + 
  geom_point() + theme_light()


mod1 <- lm(Valor ~ Año, data=datos)
mod2 <- lm(Valor ~ Año + I(Año^2), data=datos)
mod3 <- lm(Valor ~ Año + I(Año^2) + I(Año^3), data=datos)
mod4 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4), data=datos)
mod5 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5), data=datos)
mod6 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5) + I(Año^6), data=datos)
mod7 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5) + I(Año^6) + I(Año^7), data=datos)
mod8 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5) + I(Año^6) + I(Año^7) + I(Año^8), data=datos)
mod9 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5) + I(Año^6) + I(Año^7) + I(Año^8) + I(Año^9), data=datos)
mod10 <- lm(Valor ~ Año + I(Año^2) + I(Año^3) + I(Año^4) + I(Año^5) + I(Año^6) + I(Año^7) + I(Año^8) + I(Año^9) + I(Año^10), data=datos)

ggplot(datos, aes(x=Año, y=Valor)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, col='tomato') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3), se=FALSE, col='black') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4), se=FALSE, col='orange') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5), se=FALSE, col='red') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6), se=FALSE, col='yellow') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7), se=FALSE, col='grey') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8), se=FALSE, col='dark red') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9), se=FALSE, col='brown') +
  geom_smooth(method='lm', formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7)+I(x^8)+I(x^9)+I(x^10), se=FALSE, col='green') +
  theme_light()

anova(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
resumen <- anova(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10)
resmodels <- data.frame(modelo = c(1:10), SCE = resumen$`Sum of Sq`, Significancia = resumen$`Pr(>F)`)
resmodels
write.csv(resmodels, file = "06-ResModPIB.csv") #Guardalos datos en un archivo CSV

mod2$coefficients
write.csv(mod2$coefficients, file = "06-ResCoePIB.csv") #Guardalos datos en un archivo CSV
