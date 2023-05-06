#################################################################################################
#Cargar datos del Banco Mundial y CONEVAL para pobreza, para datos calculados con regresión polinomial 
#Realizado por David Valle Cruz
#25 de julio de 2021 17:00 h
#################################################################################################

#install.packages("readxl") #Instalar la librería para cargar archivos de Excel, solo la primera vez

#IMPORTANTE
#Colocar una hoja de cálculo al principio con datos que no se vayan a utilizar
#No toma en cuenta la primera hoja de cálculo

library(readxl) #Cargar librería para cargar archivos de Excel
library(dplyr)

hojas <- excel_sheets("DATOS MX.xlsx") #Mostrar los nombres de las hojas de cálculo del libro de Excel
length(hojas)

dfTodo <- NULL
#Carga todos los datos de cada hoja de cálculo en un solo dataframe
for (i in 1:length(hojas)){    
  df <- read_excel("DATOS MX.xlsx", hojas[i])
  dfTodo = rbind(dfTodo, df)
}

#Hace una lista con los países del reporte
paises <- distinct(dfTodo, dfTodo$`Country Name`) 
colnames(paises) <- c("País") #Cambia el nomnbres del campo del dataframe para poder usarlo
VecPai <- as.vector(paises$País) #Crea un vector de países

dfTodoTranspuesto <- NULL
for (i in 1:length(VecPai)){
  print(VecPai[i])   #Imprime el pais que está cargando, solo visual 
  
  dfPrueba1 <- dfTodo[dfTodo$`Country Name`== VecPai[i],]   #Extrae la información de cada pais
  df_transpose <- data.frame(t(dfPrueba1[c(-1,-2, -3, -4)]))  #Elimina las columnas que no son de datos numéricos y transpone el dataframe
  Year <- rownames(df_transpose) #Genera un arreglo con los años cargados
  df_nombres <- data.frame(t(dfPrueba1[c(-1,-2, -4)]))  #Este dataframe transpuesto sirve para obtener los nombres de las variables
  nombres <- df_nombres[1,]   #Obtiene los nombres de las variables 
  colnames(df_transpose) <- nombres #Coloca los nombres de las varables 
  Country <- rep(VecPai[i], dim(df_transpose)[1])    #Agrega un campo con el nombre del pais repetido por cada año
  df_transpose<-cbind(df_transpose,Country)     #Agrega la columna del país 
  df_transpose<-cbind(df_transpose,Year)        #Agrega la columna del año
  
  dfTodoTranspuesto <- rbind(dfTodoTranspuesto, df_transpose)
}

write.csv(dfTodoTranspuesto, file = "DatosModelo.csv") #Guardalos datos en un archivo CSV

dfTodoTranspuesto <- read.csv2("DatosModelo.csv", header = TRUE, sep = ",") #Cargar el archivo guardado


#Modelos de regresión y RNA - No se usa
library(mlbench)
summary(dfTodoTranspuesto)

#Campos a eliminar para tratamiento solo numérico
borrar <- c("Country","Year")
dfnorm <- dfTodoTranspuesto[ , !(names(dfTodoTranspuesto) %in% borrar)]

#Revisar la calidad de los datos
library(VIM)
miss <- aggr(dfnorm, col=c('green', 'red'), ylab = c("Histograma de NAs", "Patrón"))
summary(miss)


#replace_outliers <- function(dfnorm, removeNA = TRUE){
#  qrts <- quantile(x, probs = c(0.25, 0.75), na.rm = removeNA)
#  caps <- quantile(x, probs = c(.05, 0.95), na.rm = removeNA)
#  iqr <- qrts[2]-qrts[1]
#  h <- 1.5*iqr
#  x[x<qrts[1]-h] <- caps[1]
#  x[x>qrts[2]+h] <- caps[2]
#  x
#}

#min(dfnorm)

#dfnorm[is.na(dfnorm)] <- 0   #Quitar valores perdidos y colocarles 0
#write.csv(dfnorm, file = "DatosModeloNorm.csv") #Guardalos datos en un archivo CSV
#dfnorm <- read.csv2("DatosModeloNorm.csv", header = TRUE, sep = ",")
#dfnorm <- dfnorm[,-1] #Quita la primera columna 

#minimo <- min(dfnorm)  #Guardar el valor mínimo de todos los datos
#minimo <- -7

#dfnorm[dfnorm==0] <- minimo - 0.1    #En donde hay ceros colocar el mínimo valor menos 0.1


dfnorm <- read.csv("DatosModeloNorm.csv", sep = ",")

library(scales)
dfnorm <- scale(dfnorm, center = TRUE, scale = TRUE)#Normalizar valores

write.csv(dfnorm, file = "DatosModeloNorm.csv") #Guardalos datos en un archivo CSV
