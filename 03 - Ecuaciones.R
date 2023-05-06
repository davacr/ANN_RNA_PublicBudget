#################################################################################################
#Cargar pesos de las RNA
#Realizado por David Valle Cruz
#28 de julio de 2021 13:00 h
#################################################################################################

#install.packages("readxl") #Instalar la librería para cargar archivos de Excel, solo la primera vez

library(readxl) #Cargar librería para cargar archivos de Excel

hojas <- excel_sheets("Pesos - Ecuaciones .xlsx") #Mostrar los nombres de las hojas de cálculo del libro de Excel
length(hojas)

capa1 <- read_excel("Pesos - Ecuaciones .xlsx", hojas[1])

capa2 <- read_excel("Pesos - Ecuaciones .xlsx", hojas[2])
salida  <- read_excel("Pesos - Ecuaciones .xlsx", hojas[3])

wcapa1 <- data.matrix(capa1)
dim(wcapa1)
wcapa2 <- data.matrix(capa2)
dim(wcapa2)

pesos <- wcapa1 %*% wcapa2
dim(pesos)

salida <- data.matrix(salida)
dim(salida)

pesos <- pesos %*% salida

#Hace una lista con los nombres de variables
nombres <- c("Transferencias",
             "Deuda.externa",
             "Seguridad",
             "Industria", 
             "Agricultura",
             "Educación",
             "Pobreza",
             "Crecimiento.poblacional",
             "IyD",
             "Gastos.Gobierno",
             "Desempleo",
             "Ahorro.bruto",
             "Salud")
#colnames(nombres) <- c("Nombre") #Cambia el nombre del campo
#VecNom <- as.vector(nombres$Nombre) #Crea un vector de nombres
rownames(pesos) <- nombres

write.csv(pesos, file = "Ecuaciones.csv") #Guardalos datos en un archivo CSV
