#################################################################################################
#Optimización por medio del algoritmo genético NGSA-II
#Realizado por David Valle Cruz
#29 de julio de 2021 18:00 h
#################################################################################################

library(nsga2R)
library(mco)
library(tidyverse) 
library(plotly) 

fitness <- function(x){
  # Calculate f1(x)    PARA EL PIB
  f1 <- 
    -0.749554482323975 * x[1] -
    1.18886912191656  * x[2] -
    0.338628912232122 * x[3] + 
    0.139886067690921 * x[4] + 
    1.00524755557435 * x[5] - 
    0.833326688655669 * x[6] + 
    0.49999642711254 * x[7] + 
    0.344648657585942* x[8] + 
    0.204812617473497 * x[9] - 
    1.08204722687719 * x[10] + 
    0.191809999611641 * x[11] + 
    0.494525962892054 * x[12] + 
    0.56450991426266 * x[13] 
  
    
  # Calculate f2(x)    PARA LA INFLACIÓN
  f2 <- -(
    -2.22156819047987 * x[1] -
    3.52363155204507  * x[2] -
    1.00364581565737 * x[3] + 
    1.00364581565737 * x[4] + 
    2.97940449385022 * x[5] - 
    2.46985657140688 * x[6] + 
    1.481915169639 * x[7] + 
    1.02148744706403 * x[8] + 
    0.60703418726456 * x[9] - 
    3.2070273162456 * x[10] + 
    0.568496358572903 * x[11] + 
    1.46570152595334 * x[12] + 
    1.67312356647931 * x[13] )
  
    
  # Calculate f1(x)    PARA EL GINI
  f3 <- -(
    -2.72625406572344 * x[1] -
    4.32411432880628  * x[2] -
    1.23164956052561 * x[3] + 
    0.508788846940152 * x[4] + 
    3.6562522139099 * x[5] - 
    3.03094748493734 * x[6] + 
    1.81856999645502 * x[7] + 
    1.25354437355453 * x[8] + 
    0.744937485221039 * x[9] - 
    3.93558536589963 * x[10] + 
    0.697644805840313 * x[11] + 
    1.79867301007952 * x[12] + 
    2.05321625738017 * x[13] )
  
  return(c(f1,f2,f3))
}

set.seed(123)
presupuesto <- nsga2R(fn = fitness, varNo = 13, objDim = 3, generations = 62, popSize = 100, 
                      lowerBounds = rep(0, 13), upperBounds = rep(1000000, 13))

str(presupuesto)
presupuesto$parameters
res <- presupuesto$parameters[,1:13]
presupuesto$objectives
plot(presupuesto$objectives)
presupuesto$parameters[1:3, ]
presupuesto$objectives
resultado <- presupuesto$parameters[1:3, ]
colnames(resultado) <- c("Transferencias",
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
rownames(resultado) <- c("PIB",	"Inflación", "GINI")
write.csv(resultado, file = "ResultadoAG.csv") #Guardalos datos en un archivo CSV

graficar <- data.frame(
  x <- presupuesto$objectives[,1],
  y <- presupuesto$objectives[,2],
  z <- presupuesto$objectives[,3]
)
colnames(graficar) <- c("x", "y", "z")

plot_ly(graficar, x = ~x, y = ~y, z = ~z) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PIB'),
                      yaxis = list(title = 'Inflación'),
                      zaxis = list(title = 'GINI')))
