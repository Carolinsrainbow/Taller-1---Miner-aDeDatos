## Minería de Datos
## Carolina Herrera Azolas 


## Incorporación de los datos a la biblioteca 
datos <- read.csv("/Users/user/Desktop/DatosViajes.csv",sep=";", header=TRUE)

##Visualización de los primeros 6 datos 
head(datos)

## Resumen estadístico de la base de datos
summary(datos)

## Calcular la cantidad de comunas 
length(unique(datos$Comuna))

## Calculo de los promedios de la muestra 
mean(datos$Ingreso)

## Rango de ingreso
range(datos$Ingreso)

#Varianza de ingreso
var(datos$Ingreso)

## Desviación estandar 
sd(datos$Ingreso)

##Gráfico de dispersión de ingreso 
plot(x=datos$Ingreso)

## Gráfico de tabla de ingreso vs comuna
hist(datos$Comuna)

## Calculo de la mediana
median(datos$Ingreso)

## Correlación abuelos vs viajes 
cor(datos$AdultoMayores, datos$Viajes)

## Correlación trabajadores vs viajes
cor(datos$Trabajadores, datos$Viajes)

## Correlacion trabajores vs ingreso
cor(datos$Ingreso, datos$Trabajadores)

## Calculo de cuartiles 
quantile(datos$Ingreso)

## Valor Z robusto

## Creamos una nueva columna 
datos$error <-  abs(datos$Ingreso-median(datos$Ingreso))

## Creamos una nueva columna para ingresar Valor Z
View(datos)
