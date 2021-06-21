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

##Gráfico de dispersión
plot(x=datos$Ingreso)

## Calculo de la mediana
median(datos$Ingreso)

## Correlación abuelos vs viajes 
cor(datos$AdultoMayores, datos$Viajes)

## Correlación trabajadores vs viajes
cor(datos$Trabajadores, datos$Viajes)

## Correlacion trabajores vs ingreso
cor(datos$Ingreso, datos$Trabajadores)

## Valor Z robusto

