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

## Gráfico de frecuencia de trabajadores
hist(x=datos$Trabajadores)

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
plot(x=quantile(datos$Ingreso))

## Valor Z robusto

## Creamos una nueva columna 
datos$error <-  abs(datos$Ingreso-median(datos$Ingreso))

## Creamos valores de Z
datos$ValorZ <- datos$error/median(datos$error)

##Mediana de la muestra
median(datos$Ingreso)

##Mediana del error
median(datos$error)

#Buscamos los datos para outiliers con un filtro asignandóle un 4.5 (valor del Z) al corte
datos[which(datos$ValorZ >= 4.5),names(datos)]
## de usar este se quitarían 263 registros

## Después de analizar existen muchos valores cercanos al 4.5 por lo que se actualiza a 5

datos2 <- datos[which(datos$ValorZ >= 5),names(datos)]

##Vemos los datos actualizados 
summary(datos2)




