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
sqrt(var(datos$Ingreso))

##Calculo del rango de viajes 
range(datos$Viajes)

##Calculo del promedio de viajes
mean(datos$Viajes)

## varianza en la cantidad de viajes
var(datos$Viajes)

## desviación estandar de los viajes
sd(datos$Viajes)

##Calculo de coeficiente de variación para viajes
cvViajes <- sd(datos$Viajes)/mean(datos$Viajes)

## Calculo de coeficiente de variación para el ingreso
cvIngreso <- sd(datos$Ingreso)/mean(datos$Ingreso)

##Gráfico de dispersión de ingreso 
plot(x=datos$Ingreso)

## Gráfico de frecuencia de trabajadores
hist(x=datos$Trabajadores)

## Calculo de la mediana
median(datos$Ingreso)

## Histograma distribución de ingreso
hist(datos$Ingreso, breaks = seq(0,11400000, by=200000), freq = T, main="Histograma de la distribución de ingreso")

## Correlación abuelos vs viajes 
cor(datos$AdultoMayores, datos$Viajes)

## Correlación trabajadores vs viajes
cor(datos$Trabajadores, datos$Viajes)

## Correlacion trabajores vs ingreso
cor(datos$Ingreso, datos$Trabajadores)

#### REVISION MÉTODOS DE PERCENTILES

## Calculo de cuartiles 
boxplot(x=datos$Ingreso)
quantile(datos$Ingreso)
plot(x=quantile(datos$Ingreso))

## Graficamos datos atipicos 

datosAtipicos <- boxplot(datos$Ingreso, 
        main = "Gráfico de Caja de distribución de Ingresos Originales",
        boxwex = 0.5,col="skyblue", frame.plot=F)

## Revisamos valores atipicos 
datosAtipicos$out
length(datosAtipicos$out)

## Tabla de datos atipicos 
datosSuciosPercentiles <- datos[(datos$Ingreso %in% datosAtipicos$out),]

## Eliminamos los datos atipicos
datosLimpiosPercentiles <- datos[!(datos$Ingreso %in% datosAtipicos$out),]

## Gráfico de percentiles 
datosPercentilSinOut <- boxplot(datosLimpiosPercentiles$Ingreso, 
                                main = "Gráfico de Caja de distribución de Ingresos sin Out",
                                boxwex = 0.5,col="pink", frame.plot=F)


## Comparativo entre las Correlaciones de abuelos vs viajes 
cor(datos$AdultoMayores, datos$Viajes)
cor(datosLimpiosPercentiles$AdultoMayores, datosLimpiosPercentiles$Viajes)

## Comparativo entre las Correlaciones de trabajadores vs viajes
cor(datos$Trabajadores, datos$Viajes)
cor(datosLimpiosPercentiles$Trabajadores, datosLimpiosPercentiles$Viajes)

## Comparativo entre las Correlaciones de trabajores vs ingreso
cor(datos$Ingreso, datos$Trabajadores)
cor(datosLimpiosPercentiles$Ingreso, datosLimpiosPercentiles$Trabajadores)

#### REVISIÓN MÉTODO INTERVALOS DE VARIABILIDAD
mean(datos$Ingreso)
sd(datos$Ingreso)

## Intervalo que involucra el 75% de los datos δ = 2
intervalo2 <- mean(datos$Ingreso) + 2*(sd(datos$Ingreso))

## Intervalo que involucra el 88,9% de los datos δ = 3
intervalo3 <- mean(datos$Ingreso) + 3*(sd(datos$Ingreso))

## Intervalo que involucra el 93,8% de los datos δ = 4
intervalo4 <- mean(datos$Ingreso) + 4*(sd(datos$Ingreso))


##  Creación de variables con los datos candidatos a outlinier
DatosOutIntervalo2 <-datos[which(datos$Ingreso > intervalo2),names(datos)]
DatosOutIntervalo3 <-datos[which(datos$Ingreso > intervalo3),names(datos)]
DatosOutIntervalo4 <-datos[which(datos$Ingreso > intervalo4),names(datos)]

## Datos restantes de la muestra
datosIntervalo2 <-datos[which(datos$Ingreso < intervalo2),names(datos)]
datosIntervalo3 <-datos[which(datos$Ingreso < intervalo3),names(datos)]
datosIntervalo4 <-datos[which(datos$Ingreso < intervalo4),names(datos)]


## Actualización de la correlación con respecto a los intervalos

## Comparativo entre las Correlaciones de abuelos vs viajes 
cor(datos$AdultoMayores, datos$Viajes)
cor(datosIntervalo2$AdultoMayores,datosIntervalo2$Viajes)
cor(datosIntervalo3$AdultoMayores,datosIntervalo3$Viajes)
cor(datosIntervalo4$AdultoMayores,datosIntervalo4$Viajes)

## Comparativo entre las Correlaciones de trabajadores vs viajes
cor(datos$Trabajadores, datos$Viajes)
cor(datosIntervalo2$Trabajadores,datosIntervalo2$Viajes)
cor(datosIntervalo3$Trabajadores,datosIntervalo3$Viajes)
cor(datosIntervalo4$Trabajadores,datosIntervalo4$Viajes)


## Comparativo entre las Correlaciones de trabajores vs ingreso
cor(datos$Ingreso, datos$Trabajadores)
cor(datosIntervalo2$Ingreso,datosIntervalo2$Trabajadores)
cor(datosIntervalo3$Ingreso,datosIntervalo3$Trabajadores)
cor(datosIntervalo4$Ingreso,datosIntervalo4$Trabajadores)


#### REVISION MÉTODO DE VALOR Z ROBUSTO 

## Creamos una nueva columna 
datos$error <-  abs(datos$Ingreso-median(datos$Ingreso))

## Creamos valores de Z
datos$ValorZ <- datos$error/median(datos$error)

##Mediana de la muestra
median(datos$Ingreso)

##Mediana del error
median(datos$error)

#Buscamos los datos para outiliers con un filtro asignandóle un 4.5 (valor del Z) al corte
datos[which(datos$ValorZ >=4.5),names(datos)]
## de usar este se quitarían 263 registros

datos[which(datos$ValorZ >=4.5),names(datos)]

## Después de analizar existen muchos valores cercanos al 4.5 por lo que se actualiza a 5

datos2 <- datos[which(datos$ValorZ <= 5),names(datos)]

##Vemos los datos actualizados 
summary(datos2)

## Actualizaciones de los cruces

##Gráfico de dispersión de ingreso 
plot(x=datos2$Ingreso)

## Gráfico de frecuencia de trabajadores
hist(x=datos2$Trabajadores)

## Correlación abuelos vs viajes 
cor(datos2$AdultoMayores, datos2$Viajes)

## Correlación trabajadores vs viajes
cor(datos2$Trabajadores, datos2$Viajes)

## Correlacion trabajores vs ingreso
cor(datos2$Ingreso, datos2$Trabajadores)



