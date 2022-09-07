install.packages('RPostgres')
library(DBI)
con<-dbConnect(RPostgres::Postgres(),dbname='postgres',host='localhost',port=5432,user='postgres',password='Sharyngan36')
dbListTables(con) #Listamos las tablas
res1<-dbSendQuery(con,"Select * from seriestemporales")
datos<-dbFetch(res1)
dbClearResult(res1)
dim(datos)
head(datos) #Hemos obtenido los datos correctamente

#FINALIZAMOS LA OBTENCIÓN DE DATOS

#EJERCICIO PARA LA VARIABLE SST

#Graficamos la serie temporal para la tercera variable
install.packages('forecast')
library(forecast)
ts_sst<-ts(datos$sst, start = c(1982,1), frequency=365.25)
tsoutliers(ts_sst)
plot(ts_sst)
#Calculamos la función de autocorrelación de la serie temporal inicial
acf(ts_sst)
#Usamos Diferenciación (Primera Diferencia de sst)
sst_diff1<-diff(ts_sst)
plot(sst_diff1)
acf(sst_diff1) #Calculamos la función de autocorrelación de la primera diferencia
ndiffs(sst_diff1) #Nos dice cuantas diferencias requiere
#Usamos Diferenciación (Segunda Diferencia de sst)
sst_diff2<-diff(sst_diff1)
plot(sst_diff2)
acf(sst_diff2) #Calculamos la función de autocorrelación de la segunda diferencia
ndiffs(sst_diff2) #Nos dice cuantas diferencias requiere

#Fin(almente graficamos todos los resultados
par(mfrow=c(3,2))
plot(ts_sst, ylab="Serie Original")
acf(ts_sst, main="Serie no estacionaria")
plot(sst_diff1, ylab="Primera diferencia")
acf(sst_diff1, main="Serie estacionaria")
plot(sst_diff2, ylab="Segunda diferencia")
acf(sst_diff2, main="Serie estacionaria")


#EJERCICIO PARA LA VARIABLE EKMX

#Graficamos la serie temporal para la segunda variable
ts_ekmx<-ts(datos$ekmx, start = c(1982,1), frequency=365.25)
tsoutliers(ts_ekmx)
plot(ts_ekmx)
#Calculamos la función de autocorrelación de la serie temporal inicial
acf(ts_ekmx, na.action = na.exclude)
#Usamos Diferenciación (Primera Diferencia de sst)
ekmx_diff1<-diff(ts_ekmx)
plot(ekmx_diff1)
acf(ekmx_diff1, na.action = na.exclude) #Calculamos la función de autocorrelación de la primera diferencia
ndiffs(ekmx_diff1) #Nos dice cuantas diferencias requiere
#Usamos Diferenciación (Segunda Diferencia de sst)
ekmx_diff2<-diff(ekmx_diff1)
plot(ekmx_diff2)
acf(ekmx_diff2, na.action = na.exclude) #Calculamos la función de autocorrelación de la segunda diferencia
ndiffs(ekmx_diff2) #Nos dice cuantas diferencias requiere

#Fin(almente graficamos todos los resultados
par(mfrow=c(3,2))
plot(ts_ekmx, ylab="Serie Original")
acf(ts_ekmx, main="Serie no estacionaria", na.action = na.exclude)
plot(ekmx_diff1, ylab="Primera diferencia")
acf(ekmx_diff1, main="Serie estacionaria", na.action = na.exclude)
plot(ekmx_diff2, ylab="Segunda diferencia")
acf(ekmx_diff2, main="Serie estacionaria", na.action = na.exclude)

library(zoo)
#Descomponer la serie en componentes estacionales y tendencia
desc_serie_sst = decompose(ts_sst)
plot(desc_serie_sst)

#Interpolar linealmente el trend
desc_serie_ekmx = decompose(na.approx(ts_ekmx))
plot(desc_serie_ekmx$trend)


#Calcular la correlación cruzada entre las dos series temporales
correlacion_cruzada = ccf(ts_sst, ts_ekmx, na.action = na.exclude)
plot(correlacion_cruzada)


