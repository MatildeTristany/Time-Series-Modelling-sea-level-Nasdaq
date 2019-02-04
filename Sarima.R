#PACOTES NECESSÁRIOS
library(readxl)
library(forecast)
library(urca)
library(tseries)
library(car)
library(lmtest)
library(olsrr)
library(aTSA)
library(vars)
library(het.test)
library(TSA)

#ANÁLISE DE DADOS
dados<-read_excel("C:/Users/matil/Dropbox/SERIES PROJECTO/1979-2005 DataSet.xlsx", col_names=TRUE)
summary(dados)
serie=ts(dados[,2:8],start=c(1979,1,1),frequency=365.25)
par(mfrow=c(1,1))
plot.ts(serie,plot.type="multiple",main="Séries Temporais")
summary(serie)

#DECOMPOSIÇÃO EM TENDÊNCIA E SAZONALIDAE
par(mfrow=c(1,1))
y1=stl(serie[,1],s.window="period")
plot(y1,main="Fur")
y2=stl(serie[,2],s.window="period")
plot(y2,main="Hor")
y3=stl(serie[,3],s.window="period")
plot(y3,main="Ola")
y4=stl(serie[,4],s.window="period")
plot(y4,main="Sto")
y5=stl(serie[,5],s.window="period")
plot(y5,main="Ged")
y6=stl(serie[,6],s.window="period")
plot(y6,main="Kun")
y7=stl(serie[,7],s.window="period")
plot(y7,main="Rat")

#AUGMENTED DICKEY FULLER TEST (test for stationarity)
#fur
adf.test(y1$time.series[,3])
#hor
adf.test(y2$time.series[,3])
#ola
adf.test(y3$time.series[,3])
#sto
adf.test(y4$time.series[,3])
#ged
adf.test(y5$time.series[,3])
#kun
adf.test(y6$time.series[,3])
#rat
adf.test(y7$time.series[,3])

#ACF E PACF DA SERIE ORIGINAL E DOS RESÍDUOS APÓS STL
#fur
par(mfrow=c(2,2))
acf(serie[,1],main="Fur Série Original",lag.max=180)
pacf(serie[,1],main="Fur Série Original",lag.max=180)
acf(y1$time.series[,3],main="Fur Resíduos após STL",lag.max=180)
pacf(y1$time.series[,3],main="Fur Resíduos após STL",lag.max=180)
#hor
par(mfrow=c(2,2))
acf(serie[,2],main="Hor Série Original",lag.max=180)
pacf(serie[,2],main="Hor Série Original",lag.max=180)
acf(y2$time.series[,3],main="Hor Resíduos após STL",lag.max=180)
pacf(y2$time.series[,3],main="Hor Resíduos após STL",lag.max=180)
#ola
par(mfrow=c(2,2))
acf(serie[,3],main="Ola Série Original",lag.max=180)
pacf(serie[,3],main="Ola Série Original",lag.max=180)
acf(y3$time.series[,3],main="Ola Resíduos após STL",lag.max=180)
pacf(y3$time.series[,3],main="Ola Resíduos após STL",lag.max=180)
#sto
par(mfrow=c(2,2))
acf(serie[,4],main="Sto Série Original",lag.max=180)
pacf(serie[,4],main="Sto Série Original",lag.max=180)
acf(y4$time.series[,3],main="Sto Resíduos após STL",lag.max=180)
pacf(y4$time.series[,3],main="Sto Resíduos após STL",lag.max=180)
#ged
par(mfrow=c(2,2))
acf(serie[,5],main="Ged Série Original",lag.max=180)
pacf(serie[,5],main="Ged Série Original",lag.max=180)
acf(y5$time.series[,3],main="Ged Resíduos após STL",lag.max=180)
pacf(y5$time.series[,3],main="Ged Resíduos após STL",lag.max=180)
#kun
par(mfrow=c(2,2))
acf(serie[,6],main="Kun Série Original",lag.max=180)
pacf(serie[,6],main="Kun Série Original",lag.max=180)
acf(y6$time.series[,3],main="Kun Resíduos após STL",lag.max=180)
pacf(y6$time.series[,3],main="Kun Resíduos após STL",lag.max=180)
#rat
par(mfrow=c(2,2))
acf(serie[,7],main="Rat Série Original",lag.max=180)
pacf(serie[,7],main="RatSérie Original",lag.max=180)
acf(y7$time.series[,3],main="Rat Resíduos após STL",lag.max=180)
pacf(y7$time.series[,3],main="Ged Resíduos após STL",lag.max=180)

#FIT DE MODELOS ARIMA COM AUTOARIMA
#fur
ARMA_fur_1=auto.arima(serie[,1]) #série original
arimaorder(ARMA_fur_1)
summary(ARMA_fur_1)
ARMA_fur_2=auto.arima(y1$time.series[,3]) #resíduos da série
arimaorder(ARMA_fur_2)
summary(ARMA_fur_2)
ARMA_fur_3=auto.arima(y1$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_fur_3)
summary(ARMA_fur_3) #-,-,- (mas apenas 1 unidade em valores de ordem 10^5)
#hor
ARMA_hor_1=auto.arima(serie[,2]) #série original
arimaorder(ARMA_hor_1)
summary(ARMA_hor_1)
ARMA_hor_2=auto.arima(y2$time.series[,3]) #resíduos da série
arimaorder(ARMA_hor_2)
summary(ARMA_hor_2)
ARMA_hor_3=auto.arima(y2$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_hor_3)
summary(ARMA_hor_3) #mesmos valores de qualidade do modelo
#ola
ARMA_ola_1=auto.arima(serie[,3]) #série original
arimaorder(ARMA_ola_1)
summary(ARMA_ola_1)
ARMA_ola_2=auto.arima(y3$time.series[,3]) #resíduos da série
arimaorder(ARMA_ola_2)
summary(ARMA_ola_2)
ARMA_ola_3=auto.arima(y3$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_ola_3)
summary(ARMA_ola_3) #mesmos valores de qualidade do modelo
#sto
ARMA_sto_1=auto.arima(serie[,4]) #série original
arimaorder(ARMA_sto_1)
summary(ARMA_sto_1)
ARMA_sto_2=auto.arima(y4$time.series[,3]) #resíduos da série
arimaorder(ARMA_sto_2)
summary(ARMA_sto_2)
ARMA_sto_3=auto.arima(y4$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_sto_3)
summary(ARMA_sto_3) #+,+ (ordem de 4 unidades em 10^4),- (ordem de 10 unidades em 10^4)
#ged
ARMA_ged_1=auto.arima(serie[,5]) #série original
arimaorder(ARMA_ged_1)
summary(ARMA_ged_1)
ARMA_ged_2=auto.arima(y5$time.series[,3]) #resíduos da série
arimaorder(ARMA_ged_2)
summary(ARMA_ged_2)
ARMA_ged_3=auto.arima(y5$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_ged_3)
summary(ARMA_ged_3) #mesmos valores de qualidade do modelo
#kun
ARMA_kun_1=auto.arima(serie[,6]) #série original
arimaorder(ARMA_kun_1)
summary(ARMA_kun_1)
ARMA_kun_2=auto.arima(y6$time.series[,3]) #resíduos da série
arimaorder(ARMA_kun_2)
summary(ARMA_kun_2)
ARMA_kun_3=auto.arima(y6$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_kun_3)
summary(ARMA_kun_3) #-,-,- (ordem de menos de 1 unidade em 10^5)
#rat
ARMA_rat_1=auto.arima(serie[,7]) #série original
arimaorder(ARMA_rat_1)
summary(ARMA_rat_1)
ARMA_rat_2=auto.arima(y7$time.series[,3]) #resíduos da série
arimaorder(ARMA_rat_2)
summary(ARMA_rat_2)
ARMA_rat_3=auto.arima(y7$time.series[,3],stepwise=FALSE) #stepwise false da série
arimaorder(ARMA_rat_3)
summary(ARMA_rat_3) #+,+ (ordem de 1 unidade em 10^5),- (ordem de 8 unidades em 10^5)

#ANÁLISE PELAS DIFERENÇAS (AUTOARIMA A FORÇAR D=1)
#fur
diffs_1=auto.arima(serie[,1],D=1,approximation=TRUE) #série original
arimaorder(diffs_1)
summary(diffs_1)
#hor
diffs_2=auto.arima(serie[,2],D=1,approximation=TRUE) #série original
arimaorder(diffs_2)
summary(diffs_2)
#ola
diffs_3=auto.arima(serie[,3],D=1,approximation=TRUE) #série original
arimaorder(diffs_3)
summary(diffs_3)
#sto
diffs_4=auto.arima(serie[,4],D=1,approximation=TRUE) #série original
arimaorder(diffs_4)
summary(diffs_4)
#ged
diffs_5=auto.arima(serie[,5],D=1,approximation=TRUE) #série original
arimaorder(diffs_5)
summary(diffs_5)
#kun
diffs_6=auto.arima(serie[,6],D=1,approximation=TRUE) #série original
arimaorder(diffs_6)
summary(diffs_6)
#rat
diffs_7=auto.arima(serie[,7],D=1,approximation=TRUE) #série original
arimaorder(diffs_7)
summary(diffs_7)

#DIAGNÓSTICO (lag=ln(n) is recomended and ln(9862)=9.2 
#so we chose lag=10)
#all values should be contained in +-(2/sqrt(9862))
#fur
tsdisplay(residuals(ARMA_fur_2))
qqPlot(residuals(ARMA_fur_2)) #test if residuals are normally distributed
Box.test(residuals(ARMA_fur_2),lag=10,fitdf=4,type="Ljung")
checkresiduals(ARMA_fur_2)
ks.test(residuals(ARMA_fur_2),pnorm)
#shapiro.test(residuals(ARMA_fur_2))
par(mfrow=c(1,1))
test=McLeod.Li.test(ARMA_fur_2)
test$p.values
arch.test(ARMA_fur_2)
#hor
tsdisplay(residuals(ARMA_hor_2))
qqPlot(residuals(ARMA_hor_2))
Box.test(residuals(ARMA_hor_2),lag=10,fitdf=5,type="Ljung")
checkresiduals(ARMA_hor_2)
ks.test(residuals(ARMA_hor_2),pnorm)
arch.test(ARMA_hor_2)
par(mfrow=c(1,1))
test=McLeod.Li.test(ARMA_fur_2)
test$p.values
#ola
tsdisplay(residuals(ARMA_ola_2))
qqPlot(residuals(ARMA_ola_2))
Box.test(residuals(ARMA_ola_2),lag=10,fitdf=3,type="Ljung")
checkresiduals(ARMA_ola_2)
ks.test(residuals(ARMA_ola_2),pnorm)
par(mfrow=c(1,1))
test=McLeod.Li.test(ARMA_fur_2)
test$p.values
#sto
tsdisplay(residuals(ARMA_sto_2))
qqPlot(residuals(ARMA_sto_2))
Box.test(residuals(ARMA_sto_2),lag=10,fitdf=7,type="Ljung")
checkresiduals(ARMA_sto_2)
ks.test(residuals(ARMA_sto_2),pnorm)
#ged
tsdisplay(residuals(ARMA_ged_2))
qqPlot(residuals(ARMA_ged_2))
Box.test(residuals(ARMA_ged_2),lag=10,fitdf=5,type="Ljung")
checkresiduals(ARMA_ged_2)
ks.test(residuals(ARMA_ged_2),pnorm)
#kun
tsdisplay(residuals(ARMA_kun_2))
qqPlot(residuals(ARMA_kun_2))
Box.test(residuals(ARMA_kun_2),lag=10,fitdf=4,type="Ljung")
checkresiduals(ARMA_kun_2)
ks.test(residuals(ARMA_kun_2),pnorm)
#rat
tsdisplay(residuals(ARMA_rat_2))
qqPlot(residuals(ARMA_rat_2))
Box.test(residuals(ARMA_rat_2),lag=10,fitdf=5,type="Ljung")
checkresiduals(ARMA_rat_2)
ks.test(residuals(ARMA_rat_2),pnorm)

#FORECAST
forecast(y1,method="arima",h=4,level=95)
forecast(y2,method="arima",h=4,level=95)
forecast(y3,method="arima",h=4,level=95)
forecast(y4,method="arima",h=4,level=95)
forecast(y5,method="arima",h=4,level=95)
forecast(y6,method="arima",h=4,level=95)
forecast(y7,method="arima",h=4,level=95)
