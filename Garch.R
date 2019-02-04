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
library(fGarch)

#ANÁLISE DE DADOS
dados<-read.csv("C:/Users/matil/Dropbox/SERIES PROJECTO/Nasdaq.csv",header=TRUE)
summary(dados)
serie=ts(dados[,5],start=c(2015,2,23),frequency=365.25)
plot.ts(serie,plot.type="multiple",main="NASDAQ closing values")

#LOG-RETURNS
logreturn=diff(log(serie))
arch.test(logreturn)
plot.ts(logreturn,plot.type="multiple",main="Log-returns of NASDAQ closing values")
#mean
mean(logreturn) #=0.0004945504
var(logreturn) #8.721256e-05
#Mcleod Li
test=McLeod.Li.test(y=logreturn)
test$p.values

#ACF e PACF
#logreturns
par(mfrow=c(2,3))
acf(logreturn,main="ACF log-returns (NASDAQ)")
acf(logreturn^2,main="ACF squared log-returns (NASDAQ)")
#aboslute values
acf(abs(logreturn),main="ACF absolute log-returns (NASDAQ)")
pacf(logreturn,main="PACF log-returns (NASDAQ)")
#squared values
pacf(logreturn^2,main="PACF squared log-returns (NASDAQ)")
pacf(abs(logreturn),main="PACF absolute log-returns (NASDAQ)")

summary(garchFit(~garch(0,1), logreturn))
summary(garchFit(~garch(1,0), logreturn))
summary(garchFit(~garch(1,1), logreturn))
summary(garchFit(~garch(2,1), logreturn))
summary(garchFit(~garch(1,2), logreturn))
summary(garchFit(~garch(2,2), logreturn))

summary(pacf(logreturn))
#Now test if there is a serial correlation
Box.test(logreturn,lag=7,type='Ljung')
#As we can not reject the null hypothesis (independence) , we assume there is no serial dependence 
#So we can now test if variance is constant or not.

var=(logreturn-mean(logreturn))^2 #standardized squared residuals of logreturns
Box.test(var,lag=7,type='Ljung')
#Box.test shows we can reject the null hypothesis (independence) 
#on variance, so it has significant serial correlation, in other 
#words ARCH effect.

#CENAS A SERIO
adf.test(logreturn) #p-value=0.01 => accept alternative hypothesis(it is stationary)
par(mfrow=c(1,1))
qqPlot(logreturn) #QQPlot
Box.test(logreturn,lag=30,fitdf=10,type="Ljung") #p-value = 0.002327 => logreturns are correlated
t.test(logreturn)
Box.test(var,lag=7,type='Ljung') #p-value < 2.2e-16 => squared log returns are correlated
t.test(logreturn^2)

#análise dos Residuos
modelo=garchFit(~garch(1,1),logreturn)
residuos=residuals(modelo)
summary(residuos)

tsdisplay(residuos)
qqPlot(residuos) #test if residuals are normally distributed
Box.test(residuos,lag=10,fitdf=4,type="Ljung") #p-value = 0.1464
checkresiduals(residuos)
ks.test(residuos,pnorm)
#shapiro.test(residuals(ARMA_fur_2))
par(mfrow=c(1,1))
test=McLeod.Li.test(ARMA_fur_2)
test$p.values
dwtest(residuos)
