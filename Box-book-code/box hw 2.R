#same code to load and process data
getwd()
setwd("/Users/shadow/OneDrive - University of Tulsa/classes/Independent Study - Time Series/time-series-independent-study/Box-book-code")

# install.packages("readxl")
library(readxl)
# install.packages("tseries")
library(tseries)

##################### chptr 3.2 #####################

#use the chickenpox data downloaded from UCI
chicken <- read.csv("./chapter 2 code/hungary_chickenpox/hungary_chickenpox.csv")
chicken$Date <- as.Date(chicken$Date, "%d/%m/%Y")

ar(chicken$BUDAPEST )#uses AIC to select order

ar(chicken$BUDAPEST, order.max = 12 )

arima(chicken$BUDAPEST)


#much much better results when order is specified on ARIMA model
arima(chicken$BUDAPEST, order = c(12, 1, 2))


#figure 3.1
# install.packages("TSA")
library(TSA)
set.seed(12345)
par(mfrow=c(3,2))
plot(arima.sim(list(order=c(1,0,0),ar = 0.8), n=100),ylab=
        expression(z[t]),main=expression("AR(1) process with "*phi*"=0.8")) 
plot(arima.sim(list(order=c(1,0,0),ar = -0.8), n=100), ylab= expression(z[t]),main=expression("AR(1) process with "*phi*"=-0.8")) 
plot(ARMAacf(ar=0.8,ma=0,15)[-1],type="h",ylab="ACF",xlab="lag") 
abline(h=0) 
plot(ARMAacf(ar=-0.8,ma=0,15)[-1],type="h",ylab="ACF",xlab="lag") 
abline(h=0)
ARMAspec(model=list(ar=0.8),freq=seq(0,0.5,0.001),plot=TRUE) 
ARMAspec(model=list(ar=-0.8),freq=seq(0,0.5,0.001),plot=TRUE)


#3.2.4

library(TSA)
ar.acf=ARMAacf(ar=c(0.75,-0.5))
ar.spec=ARMAspec(model=list(ar=c(0.75,-0.5),freq=seq(0,0.5,0.0005))) 
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
plot(arima.sim(list(order=c(2,0,0),ar=c(0.75,-0.5)), n=70), ylab=
         expression(z[t]),xlab="Time",main=("Simulated AR(2) process")) 
plot(ar.acf,type="h",ylab="ACF",xlab="lag")
abline(h=0)
plot(ar.spec$spec, main="c")



#estimated autocorrolation
chick=ts(chicken$BUDAPEST)
acf2(chick,15)


#3.3
ACF=ARMAacf(ar=0,ma=c(-0.8,+0.5),lag.max=15,pacf=FALSE)[-1]
PACF=ARMAacf(ar=0,ma=c(-0.8,+0.5),lag.max=15,pacf=TRUE)
par(mfrow=c(2,1))
plot(ACF,type='h',ylim=c(-0.8,0.6),xlab='lag',main='(a): ACF')
abline(h=0)
plot(PACF,type='h',ylim=c(-0.8,0.6),xlab='lag',main='(b):PACF')
abline(h=0)
ACF # Retrieves the autocorrelation coefficients
PACF # Retrieves the partial autocorrelation coefficients


#3.4
ACF=ARMAacf(ar=0.8,ma=0.6,20)[-1]
PACF=ARMAacf(ar=0.8,ma=0.6,20,pacf=TRUE)
par(mfrow=c(1,2))
plot(ACF,type="h",xlab="lag");abline(h=0) 
plot(PACF,type="h",xlab="lag");abline(h=0)

