# data(package = "astsa")
library(astsa)


#2.1
#lm is "linear model"
#fit is the result
#summary outputs the information from the linear fit
data("gtemp")
summary(mod <- lm(gtemp~time(gtemp))) # regress gtemp on time 
plot(gtemp, type="o", ylab="Global Temperature Deviation")
abline(mod) # add regression line to the plot

# mse of fitted model
sum((fitted(mod) - gtemp)^2 )/length(gtemp)

#AIC of model
AIC(mod)

#BIC
BIC(mod)

#r2 value
summary(mod)$r.squared




#2.2
par(mfrow=c(3,1))
plot(cmort, main="Cardiovascular Mortality", xlab="", ylab="")
plot(tempr, main="Temperature", xlab="", ylab="")
plot(part, main="Particulates", xlab="", ylab="")
dev.new() # open a new graphic device for the scatterplot matrix
pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part)) 
temp = tempr-mean(tempr) # center temperature
temp2 = temp^2
trend = time(cmort) # time
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit) # regression results
summary(aov(fit)) # ANOVA table (compare to next line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1
num = length(cmort) # sample size
AIC(fit)/num - log(2*pi) # AIC
AIC(fit, k=log(num))/num - log(2*pi) # BIC
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc


#2.3
fish = ts.intersect(rec, soiL6=lag(soi,-6), dframe=TRUE)
summary(lm(rec~soiL6, data=fish, na.action=NULL))


#2.4

#detrended is minus the estimate of the model (so taking away the trend)
#differencing is x_t - x_{t-1} for first difference.
#first dif is delta x_t
fit = lm(gtemp~time(gtemp), na.action=NULL) # regress gtemp on time
par(mfrow=c(2,1))
plot(resid(fit), type="o", main="detrended")
plot(diff(gtemp), type="o", main="first difference")
par(mfrow=c(3,1)) # plot ACFs
acf(gtemp, 48, main="gtemp")
acf(resid(fit), 48, main="detrended")
acf(diff(gtemp), 48, main="first difference")

#?acf

#2.5
mean(diff(gtemp, lag=1)) # = 0.00659 (drift)
sd(diff(gtemp))/sqrt(length(diff(gtemp))) # = 0.00966 (SE)
# ?diff



#2.6

#makes the time series WAY more stationary in appearance by plotting the log of the series. 
par(mfrow=c(2,1))
plot(varve, main="varve", ylab="")
plot(log(varve), main="log(varve)", ylab="" )

#2.7
library(astsa)
#they renamed the methods lag1.plot from lag.plot1 in the latest version of the library... why...
#really nice methods though
lag1.plot(soi, 12) # Fig 2.7
lag2.plot(soi, rec, 8) # Fig 2.8


#2.8
set.seed(1000) # so you can reproduce these results
x = 2*cos(2*pi*1:500/50 + .6*pi) + rnorm(500,0,5)
z1 = cos(2*pi*1:500/50); z2 = sin(2*pi*1:500/50)
summary(fit <- lm(x~0+z1+z2)) # zero to exclude the intercept
plot.ts(x, lty="dashed")
lines(fitted(fit), lwd=2)

#2.9
I = abs(fft(x))^2/500 # the periodogram
P = (4/500)*I[1:250] # the scaled periodogram
f = 0:249/500 # frequencies
plot(f, P, type="l", xlab="Frequency", ylab="Scaled Periodogram")

#2.10
#very naive appraoch to use MA. Like what I wrote for the other week. 
ma5 = filter(cmort, sides=2, rep(1,5)/5)
ma53 = filter(cmort, sides=2, rep(1,53)/53)
plot(cmort, type="p", ylab="mortality")
lines(ma5); lines(ma53)
#filter


#2.11
wk = time(cmort) - mean(time(cmort))
wk2 = wk^2; wk3 = wk^3
cs = cos(2*pi*wk); sn = sin(2*pi*wk)
reg1 = lm(cmort~wk + wk2 + wk3, na.action=NULL)
reg2 = lm(cmort~wk + wk2 + wk3 + cs + sn, na.action=NULL)
plot(cmort, type="p", ylab="mortality")
lines(fitted(reg1)); lines(fitted(reg2))


#2.12
#kernel smoothing. Same idea as from stoch modeling. Lot better than MA imo but more work to implement from scratch.
plot(cmort, type="p", ylab="mortality")
lines(ksmooth(time(cmort), cmort, "normal", bandwidth=5/52))
lines(ksmooth(time(cmort), cmort, "normal", bandwidth=2))
# length(cmort)
# ?ksmooth


#2.13
par(mfrow=c(2,1))
plot(cmort, type="p", ylab="mortality", main="nearest neighbor")
lines(supsmu(time(cmort), cmort, span=.5))
lines(supsmu(time(cmort), cmort, span=.01))
plot(cmort, type="p", ylab="mortality", main="lowess")
lines(lowess(cmort, f=.02)); lines(lowess(cmort, f=2/3))


#2.14 splines baby
plot(cmort, type="p", ylab="mortality")
lines(smooth.spline(time(cmort), cmort, spar=0))
lines(smooth.spline(time(cmort), cmort, spar=0.5))
# ?smooth.spline


# 2.15 basically plotting two series against each other to "smooth" them. Shows non linear relationsship in this case
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
plot(tempr, cmort, main="lowess", xlab="Temperature",
       ylab="Mortality")
lines(lowess(tempr,cmort))
plot(tempr, cmort, main="smoothing splines", xlab="Temperature",
       ylab="Mortality")
lines(smooth.spline(tempr, cmort))

