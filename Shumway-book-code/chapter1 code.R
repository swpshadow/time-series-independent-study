#setup

# install.packages("remotes")   # if you don't have the package already
# remotes::install_github("nickpoison/astsa/astsa_build")
data(package = "astsa")
library(astsa)
data(tsa3)
plot(jj, type="o", ylab="Quarterly Earnings per Share")


#1.2
data("speech")
plot(speech)


#1.4
data(nyse)
plot(nyse, ylab="NYSE Returns")


#1.5

data(soi)
data(rec)
par(mfrow = c(2,1)) # set up the graphics
plot(soi, ylab="", xlab="", main="Southern Oscillation Index")
plot(rec, ylab="", xlab="", main="Recruitment")


#1.6
data(fmri1)
par(mfrow=c(2,1), mar=c(3,2,1,0)+.5, mgp=c(1.6,.6,0))
ts.plot(fmri1[,2:5], lty=c(1,2,4,5), ylab="BOLD", xlab="",
          main="Cortex")
ts.plot(fmri1[,6:9], lty=c(1,2,4,5), ylab="BOLD", xlab="",
          main="Thalamus & Cerebellum")
mtext("Time (1 pt = 2 sec)", side=1, line=2)


#1.7
data("EQ5")
data("EXP6")
par(mfrow=c(2,1))
plot(EQ5, main="Earthquake")
plot(EXP6, main="Explosion")


#1.8

w = rnorm(500,0,1) # 500 N(0,1) variates 2 v = filter(w, sides=2, rep(1/3,3)) # moving average
par(mfrow=c(2,1))
plot.ts(w, main="white noise")


#1.9
v = filter(w, sides=2, rep(1/3,3))
plot.ts(v, main="moving average")

#1.10

w = rnorm(550,0,1) # 50 extra to avoid startup problems
x = filter(w, filter=c(1,-.9), method="recursive")[-(1:50)]
plot.ts(x, main="autoregression")


#1.11

set.seed(154) # so you can reproduce the results
w = rnorm(200,0,1); x = cumsum(w) # two commands in one line
wd = w +.2; xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main="random walk")
lines(x); lines(.2*(1:200), lty="dashed")


#1.12
cs = 2*cos(2*pi*1:500/50 + .6*pi)
w = rnorm(500,0,1)
par(mfrow=c(3,1), mar=c(3,2,2,1), cex.main=1.5)
plot.ts(cs, main=expression(2*cos(2*pi*t/50+.6*pi)))
plot.ts(cs+w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,1)))
plot.ts(cs+5*w, main=expression(2*cos(2*pi*t/50+.6*pi) + N(0,25)))



#most of the examples are just stuff like "moving average is stationary" or definitions

#1.13
moving.average <- function(x, window.size=3){
  dat<- rep(0, length(x) - window.size + 1)
  for(i in seq(window.size, length(x))){
    dat[(i-window.size + 1)] <- sum(x[(i-window.size + 1):i]) /window.size
  }
  return(dat)
}

moving.average(x)




#1.18
autoco.rand.walk <- function(s, t, var){
  return(min(s, t)* var)
}


#1.26
data(soiltemp)
persp(1:64, 1:36, soiltemp, phi=30, theta=30, scale=FALSE, expand=4,
      ticktype="detailed", xlab="rows", ylab="cols",
      zlab="temperature")
plot.ts(rowMeans(soiltemp), xlab="row", ylab="Average Temperature")


#1.27
fs = abs(fft(soiltemp-mean(soiltemp)))^2/(64*36)
cs = Re(fft(fs, inverse=TRUE)/sqrt(64*36)) # ACovF
rs = cs/cs[1,1] # ACF
rs2 = cbind(rs[1:41,21:2], rs[1:41,1:21])
rs3 = rbind(rs2[41:2,], rs2)
par(mar = c(1,2.5,0,0)+.1)
persp(-40:40, -20:20, rs3, phi=30, theta=30, expand=30,
        scale="FALSE", ticktype="detailed", xlab="row lags",
        ylab="column lags", zlab="ACF")
