getwd()
setwd("/Users/shadow/OneDrive - University of Tulsa/classes/Independent Study - Time Series/time-series-independent-study/Box-book-code")

# install.packages("readxl")
library(readxl)
# install.packages("tseries")
library(tseries)

##################### chptr 2.1 #####################

#use the chickenpox data downloaded from UCI
chicken <- read.csv("./chapter 2 code/hungary_chickenpox/hungary_chickenpox.csv")
chicken$Date <- as.Date(chicken$Date, "%d/%m/%Y")

#lets focus on budapest but lots of cities as well
hist(chicken$BUDAPEST) #for kicks

plot(chicken$Date, chicken$BUDAPEST) #plot with dates
#clearly seasonal data but overal trend seems constant

mean(chicken$BUDAPEST)
var(chicken$BUDAPEST)

#manually calculate the estimated auto correlation
auto.correlation <- function(data, n, k){
  m <- mean(data)
  ck <- 0
  for(t in seq(1, (n-k)) ){
    ck <- ck + (data[t] - m) * (data[t + k] - m)
  }
  c0 <- 0
  for(t in seq(1, (n)) ){
    c0 <- c0 + (data[t] - m)^2
  }
  c0 <- c0/n
  
  ck <- ck/n
  return(ck/c0)
}

sapply( 0:27, auto.correlation, data=chicken$BUDAPEST, n=length(chicken$BUDAPEST) )

#use a library

acf(chicken$BUDAPEST, pl=FALSE)
#same numbers!!

#try failure data too
csr3 <- read_excel("./example_failure_data_sets.xlsx", sheet = "CSR3")
acf(csr3$IF)#, pl=FALSE


##################### chptr 3.1 #####################

