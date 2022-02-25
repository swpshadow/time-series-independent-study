# install.packages("readxl")
library(readxl)
setwd("/Users/shadow/OneDrive - University of Tulsa/classes/Independent Study - Time Series/time-series-independent-study/project")
csr3 <- read_excel("./example_failure_data_sets.xlsx", sheet = "CSR3")



lwr <- function(data, window.size=10, step.size=5, degree=1, predicting.degree=NA){
  if(is.na(predicting.degree)){
    predicting.degree=degree
  }
  #fit local regression models in a sliding window fashion
  dat<- rep(0, (length(data$x) - window.size + 1))
  models <- data.frame(matrix(vector(), nrow=length(seq(window.size, length(data$x), step.size)), ncol=degree +1) )
  count = 1
  for(i in seq(window.size, length(data$x), step.size)){
    
    models[count,] <- lm(y~poly(x, degree=degree, raw=TRUE), data[(i-window.size+ 1):i,])$coeff
    count <- count + 1
  }
  
  #fit a model to the params of the sliding window models
  params = rep(0, degree+1)
  for(d in seq(degree + 1) ){
    #make data to train
    dat <- data.frame("x" = seq(1,length(models[,1])), "y" = models[,d])
    mod <- lm(y~poly(x, degree=predicting.degree, raw=TRUE), dat)
    #make data to test
    dat <- data.frame("x" = seq(length(models[,1])+1, length(models[,1])+2))
    params[d] <- tail(predict.lm(mod,  dat), n=1)
  }
  return(params)
}



predict.lwr <- function(dat, coef){
  deg <- length(coef) - 1
  res <- rep(0, length(dat))
  for(i in seq(0, deg)){
    for(x in seq(length(dat))){
      res[x] <- res[x] + coef[i+1] * dat[x]^i
      # print(res[x])
    }
    
  }
  return(res)
}

df<-data.frame("x"=csr3$FT[1:as.integer(length(csr3$FT)* 0.9)], "y"=csr3$FN[1:as.integer(length(csr3$FT)* 0.9)])

result <- lwr(df, degree=1, predicting.degree = 3)



df<-data.frame("x"=csr3$FT[as.integer(length(csr3$FT)* 0.9):length(csr3$FT)], "y"=csr3$FN[as.integer(length(csr3$FT)* 0.9):length(csr3$FT)])
predict.lwr(df$x, result)




#MA code
moving.average <- function(x, window.size=3){
  dat<- rep(0, length(x) - window.size + 1)
  for(i in seq(window.size, length(x))){
    dat[(i-window.size + 1)] <- sum(x[(i-window.size + 1):i]) /window.size
  }
  return(dat)
}

