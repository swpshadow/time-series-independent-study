# install.packages("readxl")
library(readxl)
setwd("/Users/shadow/OneDrive - University of Tulsa/classes/Independent Study - Time Series/time-series-independent-study/project")
read_excel("./example_failure_data_sets.xlsx", sheet = "CSR3")



lwr <- function(data, window.size=10, step.size=5, degree=1, predicting.degree=NA){
  if(is.na(predicting.degree)){
    predicting.degree=degree
  }
  #fit local regression models in a sliding window fasion
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
    dat <- data.frame("x" = seq(1,length(models[,1])), "y" = models[,d])
    mod <- lm(y~poly(x, degree=predicting.degree, raw=TRUE), dat)
    dat <- data.frame("x" = seq(length(models[,1])+1, length(models[,1])+2))
    params[d] <- tail(predict.lm(mod,  dat), n=1)
  }
  return(params)
}

df <- data.frame("x" = seq(1,40), "y" = seq(2,80, 2))
# length(df$x)

result <- lwr(df, degree=2)


moving.average <- function(x, window.size=3){
  dat<- rep(0, length(x) - window.size + 1)
  for(i in seq(window.size, length(x))){
    dat[(i-window.size + 1)] <- sum(x[(i-window.size + 1):i]) /window.size
  }
  return(dat)
}

