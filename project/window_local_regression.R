#install.packages("readxl")
library(readxl)
setwd("C:/Users/swp7196/Documents/time_series/project")
csr3 <- read_excel("./example_failure_data_sets.xlsx", sheet = "S2")



lwr <- function(data, window.size=10, step.size=5, degree=1, predicting.degree=NA, ma.size = 3){

  # degree=1
  # predicting.degree=NA
  # data =  traindf
  # window.size=as.integer(0.1* length(data$x))
  # step.size = window.size
  # ma.size = 4
  if(is.na(predicting.degree)){
    predicting.degree=degree
  }
  #fit local regression models in a sliding window fashion
  #initial fit section of paper
  models <- data.frame(matrix(vector(), nrow=length(seq(window.size, length(data$x), step.size)), ncol=degree +1) )
  count = 0
  
  for(i in seq(window.size, length(data$x), step.size)){
    count <- count + 1
    models[count,] <- lm(y~poly(x, degree=degree, raw=TRUE), data[(i-window.size+ 1):i,])$coeff
    lines(x=data[(i-window.size + 1):i,1], y = predict.lwr(data[(i-window.size+ 1):i,1], models[count,]), col="blue")
  }
  if(i-step.size < length(data$x))
  {
    count <- count + 1
    models[count,] <- lm(y~poly(x, degree=degree, raw=TRUE), data[(length(data$x)-window.size + 1):length(data$x),])$coeff
    lines(x=data[(length(data$x)-window.size + 1):length(data$x),1], 
          y = predict.lwr(data[(length(data$x)-window.size+ 1):length(data$x),1], models[count,]), col="blue")
  }
  
  #fit a model to the params of the sliding window models
  #prediction section of the paper
  params = rep(0, degree+1)
  start.idx = 1
  for(d in seq(degree + 1) ){
    #make data to train
    data.train <- data.frame("x" = seq(start.idx,count), "y" = models[start.idx:count,d])
    
    mod <- lm(y~poly(x, degree=predicting.degree, raw=TRUE), data.train)
    #make data to predict
    data.predict <- data.frame("x" = c(count+1))
    params[d] <- tail(predict.lm(mod,  data.predict), n=1)
    
    data.train[count+1,] = c(count+1, tail(predict.lwr(data.predict$x, mod$coefficients), n=1)[[1]] )
    plot(data.train)
    lines(x=data.train$x, y = predict.lwr(data.train$x,  mod$coefficients))
  }
  
  #then we can use params to make the model after the data. 
  
  #but before we use the params lets first do error correction
  #error correction section fo the paper

  error_params = rep(0, degree + 1)
  ma = rep(0, degree + 1)
  epsilon = rep(0, degree + 1)
  for(d in seq(degree + 1) ){
    
    # make data to train
    data.train <- data.frame("x" = seq(1,count-1), "y" = models[1:count-1,d])
    mod <- lm(y~poly(x, degree=predicting.degree, raw=TRUE), data.train)
  
    # make data to test
    data.test <- data.frame("x" = c(count) )

    error_params[d] <- tail(predict.lm(mod,  data.test), n=1)
  
    # prediction error correction
    epsilon[d] = models[count,d] - error_params[d]
    params[d] <- params[d] + epsilon[d]
    
    # moving average error correction
    ma[d] = mean(tail(moving.average(models[,d]), n = ma.size))
    params[d] = 0.5 * (params[d] + ma[d])  
  }
  return(params)
}


#gives us the predictions for y for the data given the data and coefs of the model
predict.lwr <- function(dat, coef){
  deg <- length(coef) - 1
  res <- rep(0, length(dat))
  for(i in seq(0, deg)){
    for(x in seq(length(dat))){
      res[x] <- res[x] + coef[i+1] * dat[x]^i
    }
    
  }
  return(res)
}


#MA code
moving.average <- function(x, window.size=3){
  dat<- rep(0, length(x) - window.size + 1)
  if (window.size > 0)
  {
    for(i in seq(window.size, length(x))){
      dat[(i-window.size + 1)] <- sum(x[(i-window.size + 1):i]) /window.size
    }
  }
  return(dat)
}



traindf <-data.frame("x"=csr3$FT[1:as.integer(length(csr3$FT)* 0.9)], "y"=csr3$FN[1:as.integer(length(csr3$FT)* 0.9)])
testdf <- data.frame("x"=csr3$FT[as.integer(length(csr3$FT)* 0.9):length(csr3$FT)], "y"=csr3$FN[as.integer(length(csr3$FT)* 0.9):length(csr3$FT)])


plot(csr3$FT, csr3$FN)

window_size = as.integer(length(traindf$x)* 0.1)
result <- lwr(traindf, degree=1, window.size = window_size, step.size = window_size, ma.size = 1)

predict.lwr(testdf$x, result)

plot(csr3$FT, csr3$FN)
lines(x=testdf$x, y=predict.lwr(testdf$x, result), col="red")
mean((testdf$y - predict.lwr(testdf$x, result))^2)
