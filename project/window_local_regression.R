# install.packages("readxl")
library(readxl)
setwd("/Users/shadow/OneDrive - University of Tulsa/classes/Independent Study - Time Series/project")
read_excel("./example_failure_data_sets.xlsx", sheet = "CSR3")



lwr <- function(data, window.size=10, step.size=5, degree=1){
  # print( (length(x) - window.size + 1))
  dat<- rep(0, (length(data$x) - window.size + 1))
  models <- data.frame(matrix(vector(), ncol=length(seq(window.size, length(data$x), step.size)), nrow=degree +1) )
  count = 1
  for(i in seq(window.size, length(data$x), step.size)){
    
    models[,count] <- lm(y~poly(x, degree=degree, raw=TRUE), data[(i-window.size+ 1):i,])$coeff
    count <- count + 1
  }
  models
}

df <- data.frame("x" = seq(1,20), "y" = seq(1,20))
length(df$x)
lwr(df, degree=2)




moving.average <- function(x, window.size=3){
  dat<- rep(0, length(x) - window.size + 1)
  for(i in seq(window.size, length(x))){
    dat[(i-window.size + 1)] <- sum(x[(i-window.size + 1):i]) /window.size
  }
  return(dat)
}

