## X -> (matrix) Examples
## W -> (vector) Weights
## errot -> PerceptronOutput - IdealOutput

########################
##                    ##
##   Weight Funtion   ##
##                    ##
########################
# examples matrix, line int, weight Vector, learningRate Float, error Float
set_weight <- function(x, w, learningRate, error){
  for (i in w){
    for (j in x[,])
    w[c(i)] <- w[c(i)] + learningRate * error * x[c(i,j)]
  }
  return(w)
}

########################
##                    ##
##   Active Funtion   ##
##                    ##
########################

active <- function(x, w){
  active <- 0
  
  for(i in w){
    active <- active + sum(x[c(i)] * w[c(i)])
  }
  
  if (active <= 1)
    return(0)
  
  return(1)
}

########################
##                    ##
##  Training Funtion  ##
##                    ##
########################
training <- function(x, w)

learningRate <- 0.1
error <- 1

x <- matrix(c(1,2,3,4,5,6), nrow = 2, byrow = TRUE)
print(x)
w <- c(1,2,3,4,5,6)
for (i in 1:2){
  print(i)
}
w <- set_weight(x, w, learningRate, error)

y <- active(x, w)
print(y)

y <- active(x, -w)
print(y)

print(x)
print(w)

