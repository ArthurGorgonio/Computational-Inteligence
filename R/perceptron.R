
## X -> (matrix) Examples
## W -> (vector) Weights
## errot -> PerceptronOutput - IdealOutput

# Seting to project directory 
# setwd("~/Projects/Computational-Inteligence/R/")

########################
##                    ##
##   Global Settings  ##
##                    ##
########################
# archive to learning
data <- c()

# not mutable variables
col <- c()
row <- c()

#vectors for weight and the examples
w <- c()
x <- c()


error <- 0
learningRate <- 0.1
########################
##                    ##
##   Weight Funtion   ##
##                    ##
########################
random_weights <- function(){
  return(runif(col,-1,1))
}


# examples Vector, weight Vector, learningRate Float, error Float
set_weight <- function(x, row, w, learningRate, error){
  for (i in 1:col) {
    w[c(i)] <- w[c(i)] + learningRate * error * x[c((row - 1) * col + i)]
  }
  return(w)
}

########################
##                    ##
##   Active Funtion   ##
##                    ##
########################

active <- function(x, row, w){
  active <- 0

  for(i in 1:col){
    active <- active + sum(x[c((1 - 1) * col + i)] * w[c(i)])
  }

  if (active > 0)
    return(1)

  return(0)
}

########################
##                    ##
##  Training Funtion  ##
##                    ##
########################
training <- function(data, x, w, error){
  perceptron_output <- c()
  learning <- TRUE
  while(learning){
    error <- 0
    
    w <- set_weight(x, row, w, learningRate, error)
    
    for(i in 1:row){
      perceptron_output[c(i)] <- active(x, row, w)
    }
    k <- which(data$class != perceptron_output)
    
    for(i in k){
      error <- error
    }
    
    if(error == 0){
      learning = FALSE
    }
  }
}

########################
##                    ##
##    Read Funtion    ##
##                    ##
########################
archive_read <- function(file){
  data <- read.csv(file)
  return(data)
}

set_col <- function(data){
  return(ncol(data) - 1)
}

set_row <- function(data){
  return(nrow(data))
}

convert_to_vector <- function(data, row, col){
  for(i in 1:row) {
    for(j in 1:col) {
      x[c(((i - 1) * col) + j)] <- data[i, j]
    }
  }
  return(x)
}

data <- archive_read("or.csv")
row <- set_row(data)
col <- set_col(data)
x <- convert_to_vector(data, row, col)
w <- random_weights()


training(data,x, w, error)