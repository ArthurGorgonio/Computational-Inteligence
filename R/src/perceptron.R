
## X -> (vector) Examples
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
set_weight <- function(x, current_row, w, learningRate, error){
  for (i in 1:col) {
    w[c(i)] <- w[c(i)] + (learningRate * error * x[c((current_row - 1) * col + i)])
  }
  return(w)
}

########################
##                    ##
##   Active Funtion   ##
##                    ##
########################

activation <- function(x, current_row, w){
  active <- 0

  for(i in 1:col){
    active <- active + sum(x[c((current_row - 1) * col + i)] * w[c(i)])
  }

  if (active > 0){
    return (1)
  }else{
    return (0)
  }
}

########################
##                    ##
##  Training Funtion  ##
##                    ##
########################
training <- function(data, x, w, error){
  perceptron_output <- c()
  learning <- TRUE
  count <- 0
  while(count <= 10000 && learning){
    print(count)
    count <- count + 1
    
    for(i in 1:row){
      perceptron_output[c(i)] <- activation(x, i, w)
    }

    k <- which(data$class != perceptron_output)
    
    if(length(k) != 0){
      error <- data$class[c(k[c(1)])] - perceptron_output[c(k[c(1)])]
      w <- set_weight(x, k[c(1)], w, learningRate, error)
    }else{
      learning = FALSE
    }
  }
  return(w)
}

########################
##                    ##
##    Read Funtion    ##
##                    ##
########################
archive_read <- function(file){
  setwd("csv/")
  data <- read.csv(file)
  setwd("../")
  return(data)
}

set_col <- function(data){
  return(ncol(data) - 1)
}

set_row <- function(data){
  return(nrow(data))
}
#x1 x2 bias
convert_to_vector <- function(data, row, col){
  for(i in 1:row) {
    for(j in 1:col) {
      x[c(((i - 1) * col) + j)] <- data[i, j]
    }
  }
  return(x)
}

archive_name <- function(file){
  
}

main <- function(data, row, col, x, w, error){
  data <<- archive_read("implies.csv")
  
  row <<- set_row(data)
  
  col <<- set_col(data)
  
  x <<- convert_to_vector(data, row, col)
  
  w <<- random_weights()
  
  w <<- training(data, x, w, error)
}

main(data, row, col, x, w, error)
