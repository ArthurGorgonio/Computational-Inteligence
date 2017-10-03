
## X -> (vector) Examperceptronles
## W -> (vector) Weights
## errot -> perceptronerceperceptrontronOutperceptronut - IdealOutperceptronut

# Seting to perceptronroject directory
setwd("~/Projects/Computational-Inteligence/R/src")

########################
##                    ##
##   Class Settings   ##
##                    ##
########################
#class name = perceptron, attributes X,W
perceptron <- c()
setClass("perceptron", slots = list(x="vector", w="vector"))
perceptron <- new("perceptron", x=c(1,2), w=c(1,2))

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

# #vectors for weight and the examples
# w <- c()
# x <- c()

file <- c()
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
set_weight <- function(perceptron, current_row, learningRate, error){
  for (i in 1:col) {
    perceptron@w[c(i)] <- perceptron@w[c(i)] + (learningRate * error * perceptron@x[c((current_row - 1) * col + i)])
  }
  return(perceptron@w)
}

########################
##                    ##
##   Active Funtion   ##
##                    ##
########################

activation <- function(perceptron, current_row){
  active <- 0

  for(i in 1:col){
    active <- active + sum(perceptron@x[c((current_row - 1) * col + i)] * perceptron@w[c(i)])
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
training <- function(data, p, error){
  perceptron_output <- c()
  learning <- TRUE
  count <- 0
  while(count < 10000 && learning){
    count <- count + 1
    print(count)

    for(i in 1:row){
      perceptron_output[c(i)] <- activation(perceptron, i)
    }

    k <- which(data$class != perceptron_output)

    if(length(k) != 0){
      error <- data$class[c(k[c(1)])] - perceptron_output[c(k[c(1)])]
      perceptron@w <- set_weight(perceptron, k[c(1)], learningRate, error)
    }else{
      learning = FALSE
    }
  }
  return(perceptron@w)
}

########################
##                    ##
##    Read Funtion    ##
##                    ##
########################
archive_read <- function(file){
  setwd("../csv/")
  data <- read.csv(file)
  setwd("../src")
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
      perceptron@x[c(((i - 1) * col) + j)] <- data[i, j]
    }
  }
  return(perceptron@x)
}


archive_name <- function(){
  cat("or \nand \nimplies \nnumbers")
  switch(readline(),
    or={
      file <- "or.csv"
    },
    and={
      file <- "and.csv"
    },
    implies={
      file <- "implies.csv"
    },
    numbers={
      file <- "numbers/one.csv"
    },
    stop("Enter something that switches me!")
  )
  return(file)
}

main <- function(data, row, col, perceptron, error){
  file <<- archive_name()
  
  data <<- archive_read(file)

  row <<- set_row(data)

  col <<- set_col(data)

  perceptron@x <<- convert_to_vector(data, row, col)

  perceptron@w <<- random_weights()

  w <<- training(data, perceptron, error)
}

main(data, row, col, perceptron, error)
