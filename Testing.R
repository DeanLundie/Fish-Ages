# R file for testing

# Checking that the initialisation function outputs three columns (one for each parameter estimate)
# input function to be tested and number of columns to test for


len_test <- function(f,x,n) { # input function to be tested and number of columns to test for
  if(length(f(x)) == n) {
    return("PASSED")
  }
  else {
    return("FAILED")
  }
}

# Checking that the output values for the mean and lambda parameters are positive:

pos_test <- function(f, x) {
  count <- NULL
  init <- f(x)
  params <- data.frame(init[,1], init[,3])
  for (i in 1:nrow(init)) {
    for (j in 1:ncol(init)) {
      if (init[i, j] > 0) {
        count <- c(count, 1)
      }
      else{
        count <- c(count, 0)
      }
    }
  }
  if (length(count) == ncol(init) * nrow(init)) {
    print("PASSED")
  }
  else{
    print("FAILED")
  }
}

# Testing that the lambda k values are between 0 and 1 (as they are probabilities)


prob_test <- function(f, x) {
  init <- f(x)
  for (i in 1:nrow(init)) {
    if (init[i, 3] >= 0 & init[i, 3] <= 1) {
      return("PASSED")
    }
    else {
      return("FAILED")
    }
  }
}

# Test for ensuring parameter estimates for lambda_k (the weights) sum to 1 as they represent probabilities!
# Inputs: f = function to be tested, x = data to test on

sum_test <- function(f,x) {
  function_val <- f(x)
  if(sum(function_val$est_lambda) == 1) {
    return("PASSED")
  }
  else {
    return("FAILED") # if function fails to initialise parameter values correctly
  }
}


len_test(initialisation_step,x,3)
sum_test(initialisation_step,x)


# Test for ensuring the posterior probabilities for each group within an observation sum to 1
# Inputs: f = function to be tested


exp_test <- function(f){
  
  # Generating fake data #
  x <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 2, 1))
  estimates <- data.frame(est_mean=c(0,1,2), est_sd=c(1,1,1), est_lambda = c(0.33, 0.33, 0.34))
  post_probs <- f(x, estimates)
  sums <- NULL
  
  # Test to ensure rows add up to 1
  for (i in 1:nrow(post_probs)) {
    sums[i] <- sum(post_probs[i,])
  }
  if(sum(sums) == length(sums)) {
    return("Passed")
  }
  else {
    return("Failed")
  }
}

# Test for ensuring that convergence function has the correct output format
# Inputs: f = function to be tested
con_test <- function(f){
  
  # generate some fake data
  x <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 2, 1))
  estimates <- data.frame(est_mean=c(0,1,2), est_sd=c(1,1,1), est_lambda = c(0.33, 0.33, 0.34))
  e <- 1e-08
  
  # assume this is the first run of some cycle
  previous <- list(likelihood = NULL, result=FALSE)
  output <- f(x, estimates, e, previous)
  if (is.numeric(output[["likelihood"]][0]) & is.logical(output[["result"]])){
    return("PASSED")
  }
  else {
    return("FAILED")}
}

# Testing the maximisation step of the EM algorithm
test_maximisation_step <- function(){
  
  # Input case
  probs_vector <- c(0.7, 0.2, 0.05, 0.25, 0.6, 0.1, 0.05, 0.2, 0.85)
  nrow <- 3
  ncol <- 3
  p <- matrix(probs_vector, nrow = nrow, ncol = ncol)  # probability matrix
  x <- c(24.7, 34.5, 63.9) # data
  out <- maximisation_step(p, x)
  
  # Does function return 3x3 data frame?
  count <- 0
  for (i in 1:nrow){
    if (length(out[i, ]) == 3){
      count <- count + 1
    }
  }
  for (j in 1:ncol){
    if (length(out[, j]) == 3){
      count <- count + 1
    }
  }
  if (count == nrow + ncol){
    cat("nxk Matrix Test: PASSED")
  }
  else{
    cat("nxk Matrix Test: FAILED")
  }
  
  # Do the lambda probabilities add up to 1?
  if (sum(out[, 3]) == 1){
    cat("\nProbability Test: PASSED")
  } else{
    cat("\nProbability Test: FAILED")
  }
  
  # Are all lambda probabilities between 0 and 1?
  count_2 <- 0
  for (i in 1:nrow){
    if (out[i,3] >= 0 || out[i,3] <= 1){
      count_2 <- count_2 + 1
    }
  }
  if (count_2 == nrow){
    cat("\nProbabilities between 0 and 1 Test: PASSED")
  } else{
    cat("\nProbabilities between 0 and 1 Test: FAILED")
  }
  
  # Are all values in output positive numbers?
  count_1 <- 0
  for (i in 1:nrow){
    for (j in 1:ncol){
      if(out[i,j] > 0){
        count_1 <- count_1 + 1
      }
    }
  }
  if (count_1 == nrow*ncol){
    cat("\nPositive Parameter Estimates Test: PASSED")
  } else{
    cat("\nPositive Parameter Estimates Test: FAILED")
  }
}





