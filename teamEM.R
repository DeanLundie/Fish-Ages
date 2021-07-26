# File for developing the main function and all constituent parts

library(tidyverse)
library(dplyr)

# Function for calculating initial parameters from data (Initialise)
# INPUTS:
# x: data frame (length and age columns)
# OUTPUTS: data frame containing initial parameter estimates (est_mean, est_sd,
# est_lambda)
# Function uses the k-means clustering algorithm which will allow us to obtain 
# initial group labels on the fish length data.

initialisation_step <- function(x) { 
  lengths <- x$Length 
  # Specify how many groups we want to split the data into and run algorithm
  lengths.kmeans <- kmeans(lengths,3) 
  # Access the initial group labels of length data and store in a data frame 
  lengths.groups <- lengths.kmeans$cluster 
  lengths.df <- data.frame(x = lengths, cluster = lengths.groups) 
  # Group observations together by label and calculate mean length and sd of obs
  lengths.summary.df <- lengths.df %>%
    group_by(cluster) %>%
    summarize(est_mean = mean(x), est_sd = sd(x), size = n()) 
  lengths.summary.df %>%
    select(cluster, est_mean, est_sd)
  # Get the lambda_k's
  lengths.summary.df <- lengths.summary.df %>%
    mutate(est_lambda = size / sum(size))
  lengths.summary.df %>%
    select(est_lambda)
  parameters <- data.frame(select(lengths.summary.df, -c(cluster, size)))
  return("parameters" = parameters)
  
}

# Function for calculating probability that each observation belongs to each
# group (Expectation)
# INPUTS:
# x: vector with n elements of fish length data
# est_lambda: vector with k elements of probabilities that fish is in group k
# est_mean: vector with k elements of estimated means for each group k
# est_sd: vector with k elements of estimated standard deviations for each
# group k
# OUTPUTS:
# post_probs  = n x k matrix with posterior probabilities for each observed
# value being in group k

expectation_step <- function(x, parameters){
  est_mean <- parameters$est_mean
  est_sd <- parameters$est_sd
  est_lambda <- parameters$est_lambda
  # Creating empty vectors and matrix
  marg_probs  <- NULL
  total       <- NULL
  post_probs  <- matrix(ncol = 3, nrow = length(x))
  # Calculating the marginal probabilities by summing over the groups
  for (i in 1:length(x)){
    for (k in 1:3){
      gauss_like <- dnorm(x[i], mean = est_mean[k], sd = est_sd[k])
      gauss_prior <- est_lambda[k]
      marg_probs[k] <- gauss_like * gauss_prior
    }
    total[i] <- sum(marg_probs)
  }
  # Calculating the posterior probability for each observed value and group
  for (i in 1:length(x)){
    for (k in 1:3){
      gauss_like <- dnorm(x[i], mean = est_mean[k], sd = est_sd[k])
      gauss_prior <- est_lambda[k]
      posterior <- (gauss_like * gauss_prior) / (total[i])
      post_probs[i,k] <- posterior
    }
  }
  # This will return an n x k matrix of posterior probabilities
  return(post_probs)
}

# Function for updating best estimates of means and variances (Maximisation)
# INPUTS:
# post_probs: n x k matrix of posterior probabilities with [i,k]th entry
# corresponding to the probability that observation i belongs to group k
# x: vector with n elements of fish length data
# OUTPUTS: 
# estimates: data frame of estimates for group means(column 1), standard 
# deviations(column 2) and probabilities(column 3). row i corresponds to group i

maximisation_step <- function(post_probs, x){
  # Number of observations
  n <- nrow(post_probs)
  # Vectors for estimates of group means, standard deviations and probabilities
  est_mean <- NULL
  est_sd <- NULL
  est_lambda <- NULL
  # Loop over each group
  for (k in 1:3){
    # Expression for numerator of group mean estimate
    mean_numerator <- NULL
    # Expression for weighting factor (denominator of group mean estimate)
    weight <- NULL
    # Expression for numerator of group standard deviation estimate
    sd_numerator <- NULL
    for (i in 1:n){
      # Calculate each element of numerator in formula for group mean estimate
      mean_numerator[i] <- post_probs[i,k]*x[i]
      # Calculate each element of denominator in formula for group mean estimate
      weight[i] <- post_probs[i,k]
    }
    est_mean[k] <- sum(mean_numerator)/sum(weight)
    est_lambda[k] <- 1/n * sum(weight)
    for (i in 1:n){
      # Calculate each element of numerator in formula for group standard
      # deviation estimate
      sd_numerator[i] <- post_probs[i,k]* (x[i] - est_mean[k])**2
    }
    est_sd[k] <- sqrt(sum(sd_numerator)/sum(weight))
  }
  estimates <- data.frame(est_mean, est_sd, est_lambda)
  # This will return the new best estimates for group means, standard deviations
  # and probabilities
  return(estimates)
}

# Function for testing if the EM algorithm has converged (Convergence)
# INPUTS
# x: the fish length observations from the original data
# estimates: data frame from previous function with estimates
# e: the desired tolerance level for the difference in subsequent log likelihoods
# L_prev: the previous likelihood
# OUPUT: 
# a list containing two named quantities
# likelihood: the likelihood of the data given the estimates, for next iteration
# result: TRUE if convergence achieved, FALSE otherwise
# NOTE: output likelihood no log-likelihood so we can use the log identity
# log(A/B) = log(A) - log(B) to save computation time

convergence_step <- function(x, estimates, e, previous){
  likelihood <- previous[["likelihood"]]
  est_mean <- estimates$est_mean 
  est_sd <- estimates$est_sd
  est_lambda <- estimates$est_lambda
  if (length(likelihood) == 0) {
    L_prev <- -Inf
  }
  else{L_prev <- tail(likelihood, n=1)}
  L <- 0
  for (i in x){
    s <- 0
    # run dnorm on a three element vector, so all likelihoods done in one step
    obsvec <- rep(i, 3)
    L <- L + log(sum(est_lambda*dnorm(obsvec, est_mean, est_sd)))
  }
  likelihood <- c(likelihood, L)
  if (abs(L-L_prev) < e){
    return(list(result = TRUE, likelihood = likelihood))
  }
  else{
    return(list(result = FALSE, likelihood = likelihood))
  }
}

# Main function for executing all steps
teamEM <- function(data, epsilon = 1e-08, maxit = 1000){
  x <- data$Length
  i <- 1
  initial <- initialisation_step(data)
  L_prev <- 1e-20
  convergence <- list(result = FALSE, likelihood = NULL)
  convergence <- convergence_step(x, initial, epsilon, convergence)
  parameters <- initial
  while (!(convergence[["result"]]) & (i < maxit)){
    expectation <- expectation_step(x, parameters)
    parameters <- maximisation_step(post_probs = expectation, x = x)
    convergence <- convergence_step(x, parameters, epsilon, convergence)
    i <- i + 1
  }
  colnames(parameters) <- c("mu", "sigma", "lambda")
  colnames(initial) <- c("mu", "sigma", "lambda")
  # need to make sure groups are in correct order
  parameters <- arrange(parameters, mu)
  initial <- arrange(initial, mu)
  rownames(parameters) <- c("Age1", "Age2", "Age3")
  rownames(initial) <- c("Age1", "Age2", "Age3")
  posterior <- as.data.frame(expectation)
  return(list(estimates = parameters, inits = initial, 
              converged = convergence[["result"]], posterior = posterior,
              likelihood = convergence[["likelihood"]]))
} 




