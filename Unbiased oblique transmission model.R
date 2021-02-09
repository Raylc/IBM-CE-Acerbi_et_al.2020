library(tidyverse)

##unbiased trasmission model with single run with fixed initial trait frequency
unbiased_transmission_1 <- function(N, t_max) {
  population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE))
  
  output <- tibble(generation = 1:t_max, p = rep(NA, t_max))
  
  output$p[1] <- sum(population$trait == "A") / N
  
  for (t in 2:t_max) {
    # Copy individuals to previous_population tibble
    previous_population <- population 
    
    # Randomly copy from previous generation
    population <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))
    
    # Get p and put it into output slot for this generation t
    output$p[t] <- sum(population$trait == "A") / N 
  }
  # Export data from function
  output
  

}

#Save the simulation outputs in to data_model 
data_model1 <- unbiased_transmission_1(N = 100, t_max = 200)

#Plotting function and its realization
plot_single_run <- function(data_model1) {
  ggplot(data = data_model1, aes(y = p, x = generation)) +
    geom_line() +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

plot_single_run(data_model1)





##unbiased trasmission model with multiple runs with fixed initial trait frequency
unbiased_transmission_2 <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max))) 
  # For each run
  for (r in 1:r_max) { 
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <-
      sum(population$trait == "A") / N 
    
    # For each generation
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Randomly copy from previous generation
      population <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

#Save the simulation outputs in to data_model 
data_model2 <- unbiased_transmission_2(N = 100, t_max = 200, r_max = 5)

#Plotting function and its realization
plot_multiple_runs <- function(data_model2) {
  ggplot(data = data_model2, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

plot_multiple_runs(data_model2)


##unbiased trasmission model with multiple runs with non-fixed initial trait frequency
unbiased_transmission_3 <- function(N, p_0, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  # For each run
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, 
                                        prob = c(p_0, 1 - p_0)))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Randomly copy from previous generation
      population <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N  
    }
  }
  # Export data from function
  output 
}

# This model shares the same plotting function with model 2
plot_multiple_runs <- function(data_model3) {
  ggplot(data = data_model3, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

data_model3 <- unbiased_transmission_3(N = 10000, p_0 = 0.2, t_max = 200, r_max = 5)
plot_multiple_runs(data_model3)


#Preparation
library(tidyverse)
#Set up population size, which is fixed across generations.
N <- 100
#Set up maximum generation
t_max <- 200
#Create a table storing the traits of the first-generation-individuals.
population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE))


#Check the population table
population
#Check the trait of individual of interest
population$trait[41]


#Create a table storing the trait frequency in each generations.
output <- tibble(generation = 1:t_max, p = rep(NA, t_max))
#Assign the relative frequency of trait A to the first generation using
#the sample function with equal probability.
output$p[1] <- sum(population$trait == "A") / N
#Check the output table
output