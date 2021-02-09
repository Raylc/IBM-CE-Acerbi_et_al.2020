library(tidyverse)

#biased trasmission model
biased_transmission_direct <- function (N, s_a, s_b, p_0, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, 
                                        replace = TRUE, prob = c(p_0, 1 - p_0))) 
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # For each individual, pick a random individual from the previous generation
      demonstrator_trait <- 
        tibble(trait = sample(previous_population$trait, N, replace = TRUE)) 
      
      # Biased probabilities to copy:
      copy_a <- sample(c(TRUE, FALSE), N, prob = c(s_a, 1 - s_a), replace = TRUE) 
      copy_b <- sample(c(TRUE, FALSE), N, prob = c(s_b, 1 - s_b), replace = TRUE) 
      
      # If the demonstrator has trait A and the individual wants to copy A, then copy A
      if (nrow(population[copy_a & demonstrator_trait$trait == "A", ]) > 0) {
        population[copy_a & demonstrator_trait$trait == "A", ]$trait <- "A" 
      }  
      
      # If the demonstrator has trait B and the individual wants to copy B, then copy B
      if (nrow(population[copy_b & demonstrator_trait$trait == "B", ]) > 0) {
        population[copy_b & demonstrator_trait$trait == "B", ]$trait <- "B" 
      }  
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

data_model <- biased_transmission_direct(N = 10000, s_a = 0.6, s_b = 0.5 , 
                                         p_0 = 0.01, t_max = 150, r_max = 5)
plot_multiple_runs(data_model)