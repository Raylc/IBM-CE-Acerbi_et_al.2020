library(tidyverse)

conformist_transmission <- function (N, p_0, D, t_max, r_max) {
  
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
      
      # Create a tibble with a set of 3 randomly-picked demonstrators for each agent
      demonstrators <- tibble(dem1 = sample(population$trait, N, replace = TRUE), 
                              dem2 = sample(population$trait, N, replace = TRUE), 
                              dem3 = sample(population$trait, N, replace = TRUE))
      
      # Get the number of As in each 3-demonstrator combinations
      num_As <- rowSums(demonstrators == "A")
      
      # For 3-demonstrator combinations with all As, set to A
      population$trait[num_As == 3] <- "A"  
      # For 3-demonstrator combinations with all Bs, set to B
      population$trait[num_As == 0] <- "B"  
      
      prob_majority <- sample(c(TRUE, FALSE), 
                              prob = c((2/3 + D/3), 1 - (2/3 + D/3)), N, replace = TRUE)
      prob_minority <- sample(c(TRUE, FALSE), 
                              prob = c((1/3 - D/3), 1 - (1/3 - D/3)), N, replace = TRUE)
      
      # 3-demonstrator combinations with two As and one B
      if (nrow(population[prob_majority & num_As == 2, ]) > 0) {
        population[prob_majority & num_As == 2, ] <- "A"
      }
      if (nrow(population[prob_majority == FALSE & num_As == 2, ]) > 0) {
        population[prob_majority == FALSE & num_As == 2, ] <- "B"
      }  
      # 3-demonstrator combinations with one A and two Bs
      if (nrow(population[prob_minority & num_As == 1, ]) > 0) {
        population[prob_minority & num_As == 1, ] <- "A"
      }
      if (nrow(population[prob_minority == FALSE & num_As == 1, ]) > 0) {
        population[prob_minority == FALSE & num_As == 1, ] <- "B"
      }  
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output  
}


data_model <- conformist_transmission(N = 1000, p_0 = 0.8, D = 0.5, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)