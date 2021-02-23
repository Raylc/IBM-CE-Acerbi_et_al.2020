library(tidyverse)

vertical_transmission <- function(N, p_0, b, t_max, r_max) {
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
      
      # Randomly pick mothers and fathers
      mother <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))  
      father <- tibble(trait = sample(previous_population$trait, N, replace = TRUE)) 
      
      # Prepare next generation
      population <- tibble(trait = as.character(rep(NA, N))) 
      
      # Both parents are A, thus child adopts A
      both_A <- mother$trait == "A" & father$trait == "A"
      if (sum(both_A) > 0) {
        population[both_A, ]$trait <- "A"  
      }
      
      # Both parents are B, thus child adopts B
      both_B <- mother$trait == "B" & father$trait == "B"
      if (sum(both_B) > 0) {
        population[both_B, ]$trait <- "B" 
      }
      # If any empty NA slots (i.e. one A and one B parent) are present
      if (anyNA(population)) {  
        # They adopt A with probability b
        population[is.na(population)[,1],]$trait <- 
          sample(c("A", "B"), sum(is.na(population)), prob = c(b, 1 - b), replace = TRUE)
      }
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

data_model <- vertical_transmission(N = 10000, p_0 = 0.01, b = 0.6, 
                                    t_max = 50, r_max = 5)
plot_multiple_runs(data_model)
ggsave("vertical transmission model1.png", width = 20, height = 15, units = "cm")

data_model <- vertical_transmission(N = 10000, p_0 = 0.1, b = 0.5, 
                                    t_max = 50, r_max = 5)
plot_multiple_runs(data_model)
ggsave("vertical transmission model2.png", width = 20, height = 15, units = "cm")
