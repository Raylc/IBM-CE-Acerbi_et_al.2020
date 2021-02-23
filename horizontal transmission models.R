vertical_horizontal_transmission <- function(N, p_0, b, n, g, t_max, r_max) {
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
      # Vertical transmission --------------------------------------------------
      
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
      
      # Horizontal transmission ------------------------------------------------
      
      # Previous_population are children before horizontal transmission
      previous_population <- population 
      
      # N_B = number of Bs
      N_B <- length(previous_population$trait[previous_population$trait == "B"])  
      
      # If there are B individuals to switch, and n is not zero
      if (N_B > 0 & n > 0) {  
        # For each B individual...
        for (i in 1:N_B) {  
          # Pick n demonstrators
          demonstrator <- sample(previous_population$trait, n, replace = TRUE)
          # Get probability g
          copy <- sample(c(TRUE, FALSE), n, prob = c(g, 1-g), replace = TRUE) 
          # if any demonstrators with A are to be copied
          if ( sum(demonstrator == "A" & copy == TRUE) > 0 ) {  
            # The B individual switches to A
            population[previous_population$trait == "B",]$trait[i] <- "A"  
          }
          
        }
      }
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

data_model <- vertical_horizontal_transmission(N = 5000, p_0 = 0.01, b = 0.5, n = 5, 
                                               g = 0.1, t_max = 50, r_max = 5)
plot_multiple_runs(data_model)
ggsave("horizontal transmission model1.png", width = 20, height = 15, units = "cm")


data_model_v <- vertical_horizontal_transmission(N = 5000, p_0 = 0.01, b = 0.6, n = 0, 
                                                 g = 0, t_max = 50, r_max = 5)
plot_multiple_runs(data_model_v)
ggsave("horizontal transmission model2.png", width = 20, height = 15, units = "cm")



data_model_hn2 <- vertical_horizontal_transmission(N = 5000, p_0 = 0.01, b = 0.5, n = 2, 
                                                   g = 0.1, t_max = 50, r_max = 5)
plot_multiple_runs(data_model_hn2)
ggsave("horizontal transmission model3.png", width = 20, height = 15, units = "cm")
