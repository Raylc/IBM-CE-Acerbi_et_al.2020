library(tidyverse)

#demonstrator-based indirect bias transmission model
biased_transmission_demonstrator <- function(N, p_0, p_s, p_low, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, 
                                        replace = TRUE, prob = c(p_0, 1 - p_0)),
                         status = sample(c("high", "low"), N,
                                         replace = TRUE, prob = c(p_s, 1 - p_s))) 
    
    # Assign copying probabilities based on individuals' status
    p_demonstrator <- rep(1,N)
    p_demonstrator[population$status == "low"] <- p_low
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Copy traits based on status
      if(sum(p_demonstrator) > 0){
        demonstrator_index <- sample (N, prob = p_demonstrator, replace = TRUE)
        population$trait <- previous_population$trait[demonstrator_index]
      }
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
    
  }
  # Export data from function
  output 
}


#Plotting function and its realization
plot_multiple_runs <- function(data_model2) {
  ggplot(data = data_model2, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

# Experimenting with different parameters
data_model <- biased_transmission_demonstrator(N = 100, p_s = 0.05, p_low=0.0001, 
                                               p_0 = 0.5, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)
ggsave("context-biased model1.png", width = 20, height = 15, units = "cm")



data_model <- biased_transmission_demonstrator(N = 10000, p_s = 0.005, p_low=0.0001, 
                                               p_0 = 0.5, t_max = 200, r_max = 10)
plot_multiple_runs(data_model)
ggsave("context-biased model2.png", width = 20, height = 15, units = "cm")

