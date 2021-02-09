library(tidyverse)

#unbiased transmission model
unbiased_mutation <- function(N, mu, p_0, t_max, r_max) {
  # Create the output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max))) 
  
  for (r in 1:r_max) {
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, 
                                        prob = c(p_0, 1 - p_0)))
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Determine 'mutant' individuals
      mutate <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
      
      # If there are 'mutants' from A to B
      if (nrow(population[mutate & previous_population$trait == "A", ]) > 0) { 
        # Then flip them to B
        population[mutate & previous_population$trait == "A", ]$trait <- "B" 
      }
      
      # If there are 'mutants' from B to A
      if (nrow(population[mutate & previous_population$trait == "B", ]) > 0) { 
        # Then flip them to A
        population[mutate & previous_population$trait == "B", ]$trait <- "A" 
      }
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

plot_multiple_runs <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

# Model testing. The key thing is that unbiased mutation applied individually
# so regardless of initial trait distribution it will converge to 0.5.
data_model <- unbiased_mutation(N = 100, mu = 0.05, p_0 = 0.5, t_max = 200, r_max = 5)
plot_multiple_runs(data_model)





#biased transmission model
biased_mutation <- function(N, mu_b, p_0, t_max, r_max) {
  # Create the output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, 
                                        prob = c(p_0, 1 - p_0)))
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Determine 'mutant' individuals
      mutate <- sample(c(TRUE, FALSE), N, prob = c(mu_b, 1 - mu_b), replace = TRUE) 
      
      # If there are 'mutants' from B to A
      if (nrow(population[mutate & previous_population$trait == "B", ]) > 0) {
        # Then flip them to A
        population[mutate & previous_population$trait == "B", ]$trait <- "A"
      }
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

plot_multiple_runs <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

data_model <- biased_mutation(N = 100, mu_b = 0.05, p_0 = 0, t_max = 200, r_max = 5)
plot_multiple_runs(data_model)