library(tidyverse)


rogers_model2 <- function(N, t_max, r_max, w = 1, b = 0.5, c, s = 0, mu, p, u) {
  
  # Check parameters to avoid negative fitnesses
  if (b * (1 + c) > 1 || b * (1 + s) > 1) {
    stop("Invalid parameter values: ensure b*(1+c) < 1 and b*(1+s) < 1")
  }
  
  # Create output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   p_SL = as.numeric(rep(NA, t_max * r_max)), 
                   p_IL = as.numeric(rep(NA, t_max * r_max)), 
                   p_CL = as.numeric(rep(NA, t_max * r_max)), 
                   W = as.numeric(rep(NA, t_max * r_max)))
  
  for (r in 1:r_max) {
    
    # Create a population of individuals
    population <- tibble(learning = rep("individual", N), 
                         behaviour = rep(NA, N), fitness = rep(NA, N))
    
    # Initialise the environment
    E <- 0
    
    for (t in 1:t_max) {
      
      # Now we integrate fitnesses into the learning stage
      population$fitness <- w  
      
      # 1. Social learning
      if (sum(population$learning == "social") > 0) {
        # Subtract cost b*s from fitness of social learners
        population$fitness[population$learning == "social"] <- 
          population$fitness[population$learning == "social"] - b*s
        # Update behaviour
        population$behaviour[population$learning == "social"] <- 
          sample(previous_population$behaviour, sum(population$learning == "social"), replace = TRUE)
        
      }
      
      # 2. Individual learning
      # Subtract cost b*c from fitness of individual learners
      population$fitness[population$learning == "individual"] <- 
        population$fitness[population$learning == "individual"] - b*c
      # Update behaviour
      learn_correct <- sample(c(TRUE, FALSE), N, prob = c(p, 1 - p), replace = TRUE)
      population$behaviour[learn_correct & population$learning == "individual"] <- E
      population$behaviour[!learn_correct & population$learning == "individual"] <- E - 1
      
      
      # 3. Critical social learning
      if (sum(population$learning == "critical") > 0) {
        
        # Subtract b*s from fitness of socially learning critical learners
        population$fitness[population$learning == "critical"] <- 
          population$fitness[population$learning == "critical"] - b*s
        
        # First critical learners socially learn
        population$behaviour[population$learning == "critical"] <- 
          sample(previous_population$behaviour, 
                 sum(population$learning == "critical"), replace = TRUE)
        
        # Subtract b*c from fitness of individually learning critical learners
        population$fitness[population$learning == "critical" & population$behaviour != E] <- 
          population$fitness[population$learning == "critical" & population$behaviour != E] - b*c
        
        # Individual learning for those critical learners who did not copy correct behaviour
        population$behaviour[learn_correct & population$learning == "critical" & population$behaviour != E] <- E
        population$behaviour[!learn_correct & population$learning == "critical" & population$behaviour != E] <- E - 1
      }
      
      # 4. Calculate fitnesses (now only need to do the b bonus or penalty)
      population$fitness[population$behaviour == E] <-
        population$fitness[population$behaviour == E] + b  
      population$fitness[population$behaviour != E] <-
        population$fitness[population$behaviour != E] - b
      
      # 5. store population characteristics in output
      output[output$generation == t & output$run == r, ]$p_SL <- 
        mean(population$learning == "social")
      output[output$generation == t & output$run == r, ]$p_IL <- 
        mean(population$learning == "individual")
      output[output$generation == t & output$run == r, ]$p_CL <- 
        mean(population$learning == "critical")
      output[output$generation == t & output$run == r, ]$W <- 
        mean(population$fitness)
      
      # 6. Reproduction
      previous_population <- population
      population$behaviour <- NA
      population$fitness <- NA
      
      # Individual learners
      if (sum(previous_population$learning == "individual") > 0) {
        fitness_IL <- sum(previous_population$fitness[previous_population$learning == "individual"]) / 
          sum(previous_population$fitness)
      } else {
        fitness_IL <- 0
      }
      
      # Social learners
      if (sum(previous_population$learning == "social") > 0) {
        fitness_SL <- sum(previous_population$fitness[previous_population$learning == "social"]) / 
          sum(previous_population$fitness)
      } else {
        fitness_SL <- 0
      }
      
      population$learning <- sample(c("individual", "social", "critical"), size = N, 
                                    prob = c(fitness_IL, fitness_SL, 1 - (fitness_SL + fitness_IL)), replace = TRUE)
      
      mutation <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE)
      
      previous_population2 <- population
      
      population$learning[mutation & previous_population2$learning == "individual"] <- 
        sample(c("critical", "social"), 
               sum(mutation & previous_population2$learning == "individual"), 
               prob = c(0.5, 0.5), replace = TRUE)
      
      population$learning[mutation & previous_population2$learning == "social"] <- 
        sample(c("critical", "individual"), 
               sum(mutation & previous_population2$learning == "social"), 
               prob = c(0.5, 0.5), replace = TRUE)
      
      population$learning[mutation & previous_population2$learning == "critical"] <- 
        sample(c("individual", "social"), 
               sum(mutation & previous_population2$learning == "critical"), 
               prob = c(0.5, 0.5), replace = TRUE)
      
      # 7. Potential environmental change
      if (runif(1) < u) E <- E + 1
    }
  }
  # Export data from function
  output
}



data_model <- rogers_model2(N = 1000, t_max = 200, r_max = 10, c = 0.9, mu = 0.01, p = 1, u = 0.2)

plot_prop <- function(data_model) {
  
  names(data_model)[3:5] <- c("social", "individual", "critical")
  data_model_long <- pivot_longer(data_model, -c(W, generation, run), 
                                  names_to = "learning", 
                                  values_to = "proportion")
  
  ggplot(data = data_model_long, aes(y = proportion, x = generation, colour = learning)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "proportion of learners")
}

plot_prop(data_model)

ggsave("Critical social learner.png", width = 20, height = 15, units = "cm")

plot_W(data_model, c = 0.9, p = 1)
ggsave("Critical social learner fitness.png", width = 20, height = 15, units = "cm")


