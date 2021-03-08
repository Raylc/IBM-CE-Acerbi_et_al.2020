library(tidyverse)

rogers_model <- function(N, t_max, r_max, w = 1, b = 0.5, c, s = 0, mu, p, u) {
  
  # Check parameters to avoid negative fitnesses
  if (b * (1 + c) > 1 || b * (1 + s) > 1) {
    stop("Invalid parameter values: ensure b*(1+c) < 1 and b*(1+s) < 1")
  }
  
  # Create output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   p_SL = as.numeric(rep(NA, t_max * r_max)), 
                   W = as.numeric(rep(NA, t_max * r_max)))
  
  for (r in 1:r_max) {
    
    # Create a population of individuals
    population <- tibble(learning = rep("individual", N), 
                         behaviour = rep(NA, N), fitness = rep(NA, N))
    
    # Initialise the environment
    E <- 0
    
    for (t in 1:t_max) {
      
      # 1. Social learning
      if (sum(population$learning == "social") > 0) {
        population$behaviour[population$learning == "social"] <- 
          sample(previous_population$behaviour, sum(population$learning == "social"), replace = TRUE)
      }
      
      # 2. individual learning
      learn_correct <- sample(c(TRUE, FALSE), N, prob = c(p, 1 - p), replace = TRUE)
      population$behaviour[learn_correct & population$learning == "individual"] <- E
      population$behaviour[!learn_correct & population$learning == "individual"] <- E - 1
      
      # 3. Calculate fitnesses
      population$fitness <- w  
      
      population$fitness[population$behaviour == E] <- 
        population$fitness[population$behaviour == E] + b  
      
      population$fitness[population$behaviour != E] <- 
        population$fitness[population$behaviour != E] - b
      
      population$fitness[population$learning == "individual"] <- 
        population$fitness[population$learning == "individual"] - b*c  
      
      population$fitness[population$learning == "social"] <- 
        population$fitness[population$learning == "social"] - b*s 
      
      # 4. Store population characteristics in output
      output[output$generation == t & output$run == r, ]$p_SL <- 
        mean(population$learning == "social")
      output[output$generation == t & output$run == r, ]$W <- 
        mean(population$fitness)
      
      # 5. Reproduction
      previous_population <- population
      population$behaviour <- NA
      population$fitness <- NA
      
      if (sum(previous_population$learning == "individual") > 0) {
        fitness_IL <- sum(previous_population$fitness[previous_population$learning == "individual"]) / 
          sum(previous_population$fitness)
      } else {
        fitness_IL <- 0
      }
      
      population$learning <- sample(c("individual", "social"), size = N, 
                                    prob = c(fitness_IL, 1 - fitness_IL), replace = TRUE)     
      
      mutation <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE)
      
      previous_population2 <- population
      population$learning[previous_population2$learning == "individual" & mutation] <- "social"  
      population$learning[previous_population2$learning == "social" & mutation] <- "individual"  
      
      # 6. Potential environmental change
      if (runif(1) < u) E <- E + 1
      
    }
  }
  # Export data from function
  output
  
}

plot_multiple_runs_p_SL <- function(data_model) {
  ggplot(data = data_model, aes(y = p_SL, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "proportion of social learners")
}


plot_W <- function(data_model, w = 1, b = 0.5, c, p) {
  ggplot(data = data_model, aes(y = W, x = generation)) +
    geom_line(aes(color = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    geom_hline(yintercept = w + b * (2 * p - c - 1), linetype = 2) +
    ylim(c(0, NA)) +
    theme_bw() +
    labs(y = "mean population fitness")
}

data_model <- rogers_model(N = 1000, t_max = 200, r_max = 10, c = 0.4, mu = 0.01, p = 1, u = 0.2)
plot_multiple_runs_p_SL(data_model)
ggsave("Roger's paradox 1.png", width = 20, height = 15, units = "cm")

plot_W(data_model, c = 0.4, p = 1)
ggsave("Roger's paradox fitness1.png", width = 20, height = 15, units = "cm")


data_model <- rogers_model(N = 1000, t_max = 200, r_max = 10, c = 0.9, mu = 0.01, p = 1, u = 0)
plot_multiple_runs_p_SL(data_model)
ggsave("Roger's paradox 2.png", width = 20, height = 15, units = "cm")

plot_W(data_model, c = 0.9, p = 1)
ggsave("Roger's paradox fitness2.png", width = 20, height = 15, units = "cm")


