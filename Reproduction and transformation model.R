library(tidyverse)
N <- 1000
population <- tibble(P = runif(N))

reproduction <- function(N, t_max, r_max, mu) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Select a pair of demonstrator for each individual
      demonstrators <- tibble(P1 = sample(previous_population$P, N, replace = TRUE), 
                              P2 = sample(previous_population$P, N, replace = TRUE))
      
      # Copy the one with the trait value closer to 1
      copy <- pmax(demonstrators$P1, demonstrators$P2)
      
      # Add mutation
      population$P <- copy + runif(N, -mu, +mu)
      
      # Keep the  traits' value in the boundaries [0,1]
      population$P[population$P > 1] <- 1
      population$P[population$P < 0] <- 0
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$P) / N 
    }
  }
  # Export data from function
  output 
}



plot_multiple_runs_p <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (average value of P)")
}

data_model <- reproduction(N = 1000, t_max = 20, r_max = 5, mu = 0.05)
plot_multiple_runs_p(data_model)
ggsave("reproduction continuous trait.png", width = 20, height = 15, units = "cm")


transformation <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population
      
      # Only one demonstrator is selected at random for each individual
      demonstrators <- tibble(P = sample(previous_population$P, N, replace = TRUE))
      
      # The new P is in between the demonstrator's value and 1
      population$P <- demonstrators$P + runif(N, max = 1 - demonstrators$P)
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- sum(population$P) / N 
    }
  }
  # Export data from function
  output 
}

data_model <- transformation(N = 1000, t_max = 20, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("transformation continuous trait.png", width = 20, height = 15, units = "cm")









#Emergent similarity

reproduction <- function(N, t_max, r_max, mu) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   d = as.numeric(rep(NA, t_max * r_max)))
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population
      
      demonstrators <- tibble(P1 = sample(previous_population$P, N, replace = TRUE), 
                              P2 = sample(previous_population$P, N, replace = TRUE))
      
      copy <- pmax(demonstrators$P1, demonstrators$P2)
      
      population$P <- copy + runif(N, -mu, +mu)
      population$P[population$P > 1] <- 1
      population$P[population$P < 0] <- 0
      
      # Output:
      output[output$generation == t & output$run == r, ]$p <- sum(population$P) / N 
      output[output$generation == t & output$run == r, ]$d <- sum(abs(population$P - copy)) / N
    }
  }
  # Export data from function
  output 
}

transformation <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   d = as.numeric(rep(NA, t_max * r_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      demonstrators <- tibble(P = sample(previous_population$P, N, replace = TRUE))
      
      population$P <- demonstrators$P + runif(N, max = 1-demonstrators$P)
      
      # Output
      output[output$generation == t & output$run == r, ]$p <- sum(population$P) / N 
      output[output$generation == t & output$run == r, ]$d <- sum(abs(population$P - demonstrators$P)) / N
    }
  }
  # Export data from function
  output 
}


data_model_reproduction <- reproduction(N = 1000, t_max = 20, r_max = 5, mu = 0.05)
data_model_transformation <- transformation(N = 1000, t_max = 20, r_max = 5)

data_to_plot <- tibble(distance = c(na.omit(data_model_reproduction$d), 
                                    na.omit(data_model_transformation$d)), 
                       condition = rep(c("reproduction", "transformation"), each = 95),
                       generation = rep(2:20,10),
                       run = as.factor(rep(1:10, each = 19)))
ggplot(data = data_to_plot, aes(y = distance, x = generation, group = run, color = condition)) +
  geom_line() +   
  geom_point() +
  theme_bw() +
  labs(y = "d (average distance observer/demonstrator)")


ggsave("reproduction_transformation continuous trait distance.png", width = 20, height = 15, units = "cm")










# Cultural fitness

reproduction <- function(N, t_max, r_max, mu) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   cov_W_P = as.numeric(rep(NA, t_max * r_max)))
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Sample the demonstrators using their indexes
      demonstrators <- cbind(sample(N, N, replace = TRUE), sample(N, N, replace = TRUE))
      # Retrieve their traits from the indexes 
      copy <- max.col(cbind(previous_population[demonstrators[,1],], 
                            previous_population[demonstrators[,2],]))
      # Save the demonstrators
      demonstrators <- demonstrators[cbind(1 : N, copy)]
      fitness <- tabulate(demonstrators, N) 
      
      population$P <- previous_population[demonstrators,]$P + runif(N, -mu, +mu)
      population$P[population$P > 1] <- 1
      population$P[population$P < 0] <- 0
      
      # Output
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$P) / N 
      output[output$generation == t & output$run == r, ]$cov_W_P <- 
        cov(fitness, previous_population$P) 
    }
  }
  # Export data from function
  output 
}

transformation <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)), 
                   cov_W_P = as.numeric(rep(NA, t_max * r_max)))
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- sum(population$P) / N 
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Choose demonstrators and calcualte  their fitness
      demonstrators <- sample(N, N, replace = TRUE)
      fitness <- tabulate(demonstrators, N) 
      
      population$P <- previous_population[demonstrators,]$P + 
        runif(N, max = 1 - previous_population[demonstrators,]$P)
      
      # Output
      output[output$generation == t & output$run == r, ]$p <- sum(population$P) / N 
      output[output$generation == t & output$run == r, ]$cov_W_P <- cov(fitness,previous_population$P)
    }
  }
  # Export data from function
  output 
}

data_model_reproduction <- reproduction(N = 1000, t_max = 20, r_max = 5, mu = 0.05)
data_model_transformation <- transformation(N = 1000, t_max = 20, r_max = 5)

data_to_plot <- tibble(covariance = c(na.omit(data_model_reproduction$cov_W_P), 
                                      na.omit(data_model_transformation$cov_W_P)), 
                       condition = rep(c("reproduction", "transformation"), each = 95),
                       generation = rep(2:20,10),
                       run = as.factor(rep(1:10, each = 19)))
ggplot(data = data_to_plot, aes(y = covariance, x = generation, 
                                group = run, color = condition)) +
  geom_line() +   
  geom_point() +
  theme_bw() +
  labs(y = "covariance between cultural fitness and P")

ggsave("reproduction_transformation continuous trait fitness.png", width = 20, height = 15, units = "cm")
