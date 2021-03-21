library(tidyverse)
my_world <- matrix(c(1,1,-1,-1,1,1,-1,-1,-1,-1,1,1,-1,-1,1,1), nrow = 4, ncol = 4)
my_world

traits_inter_dependence <- function(N, t_max, k, mu, p_death, world){
  output <- tibble(trait = rep(c("A","B","C","D"), each = t_max), 
                   generation = rep(1:t_max, 4), 
                   p = as.numeric(rep(NA, t_max * 4)))    
  
  population <- matrix(0, ncol = 4, nrow = N)
  
  output[output$generation == 1 ,]$p <- colSums(population) / N
  
  for(t in 2:t_max){
    
    # Innovations
    innovators <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
    innovations <- sample(1:4, sum(innovators), replace = TRUE)
    population[cbind(which(innovators == TRUE), innovations)] <- 1
    
    # Copying
    demonstrators <- sample(1:N, replace = TRUE)
    demonstrators_traits <- sample(1:4, N, replace = TRUE)
    
    for(i in 1:N){
      if(population[demonstrators[i], demonstrators_traits[i]]){
        compatibility_score <- sum(world[demonstrators_traits[i], population[i, ] != 0])
        copy <- (1 / (1 + exp(-k*compatibility_score))) > runif(1)
        population[i,demonstrators_traits[i]] <- 1 * copy
      }
    }
    
    # Birth/death
    replace <- sample(c(TRUE, FALSE), N, prob = c(p_death, 1 - p_death), replace = TRUE)
    population[replace, ] <- 0
    
    # Output
    output[output$generation == t ,]$p <- colSums(population) / N
  }
  # Export data from function
  output
}

plot_multiple_traits <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(colour = trait)) +
    ylim(c(0, 1)) +
    theme_bw() +
    theme(legend.position = "none")
}


my_world <- matrix(c(1,1,-1,-1,1,1,-1,-1,-1,-1,1,1,-1,-1,1,1), nrow = 4, ncol = 4)
data_model <- traits_inter_dependence(N = 100, t_max = 1000, k = 10, 
                                      mu = 0.0005, p_death = 0.01, world = my_world)
plot_multiple_traits(data_model)
ggsave("Trait inter-dependence 1.png", width = 20, height = 15, units = "cm")



my_world <- matrix(c(1,1,1,-1, 1,1,1,-1,1,1,1,-1,-1,-1,-1,1), nrow = 4, ncol = 4)
data_model <- traits_inter_dependence(N = 100, t_max = 1000, k = 10, 
                                      mu = 0.0005, p_death = 0.01, world = my_world)
plot_multiple_traits(data_model)
ggsave("Trait inter-dependence 2.png", width = 20, height = 15, units = "cm")



my_world <- matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), nrow = 4, ncol = 4)
data_model <- traits_inter_dependence(N = 100, t_max = 1000, k = 10, 
                                      mu = 0.0005, p_death = 0.01, world = my_world)
plot_multiple_traits(data_model)
ggsave("Trait inter-dependence 3.png", width = 20, height = 15, units = "cm")




traits_inter_dependence_2 <- function(N, M, t_max, k, mu, p_death, gamma){
  output <- matrix(data = NA, nrow = t_max, ncol = M)
  
  # Initialise the traits' world
  world <- matrix( rep(1, M * M), nrow = M)
  compatibilities <- sample(c(1, -1), choose(M,2), prob = c(gamma, 1 - gamma), replace = TRUE) 
  world[upper.tri(world)] <- compatibilities
  world <- t(world)
  world[upper.tri(world)] <- compatibilities
  
  # Initialise the population
  population <- matrix(0, ncol = M, nrow = N)
  output[1, ] <- colSums(population) / N 
  
  for(t in 2:t_max){
    # Innovations
    innovators <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
    innovations <- sample(1:M, sum(innovators), replace = TRUE)
    population[cbind(which(innovators == TRUE), innovations)] <- 1
    
    # Copying
    demonstrators <- sample(1:N, replace = TRUE)
    demonstrators_traits <- sample(1:M, N, replace = TRUE)
    
    for(i in 1:N){
      if(population[demonstrators[i], demonstrators_traits[i]]){
        compatibility_score <- sum(world[demonstrators_traits[i], which(population[i,]>0)])
        copy <- (1 / (1 + exp(-k*compatibility_score))) > runif(1)
        if(copy){
          population[i,demonstrators_traits[i]] <- 1
        }
      }
    }
    
    # Birth/death
    replace <- sample(c(TRUE, FALSE), N, prob = c(p_death, 1 - p_death), replace = TRUE)
    population[replace, ] <- 0
    
    # Write output
    output[t, ] <- colSums(population) / N
  }
  # Export data from function
  output
}  

plot_multiple_traits_matrix <- function(data_model) {
  generation <- rep(1:dim(data_model)[1], each = dim(data_model)[2])
  
  data_to_plot <- as_tibble(data_model) %>%
    pivot_longer(everything(), names_to = "trait", values_to = "p") %>%
    add_column(generation)
  
  ggplot(data = data_to_plot, aes(y = p, x = generation)) +
    geom_line(aes(colour = trait)) +
    ylim(c(0, 1)) +
    theme_bw() +
    theme(legend.position = "none")
}

data_model <- traits_inter_dependence_2(N = 100, M = 20, t_max = 2000, k = 10,
                                        mu = 0.001, p_death = 0.01, gamma = .5)
plot_multiple_traits_matrix(data_model)
ggsave("Trait inter-dependence 4.png", width = 20, height = 15, units = "cm")


data_model <- traits_inter_dependence_2(N = 100, M = 20, t_max = 2000, k = 10, 
                                        mu = 0.001, p_death = 0.01, gamma = 1)
plot_multiple_traits_matrix(data_model)
ggsave("Trait inter-dependence 5.png", width = 20, height = 15, units = "cm")



r_max = 10
test_inter_dependence <- tibble(gamma = as.factor(rep(seq(0, 1, by = .1), r_max)), 
                                run = as.factor(rep(1:r_max, each = 11)), 
                                C = as.numeric(NA))
for(condition in seq(0, 1, by = .1)){
  for(r in 1:r_max) {
    data_model <- traits_inter_dependence_2(N = 100, M = 20, t_max = 2000, k = 10, 
                                            mu = 0.001, p_death = 0.01, gamma = condition)
    test_inter_dependence[test_inter_dependence$gamma == condition & 
                            test_inter_dependence$run == r, ]$C <- 
      sum(data_model[2000,]>.5)
  }
}

ggplot(data = test_inter_dependence, aes(x = gamma, y = C)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5) +
  theme_bw() +
  labs(x = "Average compatibility", y = "C (number of common traits)")
ggsave("Trait inter-dependence 6.png", width = 20, height = 15, units = "cm")
