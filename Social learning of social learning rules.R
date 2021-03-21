library(tidyverse)
N <- 1000
population <- tibble(P = runif(N))

openness_conservatism <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(P = runif(N))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$P) / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Choose demonstrators at random
      demonstrators <- tibble(P = sample(previous_population$P, N, replace = TRUE)) 
      
      # Choose individuals that copy, according to their P
      copy <- previous_population$P > runif(N) 
      
      # Copy
      population[copy, ]$P <- demonstrators[copy, ]$P 
      
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



data_model <- openness_conservatism(N = 1000, t_max = 50, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("Openness and conservatism 1.png", width = 20, height = 15, units = "cm")





##Maintaining open populations
openness_conservatism_2 <- function(N, M, mu, p_death, t_max, r_max){
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   m = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    
    # Initialise population
    population_preferences <- matrix( runif(M * N), ncol = M, nrow = N)
    population_traits <- matrix(0, ncol = M, nrow = N)
    
    # Write first output
    output[output$generation == 1 & output$run == r, ]$p <- mean(population_preferences)
    output[output$generation == 1 & output$run == r, ]$m <- sum(population_traits) / N  
    
    for(t in 2:t_max){
      # Innovations
      innovators <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
      innovations <- sample(1:M, sum(innovators), replace = TRUE)
      population_traits[cbind(which(innovators == TRUE), innovations)] <- 1
      
      # Copying
      previous_population_preferences <- population_preferences
      previous_population_traits <- population_traits
      
      demonstrators <- sample(1:N, replace = TRUE)
      demonstrators_traits <- sample(1:M, N, replace = TRUE)
      
      copy <- previous_population_traits[cbind(demonstrators,demonstrators_traits)] == 1 & 
        previous_population_preferences[cbind(1:N, demonstrators_traits)] > runif(N)
      
      population_traits[cbind(which(copy), demonstrators_traits[copy])] <- 1
      
      population_preferences[cbind(which(copy), demonstrators_traits[copy])] <- 
        previous_population_preferences[cbind(demonstrators[copy], demonstrators_traits[copy])] 
      
      # Birth/death
      replace <- sample(c(TRUE, FALSE), N, prob = c(p_death, 1 - p_death), replace = TRUE)
      population_traits[replace, ] <- 0
      population_preferences[replace, ] <- runif(M * sum(replace))
      
      # Write output
      output[output$generation == t & output$run == r, ]$p <- mean(population_preferences)
      output[output$generation == t & output$run == r, ]$m <- sum(population_traits) / N    
    }
  }
  # Export data from function
  output
}

data_model <- openness_conservatism_2(N = 1000, M = 1, mu = 0.1, 
                                      p_death = 0.01, t_max = 50, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("Openness and conservatism 2.png", width = 20, height = 15, units = "cm")



data_model <- openness_conservatism_2(N = 1000, M = 10, mu = 0.1, 
                                      p_death = 0.01, t_max = 50, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("Openness and conservatism 3.png", width = 20, height = 15, units = "cm")



data_model <- openness_conservatism_2(N = 1000, M = 10, mu = 0.1, 
                                      p_death = 0.01, t_max = 1000, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("Openness and conservatism 4.png", width = 20, height = 15, units = "cm")


plot_multiple_runs_m <- function(data_model, M) {
  ggplot(data = data_model, aes(y = m, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, M)) +
    theme_bw() +
    labs(y = "m (average number of traits)")
}
plot_multiple_runs_m(data_model, M = 10)
ggsave("Openness and conservatism 5.png", width = 20, height = 15, units = "cm")


data_model <- openness_conservatism_2(N = 1000, M = 50, mu = 0.1, p_death = 0.01, t_max = 1000, r_max = 5)
plot_multiple_runs_p(data_model)
ggsave("Openness and conservatism 6.png", width = 20, height = 15, units = "cm")



plot_multiple_runs_m(data_model, M = 50)
ggsave("Openness and conservatism 7.png", width = 20, height = 15, units = "cm")




test_openness <- tibble(M = c(1,5,10,20,50,100), p = as.numeric(rep(NA, 6)))
for(condition in test_openness$M){
  data_model <- openness_conservatism_2(N = 1000, M = condition, mu = 0.1, 
                                        p_death = 0.01, t_max = 1000, r_max = 1)
  test_openness[test_openness$M == condition, ]$p <- 
    data_model[data_model$generation == 1000, ]$p
}
ggplot(data = test_openness, aes(x = M, y = p)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  theme_bw() +
  labs(x = "Maximum possible number of traits", y = "p (final average value of p)")
ggsave("Openness and conservatism 8.png", width = 20, height = 15, units = "cm")


test_openness <- tibble(M = c(0.5,0.1,0.05,0.01,0.005,0.001), p = as.numeric(rep(NA, 6)))
for(condition in test_openness$M){
  data_model <- openness_conservatism_2(N = 1000, M = 10, mu = 0.1, 
                                        p_death = condition, t_max = 1000, r_max = 1)
  test_openness[test_openness$M == condition, ]$p <- 
    data_model[data_model$generation == 1000, ]$p
}
ggplot(data = test_openness, aes(x = M, y = p)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  theme_bw() +
  labs(x = "Birth rate/Death rate", y = "p (final average value of p)")
ggsave("Openness and conservatism 9.png", width = 20, height = 15, units = "cm")
