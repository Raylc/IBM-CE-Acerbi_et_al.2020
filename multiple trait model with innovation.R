library(tidyverse)

multiple_traits_matrix <- function(N, t_max, mu) {
  
  max_traits <- N + N * mu * t_max
  
  output <- matrix(data = NA, nrow = t_max, ncol = max_traits)
  
  # Create first generation
  population <- sample(1:N, N, replace = TRUE)
  output[1, ] <- tabulate(population, nbins = N) / N
  
  # Add first generation's p for all traits
  for (t in 2:t_max) {
    # Record what is the last trait introduced in the population
    last_trait <- max(population) 
    
    # Copy individuals to previous_population tibble
    previous_population <- population 
    
    # Randomly copy from previous generation
    population <- sample(previous_population, N, replace = TRUE)
    
    # Select the innovators
    innovators <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
    if ((last_trait + sum(innovators)) < max_traits) {
      # Replace innovators' traits with new traits
      population[innovators] <- (last_trait + 1):(last_trait + sum(innovators)) 
    }
    # Get p for all traits and put it into output slot for this generation t
    output[t, ] <- tabulate(population, nbins = max_traits) / N 
  }
  # Export data
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


data_model <- multiple_traits_matrix(N = 100, t_max = 200, mu = 0.01)
plot_multiple_traits_matrix(data_model)
ggsave("multiple trait model with innovation 1.png", width = 20, height = 15, units = "cm")

data_model <- multiple_traits_matrix(N = 100, t_max = 1000, mu = 0.01)
plot_multiple_traits_matrix(data_model)
ggsave("multiple trait model with innovation 2.png", width = 20, height = 15, units = "cm")

data_model <- multiple_traits_matrix(N = 10000, t_max = 100, mu = 0.01)
plot_multiple_traits_matrix(data_model)
ggsave("multiple trait model with innovation 3.png", width = 20, height = 15, units = "cm")

cumulative <- colSums(data_model) * 100
cumulative <- cumulative[cumulative > 0]
data_to_plot <- tibble(cumulative = sort(cumulative, decreasing = TRUE))

ggplot(data = data_to_plot, aes(x = seq_along(cumulative), y = cumulative)) +
  geom_point() +
  theme_bw() +
  labs(x = "trait sorted by decreasing popularity", y = "cumulative popularity")

ggsave("multiple trait model with innovation popularity 3.png", width = 20, height = 15, units = "cm")

