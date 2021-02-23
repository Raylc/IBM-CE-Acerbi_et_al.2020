multiple_traits <- function(N, t_max) {
  
  output <- tibble(trait = as.factor(rep(1:N, each = t_max)), 
                   generation = rep(1:t_max, N), 
                   p = as.numeric(rep(NA, t_max * N)))
  
  # Create first generation
  population <- tibble(trait = sample(1:N, N, replace = TRUE))  
  
  # Add first generation's p for all traits
  output[output$generation == 1, ]$p <- tabulate(population$trait, nbins = N) / N  
  
  for (t in 2:t_max) {
    # Copy individuals to previous_population tibble
    previous_population <- population 
    
    # Randomly copy from previous generation
    population <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))
    
    # Get p for all traits and put it into output slot for this generation t
    output[output$generation == t, ]$p <- tabulate(population$trait, nbins = N) / N  
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


data_model <- multiple_traits(N = 100, t_max = 200)
plot_multiple_traits(data_model)
ggsave("multiple trait model1.png", width = 20, height = 15, units = "cm")

data_model <- multiple_traits(N = 100, t_max = 1000)
plot_multiple_traits(data_model)
ggsave("multiple trait model2.png", width = 20, height = 15, units = "cm")
