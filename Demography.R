library(tidyverse)
library(extraDistr)

demography_model <- function(T_MAX, N, ALPHA, SIGMA, R_MAX){
  res <- lapply(1:R_MAX, function(repetition){
    z <- rep(1, N)
    z_delta_bar <- rep(NA, T_MAX)
    for(turn in 1:T_MAX){
      z_new <- rgumbel(n = N, mu = max(z) - ALPHA, sigma = SIGMA)
      z_delta_bar[turn] <- mean(z_new - z)
      z <- z_new
    }
    return(mean(z_delta_bar))
  })
  mean(unlist(res))
}

sizes <- c(2, seq(from = 100, to = 6100, by = 500))

simple_skill <- lapply(X = sizes, FUN = demography_model, 
                       T_MAX = 200, ALPHA = 7, SIGMA = 1, R_MAX = 20)

complex_skill <- lapply(X=sizes, FUN=demography_model, 
                        T_MAX = 200, ALPHA = 9, SIGMA = 1, R_MAX = 20)

data <- tibble(N = rep(sizes, 2), 
               z_delta_bar = c(unlist(simple_skill), 
                               unlist(complex_skill)), 
               skill = rep(c("simple","complex"), each = length(sizes)))

ggplot(data) + 
  geom_line(aes(x = N, y = z_delta_bar, color = skill)) +
  xlab("effective population size") + 
  ylab("change in average skill level, delta z bar") + 
  geom_hline(yintercept = 0) + 
  theme_bw()
ggsave("demography1.png", width = 20, height = 15, units = "cm")



# Calculating critical population sizes based on skill complexity

ggplot(data) + 
  geom_line(aes(x = log(N), y = z_delta_bar, color=skill)) +
  xlab("log(effective population size)") +
  ylab("change in average skill level, delta z bar") + 
  geom_hline(yintercept = 0) + 
  theme_bw()
ggsave("demography2.png", width = 20, height = 15, units = "cm")

# Create linear regression for the change in average skill level in response to population size
fit <- lm(formula = z_delta_bar ~ log(N), 
          data = data[data$skill == "simple",])
fit

# Solve for y = 0 by using the coefficients of the linear regression:
b <- fit$coefficients[1]
m <- fit$coefficients[2]
N_star_simple <- exp(-(b / m))

# And the same calculation for the complex skill
fit <- lm(formula = z_delta_bar ~ log(N), 
          data = data[data$skill == "complex",])
N_star_complex <- exp(-(fit$coefficients[1] / fit$coefficients[2]))

# Run simulation for the following population sizes
sizes <- seq(from = 100, to = 6100, by = 500)

# Run simulation for the following values of alpha
alphas <- seq(from = 4, to = 9, by = .5)

simulations <- expand.grid(N = sizes, alpha = alphas)
head(simulations)

z_delta_bar <- lapply(X = 1:nrow(simulations), FUN = function(s){
  demography_model(T_MAX = 200, 
                   N = simulations[s, "N"], 
                   ALPHA = simulations[s, "alpha"], 
                   SIGMA = 1, 
                   R_MAX = 5)
})
# Add results to population size and skill complexity
data <- cbind(simulations, z_delta_bar=unlist(z_delta_bar))

n_stars <- lapply(X = unique(data$alpha), FUN = function(alpha){
  # Only use the results with identical value for alpha
  subset <- data[data$alpha == alpha,]
  # Fit regression
  fit <- lm(formula = z_delta_bar ~ log(N), data = subset)
  # Solve for n star
  n_star <- exp(solve(coef(fit)[-1], -coef(fit)[1]))
  return(n_star)
})
# Combine all results in a single tibble
results <- tibble(n_star = unlist(n_stars), alpha = unique(data$alpha))

ggplot(results, aes(x = alpha, y = n_star)) + 
  geom_line() + 
  xlab(expression(alpha/sigma)) +
  ylab("critical populaton size, N*") + 
  theme_bw()
ggsave("demography3.png", width = 20, height = 15, units = "cm")
