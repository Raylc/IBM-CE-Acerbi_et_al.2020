#Preparation
library(tidyverse)
#Set up population size, which is fixed across generations.
N <- 100
#Set up maximum generation
t_max <- 200
#Create a table storing the traits of the first-generation-individuals.
population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE))


#Check the population table
population
#Check the trait of individual of interest
population$trait[41]


#Create a table storing the trait frequency in each generations.
output <- tibble(generation = 1:t_max, p = rep(NA, t_max))
#Assign the relative frequency of trait A to the first generation using
#the sample function with equal probability.
output$p[1] <- sum(population$trait == "A") / N
#Check the output table
output