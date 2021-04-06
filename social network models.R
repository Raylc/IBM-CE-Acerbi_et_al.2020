library(tidyverse)
gossip_model <- function(N, f, r_max, sim = 1){
  # Create a vector indicating possession of gossip and set one entry to TRUE
  gossip <- rep(FALSE, N)
  gossip[sample(x = N, size = 1)] <- TRUE
  # Create a network
  adjm <- create_network(N = N, f = f)
  # Create a reporting variable
  proportion <- rep(0, r_max)
  
  # Loop over r_max rounds
  for(r in 1:r_max){
    # In random sequence go through all individuals 
    for(i in sample(N)){
      # Select i's neighbourhood
      nei <- adjm[i,] > 0
      # Proceed if there is at least one neighbour
      if(sum(nei) > 0){
        # Choose one random neighbour, j
        j <- sampling(x = which(nei), size = 1)
        # Set i's gossip indicator to TRUE if j's indicator is TRUE
        if(gossip[j]){
          gossip[i] <- TRUE
        }
      }
    }
    # Record proportion of the population with gossip
    proportion[r] <- sum(gossip) / N
    # Increment the round counter
    r <- r + 1
  }
  # Return a tibble with simulation results
  return(tibble(time = 1:r_max, proportion = proportion, f = f, sim = sim))
}

create_network <- function(N, f){
  # Set up an empty adjacency matrix of size NxN
  A <- matrix(0, ncol = N, nrow = N)
  # Set up a friendship counter
  friends <- 0
  
  # We will add friendships until we reach the desired number
  while(friends < round((((N^2) - N) / 2) * f)){
    dyad <- sample(x = N, size = 2, replace = FALSE)
    i <- dyad[1]
    j <- dyad[2]
    if(A[i, j] == 0){
      A[i, j] <- A[j, i] <- 1
      friends <- friends + 1
    }
  }
  return(A)
}

sampling <- function(x, size = length(x), prob = NULL){
  if(length(x) == 1){
    return(x)
  } else {
    return(sample(x = x, size = size, prob = prob))
  }
}

N <- 1000
set.seed(3)
data <- lapply(X = c(1/N, 2/N, 3/N, 5/N, 8/N, 10/N), 
               FUN = function(f) gossip_model(N = N, f = f, r_max = 50))
data_bnd <- bind_rows(data)

ggplot(data_bnd) + 
  geom_line(aes(x = time, y = proportion, col = factor(round(f * N)))) + 
  ylab("proportion of individuals with gossip") +
  labs(col = "average number of friends") +
  theme_bw()
ggsave("network1.png", width = 20, height = 15, units = "cm")


adjm <- create_network(N = 10, f = .5)
library(igraph)
net <- graph_from_adjacency_matrix(adjm)
plot(net)
degree(graph = net)
ggsave("network0.png", width = 20, height = 15, units = "cm")


N <- 100
f <- rep(c(1/N, 2/N, 3/N, 5/N, 8/N, 10/N), each = 10)

set.seed(5)
data <- lapply(X = 1:length(f), 
               FUN = function(i) gossip_model(N = N, f = f[i], r_max = 50, sim = i))

data_bnd <- bind_rows(data)
ggplot(data_bnd) +
  geom_line(aes(x = time, y = proportion, col = factor(f * N), group = factor(sim))) + 
  ylab("proportion of individual with gossip") +
  labs(col = "average number of friends") +
  theme_bw()
ggsave("network2.png", width = 20, height = 15, units = "cm")

data_bnd_last <- data_bnd[data_bnd$time == max(data_bnd$time), ]
ggplot(data_bnd_last) + 
  geom_boxplot(aes(x = factor(f * N), y = proportion)) + 
  xlab("average number of friends") +
  ylab("proportion of individuals with gossip") + 
  theme_bw()
ggsave("network3.png", width = 20, height = 15, units = "cm")



data_bnd_time <- lapply(data, function(dat){
  tibble(dat[1,], time_to_x = which(dat[,"proportion"] >= 0.75)[1])
}) %>% bind_rows()

data_bnd_time$time_to_x[is.na(data_bnd_time$time_to_x)] <- 50

ggplot(data_bnd_time) + 
  geom_boxplot(aes(x = factor(f * N), y = time_to_x)) + 
  xlab("average number of friends") +
  ylab("time to spread to 75% of population") +
  theme_bw()
ggsave("network4.png", width = 20, height = 15, units = "cm")




j <- sampling(x = which(nei), size = 1)
if(gossip[j]){
  gossip[i] <- TRUE
}

p_g <- sum(gossip * nei) / length(nei)
if(runif(n = 1, min = 0, max = 1) <= p_g){
  gossip[i] <- TRUE
}

if(runif(n = 1, min = 0, max = 1) <= (sum(nei) / length(nei))^e){
  gossip[i] <- TRUE
}
prop_nei_info <- seq(from = 0, to = 1, length.out = 10)
e <- c(1,2)
contagion <- tibble(
  contagion = rep(c("simple", "complex"), each = 10),
  x = rep(prop_nei_info, 2), 
  y = c(prop_nei_info^e[1], prop_nei_info^e[2]))

ggplot(contagion, aes(x = x, y = y, col = contagion)) + 
  geom_point() +
  geom_line() +
  xlab("proportion of neighbours with gossip") + 
  ylab("probability to acquire gossip") +
  theme_bw()
ggsave("network5.png", width = 20, height = 15, units = "cm")

net_clust <- make_lattice(length = 10, dim = 2, circular = T, nei = 2)
net_rand <- rewire(graph = net_clust, with = keeping_degseq(loops = F, niter = 10^3))

gossip_model_2 <- function(net, rewire, e = 1, r_max, sim = 1){
  # Rewire network if random is set to TRUE
  if(rewire){
    net <- rewire(graph = net, with = keeping_degseq(loops = F, niter = 10^3))
  }
  # Get adjacency matrix from network
  adjm <- get.adjacency(net, sparse = F)
  # Turn adjacency matrix into boolean (TRUE / FALSE)
  adjm_bool <- adjm > 0
  # Set number of individuals based adjacency matrix
  N <- vcount(net)
  
  # Create a vector indicating possession of gossip and set one entry to TRUE
  gossip <- rep(FALSE, N)
  gossip[sample(x = N, size = 1)] <- TRUE
  
  # Create a reporting variable
  proportion <- rep(0, r_max)
  
  # Rounds
  for(r in 1:r_max){
    # In random sequence go through all individuals without gossip
    for(i in sample(N)){
      # Select i's neighbourhood (boolean)
      nei <- adjm_bool[i,]
      # Proceed if there is at least one neighbour
      if(sum(nei) > 0){
        # Simple contagion for e = 1 and complex contagion for e = 2
        if(runif(n = 1, min = 0, max = 1) <= (sum(gossip * nei) / length(nei))^e){
          gossip[i] <- TRUE
        }
      }
    }
    # Record proportion of the population with gossip
    proportion[r] <- sum(gossip) / N
    # Increment the round counter
    r <- r + 1
  }
  # Return a tibble with simulation results
  return(tibble(time = 1:r_max, 
                proportion = proportion, 
                time_to_max = which(proportion == max(proportion))[1],
                e = e,
                network = ifelse(test = rewire, yes = "random", no = "clustered"), 
                sim = sim))
}
set.seed(1)
res <- bind_rows(
  gossip_model_2(net = net_clust, rewire = TRUE, e = 1, r_max = 500),
  gossip_model_2(net = net_clust, rewire = FALSE, e = 1, r_max = 500),
  gossip_model_2(net = net_clust, rewire = TRUE, e = 2, r_max = 5000),
  gossip_model_2(net = net_clust, rewire = FALSE, e = 2, r_max = 5000)
)

ggplot(res) + 
  geom_line(aes(x = time, y = proportion, col = network)) + 
  facet_wrap("e", labeller = label_both, scales = "free_x") +
  theme_bw()
ggsave("network6.png", width = 20, height = 15, units = "cm")
