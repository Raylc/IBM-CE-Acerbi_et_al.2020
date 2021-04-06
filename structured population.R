library(tidyverse)
structured_population_2 <- function(n, c, r_max){
  total_pop <- c * n
  cluster <- rep(1:c, each = n)
  behaviours <- sample(x = 2, size = total_pop, replace = TRUE)
  rec_behav <- matrix(NA, nrow = r_max, ncol = c)
  
  for(round in 1:r_max){
    behaviours[ sample(x = total_pop, size = 1) ] <- sample(x = behaviours, size = 1)
    # Recalculate p for each cluster
    for(clu in 1:c){
      rec_behav[round, clu] <- sum(behaviours[cluster == clu] == 1) / n
    }
  }
  rec_behav_tbl <- as_tibble(rec_behav)
  # Set column names to c1, c2 to represent each cluster
  colnames(rec_behav_tbl) <- paste("c", 1:c, sep = "")
  rec_behav_tbl$time <- 1:r_max
  rec_behav_tbl_l <- pivot_longer(data = rec_behav_tbl, names_to = "cluster", 
                                  values_to = "p", !time)
  return(rec_behav_tbl_l)
}

res <- structured_population_2(n = 50, c = 4, r_max = 1000)

ggplot(res) + 
  geom_line(aes(x = time, y = p, col = cluster)) + 
  scale_y_continuous(limits = c(0,1)) + 
  ylab("proportion of behaviour 1 in cluster") + 
  theme_bw()
ggsave("structured population1.png", width = 20, height = 15, units = "cm")



structured_population_3 <- function(n, c, r_max){
  total_pop <- c * n
  cluster <- rep(1:c, each = n)
  behaviours <- sample(x = 2, size = total_pop, replace = TRUE)
  rec_behav <- matrix(NA, nrow = r_max, ncol = c)
  
  for(round in 1:r_max){
    # Choose a random cluster
    cluster_id <- sample(c, 1)
    # If there are at least two individuals in this cluster
    if(sum(cluster == cluster_id)>1){
      # Choose a random observer and a random individual to observe within the same cluster 
      observer_model <- sample(x = total_pop, size = 2, replace = F, 
                               prob = (cluster == cluster_id)*1)
      behaviours[ observer_model[1] ] <- behaviours[ observer_model[2] ]
    }
    
    for(clu in 1:c){
      rec_behav[round, clu] <- sum(behaviours[cluster == clu] == 1) / n
    }
  }
  return(matrix_to_tibble(m = rec_behav))
}

matrix_to_tibble <- function(m){
  m_tbl <- as_tibble(m)
  colnames(m_tbl) <- paste("c" ,1:ncol(m), sep = "")
  m_tbl$time<- 1:nrow(m)
  m_tbl_l <- pivot_longer(data = m_tbl, names_to = "cluster", values_to = "p", !time)
  return(m_tbl_l)
}
res <- structured_population_3(n = 20, c = 3, r_max = 1000)
ggplot(res) + 
  geom_line(aes(x = time, y = p, col = cluster)) + 
  scale_y_continuous(limits = c(0,1)) + 
  ylab("proportion of behaviour 1 in cluster") + 
  theme_bw()
ggsave("structured population2.png", width = 20, height = 15, units = "cm")


structured_population_4 <- function(n, c, p_c, r_max){
  total_pop <- c * n
  cluster <- rep(1:c, each = n)
  behaviours <- sample(x = 2, size = total_pop, replace = TRUE)
  rec_behav <- matrix(NA, nrow = r_max, ncol = c)
  
  for(round in 1:r_max){
    cluster_id <- sample(c, 1)
    s <- cluster == cluster_id
    if(sum(s)>1){
      # Choose a random observer and a random individual to observe within the same cluster 
      observer_model <- sample(x = total_pop, size = 2, replace = F, 
                               prob = (s + p_c) / (1 + p_c))
      behaviours[ observer_model[1] ] <- behaviours[ observer_model[2] ]
    }
    
    for(clu in 1:c){
      rec_behav[round, clu] <- sum(behaviours[cluster == clu] == 1) / n
    }
  }
  return(matrix_to_tibble(m = rec_behav))
}
res_0  <- structured_population_4(n = 20, c = 5, p_c = 0, r_max = 1000)
res_01 <- structured_population_4(n = 20, c = 5, p_c = 0.1, r_max = 1000)
res_1  <- structured_population_4(n = 20, c = 5, p_c = 1, r_max = 1000)

ggplot(res_0) + 
  geom_line(aes(x = time, y = p, col = cluster)) + 
  scale_y_continuous(limits = c(0,1)) + 
  ylab("proportion of behaviour 1 in cluster") + 
  theme_bw()
ggsave("structured population3.png", width = 20, height = 15, units = "cm")

ggplot(res_01) + 
  geom_line(aes(x = time, y = p, col = cluster)) + 
  scale_y_continuous(limits = c(0,1)) + 
  ylab("proportion of behaviour 1 in cluster") + 
  theme_bw()
ggsave("structured population4.png", width = 20, height = 15, units = "cm")

ggplot(res_1) + 
  geom_line(aes(x = time, y = p, col = cluster)) + 
  scale_y_continuous(limits = c(0,1)) + 
  ylab("proportion of behaviour 1 in cluster") + 
  theme_bw()
ggsave("structured population5.png", width = 20, height = 15, units = "cm")



#migration
structured_population_5 <- function(n, c, p_c, p_m, r_max){
  total_pop <- c * n
  cluster <- rep(1:c, each = n)
  behaviours <- sample(x = 2, size = total_pop, replace = TRUE)
  rec_behav <- matrix(NA, nrow = r_max, ncol = c)
  
  for(round in 1:r_max){
    cluster_id <- sample(c, 1)
    s <- cluster == cluster_id
    if(sum(s)>1){
      observer_model <- sample(x = total_pop, size = 2, replace = F, 
                               prob = (s + p_c) / (1 + p_c))
      behaviours[ observer_model[1] ] <- behaviours[ observer_model[2] ]
    }
    
    # Migration to another cluster with probability p_m and if there is more than 
    # one subset
    if((runif(1,0,1) <= p_m) & (c > 1)){
      # Set cluster id that is different from the current one
      cluster[ observer_model[1] ] <- sample((1:c)[-cluster_id], 1)
    }
    
    for(clu in 1:c){
      rec_behav[round, clu] <- sum(behaviours[cluster == clu] == 1) / sum(cluster == clu)
    }
  }
  return(matrix_to_tibble(m = rec_behav))
}
res_0  <- structured_population_5(c = 5, n = 50, r_max = 10000, p_m = 0, p_c=0)
res_1  <- structured_population_5(c = 5, n = 50, r_max = 10000, p_m = 1, p_c=0)
res_01 <- structured_population_5(c = 5, n = 50, r_max = 10000, p_m = 0.1, p_c=0)

ggplot(res_0) + 
  geom_line(aes(x = time, y = p, col = cluster)) +
  scale_y_continuous(limits = c(0,1)) + 
  ylab("relative frequency of behaviour 1") + 
  theme_bw()

ggsave("structured population m1.png", width = 20, height = 15, units = "cm")

ggplot(res_1) + 
  geom_line(aes(x = time, y = p, col = cluster)) +
  scale_y_continuous(limits = c(0,1)) + 
  ylab("relative frequency of behaviour 1") + 
  theme_bw()

ggsave("structured population m2.png", width = 20, height = 15, units = "cm")

ggplot(res_01) + 
  geom_line(aes(x = time, y = p, col = cluster)) +
  scale_y_continuous(limits = c(0,1)) + 
  ylab("relative frequency of behaviour 1") + 
  theme_bw()
ggsave("structured population m3.png", width = 20, height = 15, units = "cm")



#migration with probability

structured_population_6 <- function(n, c, p_c, p_m, r_max, sim = 1){
  total_pop <- c * n
  cluster <- rep(1:c, each = n)
  behaviours <- sample(x = 2, size = total_pop, replace = TRUE)
  rec_behav <- rep(NA, times = c)
  # Adding a reporting variable for the similarity of clusters
  rec_var <- rep(NA, r_max)
  
  for(round in 1:r_max){
    cluster_id <- sample(c, 1)
    s <- cluster == cluster_id
    if(sum(s)>1){
      observer_model <- sample(x = total_pop, size = 2, replace = F, 
                               prob = (s + p_c) / (1 + p_c))
      behaviours[ observer_model[1] ] <- behaviours[ observer_model[2] ]
    }
    
    if((runif(1,0,1) <= p_m) & (c > 1)){
      cluster[ observer_model[1] ] <- sample((1:c)[-cluster_id], 1)
    }
    
    for(clu in 1:c){
      rec_behav[clu] <- sum(behaviours[cluster == clu] == 1) / sum(cluster == clu)
    }
    # Calculating variance in behaviour 1 between clusters
    rec_var[round] <- var(rec_behav)
  }
  # Preparing a reporting table to return
  rec_var <- bind_cols(time = 1:r_max, var = rec_var, sim = sim, p_c = p_c, p_m = p_m)
  return(rec_var)
}
grid <- expand.grid(rep = 1:20, 
                    pm = c(0, .01, .1, 1), 
                    pc = c(0, .01, .1, 1))
res <- bind_rows(lapply(1:nrow(grid), function(i) 
  structured_population_6(c = 5, 
                          n = 20, 
                          r_max = 2000, 
                          p_m = grid[i, "pm"], 
                          p_c = grid[i, "pc"], 
                          sim = i)
))
ggplot(res) +
  geom_line(aes(x = time, y = var, group = sim), alpha=.5) +
  facet_grid(p_m ~ p_c, labeller = label_both) + 
  ylab("average variance between subsets") +
  theme_bw()
ggsave("structured population m4.png", width = 20, height = 15, units = "cm")
