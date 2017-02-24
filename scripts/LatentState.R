library(tidyverse); library(gridExtra)

theme_set(theme_minimal())

########################
# Plot Brownian Motion #
########################

brownianMotion = read_csv("data/brownianMotion.csv", 
                          col_names = c("time", 
                                        sapply(1:4, function(i) paste("state", i, sep = "_"))), 
                          n_max = 200)

brownianMotion %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line()

###########################
# Plot ornstein uhlenbeck #
###########################

ou_process = read_csv("data/ornsteinUhlenbeck.csv", 
                          col_names = c("time", 
                                        sapply(1:2, function(i) paste("state", i, sep = "_"))), 
                          n_max = 200)

ou_process %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line()

