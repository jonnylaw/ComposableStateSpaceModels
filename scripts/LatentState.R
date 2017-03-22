library(tidyverse); library(gridExtra); library(extrafont)

theme_set(theme_solarized_2(light = FALSE))

########################
# Plot Brownian Motion #
########################

brownianMotion = read_csv("data/brownianMotion.csv", 
                          col_names = c("time", 
                                        sapply(1:4, function(i) paste("state", i, sep = "_"))), 
                          n_max = 200)

brownianMotion %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key, colour = key)) +
  geom_line()

###########################
# Plot ornstein uhlenbeck #
###########################

ou_process = read_csv("data/OrnsteinUhlenbeck.csv", 
                          col_names = c("time", 
                                        sapply(1:2, function(i) paste("state", i, sep = "_"))))

ou_process %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key, colour = key)) +
  geom_line() +
  theme(legend.position = "bottom", text = element_text(family = "Georgia"))

ggsave("Figures/ouProcess.png")
