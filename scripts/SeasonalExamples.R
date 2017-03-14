library(tidyverse); library(gridExtra); library(ggmcmc); library(coda)
source("scripts/PlotMCMC.R")

theme_set(theme_solarized_2(light = FALSE))

h = 1

seasonalSims = read_csv("data/SeasonalModelSims.csv", 
                       col_names = c("time", "observation", "eta", "gamma", 
                                     sapply(1:(h*2), function(i) paste("state", i, sep = "_"))),
                       n_max = 200)

#####################
# Plot seasonal Sims #
#####################

p1 = seasonalSims %>%
  select(time, eta, observation) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key, colour = key)) +
  geom_line() +
  theme(legend.position = "bottom")

p2 = seasonalSims %>%
  select(time, contains("state")) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  theme(legend.position = "none")
  # facet_wrap(~key, ncol = 1)

grid.arrange(p1, p2)

####################
# seasonal Filtered #
####################

seasonalFiltered = read_csv("data/SeasonalModelFiltered.csv", 
                           col_names = c("time", "observation", 
                                         "pred_eta", "lower_eta", "upper_eta",
                                         sapply(1:(h*2), function(i) paste("pred_state", i, sep = "_")),
                                         sapply(1:(h*2), function(i) c(paste("lower_state", i, sep = "_"), 
                                                                   paste("upper_state", i, sep = "_")))), skip = 1)

p1 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("eta")) %>%
  gather(key, value, -time, -upper_eta, -lower_eta) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_eta, ymax = upper_eta), alpha = 0.5, colour = NA, fill = "#1f5081")

p2 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("state_3")) %>%
  gather(key, value, -time, -contains("upper"), -contains("lower")) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_state_3, ymax = upper_state_3), alpha = 0.5, colour = NA, fill = "#1f5081") +
  theme(legend.position = "bottom")

p3 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("state_2")) %>%
  gather(key, value, -time, -contains("upper"), -contains("lower")) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_state_2, ymax = upper_state_2), alpha = 0.5, colour = NA, fill = "#1f5081") +
  theme(legend.position = "bottom")

# png("FilteringSeasonal.png")
grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
# dev.off()

#############
# Pilot Run #
#############

pilot_run = read_csv("data/SeasonalPilotRun.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line() + 
  geom_point()

###################
# Seasonal Params #
###################

params = c("v", sapply(1:(h*2), function(i) paste("m0", i, sep = "_")),
                sapply(1:(h*2), function(i) paste("c0", i, sep = "_")),
                sapply(1:(h*2), function(i) paste("mu", i, sep = "_")),
                sapply(1:(h*2), function(i) paste("sigma", i, sep = "_")))

actual_values = data_frame(parameter = params, 
                           actual_value = c(3.0, rep(0.5, (h*2)), rep(0.12, (h*2)),
                                            rep(0.1, (h*2)), rep(0.5, (h*2))))

read_chain = function(file, params) {
  chain = lapply(readLines(file), function(x) fromJSON(x)$params) %>% 
    unlist() %>%
    matrix(ncol = 9, byrow = T) %>%
    as_data_frame()
  
  colnames(chain) = params
  
  chain
}

chains = mcmc.list(mcmc(read_chain("data/SeasonalModelParams-1.json", params)), 
  mcmc(read_chain("data/SeasonalModelParams-2.json", params))) %>% ggs()

ggmcmc(chain)