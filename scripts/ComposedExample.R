library(tidyverse); library(gridExtra)

theme_set(theme_minimal())

seasonalSims = read_csv("../data/seasonalPoissonSims.csv", 
                        col_names = c("time", "observation", "eta", "gamma", 
                                      sapply(1:7, function(i) paste("state", i, sep = "_"))))

#####################
# Plot seasonal Sims #
#####################

p1 = seasonalSims %>%
  select(time, eta, observation) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line() +
  theme(legend.position = "bottom")

p2 = seasonalSims %>%
  select(time, contains("state")) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line()
# facet_wrap(~key, ncol = 1)

grid.arrange(p1, p2)

####################
# seasonal Filtered #
####################

seasonalFiltered = read_csv("data/SeasonalPoissonFiltered.csv", 
                            col_names = c("time", "observation", 
                                          "pred_eta", "lower_eta", "upper_eta",
                                          sapply(1:7, function(i) paste("pred_state", i, sep = "_")),
                                          sapply(1:7, function(i) c(paste("lower_state", i, sep = "_"), 
                                                                    paste("upper_state", i, sep = "_")))), skip = 1)

p1 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("eta")) %>%
  gather(key, value, -time, -upper_eta, -lower_eta) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_eta, ymax = upper_eta), alpha = 0.5, colour = NA)

p2 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("state_1")) %>%
  gather(key, value, -time, -contains("upper"), -contains("lower")) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_state_1, ymax = upper_state_1), alpha = 0.5, colour = NA) +
  theme(legend.position = "bottom")

p3 = seasonalFiltered %>%
  select(-observation) %>%
  inner_join(seasonalSims, by = "time") %>%
  select(time, contains("state_2")) %>%
  gather(key, value, -time, -contains("upper"), -contains("lower")) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_state_2, ymax = upper_state_2), alpha = 0.5, colour = NA) +
  theme(legend.position = "bottom")

# png("FilteringSeasonal.png")
grid.arrange(p1, p2, p3, layout_matrix = rbind(c(1, 1), c(2, 3)))
# dev.off()

################################
# Pilot run for composed Model #
################################

pilot_run = read_csv("../data/ComposedPilotRun.csv")

pilot_run %>%
  ggplot(aes(x = particles, y = mll_variance)) +
  geom_point() +
  geom_line()

#######################################
# Sample from the prior distributions #
#######################################
params = c("m0", "c0", "mu", "sigma", 
           sapply(1:6, function(i) paste("m0", i, sep = "_")),
           sapply(1:6, function(i) paste("c0", i, sep = "_")),
           sapply(1:6, function(i) paste("theta", i, sep = "_")),
           sapply(1:6, function(i) paste("alpha", i, sep = "_")),
           sapply(1:6, function(i) paste("sigma", i, sep = "_")))

# Actual parameter values
actual_params = data_frame(parameters = params,
                           value = c(1.0, 1.0, 0.01, 0.01, rep(1.0, 6), rep(1.0, 6), rep(1.0, 6), rep(0.5, 6), rep(0.3, 6))



#################################
# Determine composed parameters #
#################################

# Using random walk metropolis hastings
chain1 = read_csv("data/seasonalPoissonParams-1.csv", col_names = c(params, "accepted")) %>% 
  mutate(chain = 1, iteration = seq_len(n()))
chain2 = read_csv("data/seasonalPoissonParams-2.csv", col_names = c(params, "accepted")) %>% 
  mutate(chain = 2, iteration = seq_len(n()))

n = max(nrow(chain1), nrow(chain2))

bind_rows(chain1[1:n,], chain2[1:n,]) %>%
  plot_running_mean()

############
# Forecast #
############

forecast = read_csv("../data/seasonalPoissonForecast.csv", 
                    col_names = c("time", "observation", "observation_lower", "observation_upper",
                                  "eta", "eta_lower", "eta_upper",
                                  sapply(1:7, function(i) paste("mean_state", i, sep = "_")),
                                  sapply(1:7, function(i) c(paste("state_lower", i, sep = "_"), 
                                                            paste("state_upper", i, sep = "_")))))
seasonalSims %>%
  select(time, observation) %>%
  mutate(observation_upper = NA, observation_lower = NA) %>%
  bind_rows(forecast %>% select(time, contains("observation"))) %>%
  ggplot(aes(x = time, y = observation)) +
  geom_line() + 
  geom_ribbon(aes(x = time, ymin = observation_lower, ymax = observation_upper), alpha = 0.5)