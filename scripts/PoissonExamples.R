library(tidyverse); library(gridExtra)

theme_set(theme_minimal())

poissonSims = read_csv("data/PoissonModelSims.csv", 
                      col_names = c("time", "observation", "eta", "gamma", "state"))

#####################
# Plot Poisson Sims #
#####################

poissonSims %>%
  select(time, observation, eta, state) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  theme(legend.position = "none")

#####################
# Poisson Pilot Run #
#####################

pilot_run = read_csv("data/PoissonPilotRun.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line() + 
  geom_point()


################################
# Determine Poisson Parameters #
################################

params = c("m0", "c0", "mu", "sigma")

actual_values = data_frame(parameter = params, 
                           actual_value = c(0.5, 0.12, 0.1, 0.5))

chain1 = read_csv("data/PoissonParams.csv", col_names = c(params, "accepted")) %>%
  mutate(chain = 1, iteration = seq_len(n()))

plot_running_mean(chain1)

traceplot(chain1, parameters = params)

plot_density(chain1, parameters = params)

####################
# Poisson Filtered #
####################

poissonFiltered = read_csv("data/PoissonModelFiltered.csv",
                           col_names = c("time", "observation", "pred_eta", "eta_lower", "eta_upper", "pred_state", "state_upper", "state_lower"))

p1 = poissonFiltered %>%
  select(-observation) %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("eta")) %>%
  gather(key, value, -time, -eta_upper, -eta_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = eta_lower, ymax = eta_upper), alpha = 0.5, colour = NA)

p2 = poissonFiltered %>%
  select(-observation) %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("state")) %>%
  gather(key, value, -time, -state_upper, -state_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = NA)

png("FilteringPoisson.png")
grid.arrange(p1, p2, ncol = 1)
dev.off()

####################
# Poisson Forecast #
####################

poissonForecast = read_csv("data/PoissonLongForecast.csv", 
                           col_names = c("time", "pred_observation", "observation_lower", "observation_upper",
                                         "pred_eta", "eta_lower", "eta_upper", 
                                         "pred_state", "state_upper", "state_lower"))

poissonForecast %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("observation")) %>%
  gather(key, value, -time, -observation_lower, -observation_upper) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = observation_lower, ymax = observation_upper), alpha = 0.5)
