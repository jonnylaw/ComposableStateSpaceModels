library(tidyverse); library(gridExtra); library(ggthemes); library(extrafont)

theme_set(theme_solarized_2(light = F))

poissonSims = read_csv("data/PoissonModelSims.csv", 
                      col_names = c("time", "y", "eta", "gamma", "state"))

#####################
# Plot Poisson Sims #
#####################

poissonSims %>%
  select(time, y, eta, state) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y", strip.position = "right") +
  theme(legend.position = "none", text = element_text(family = "Georgia"))

ggsave("Figures/PoissonModel.png")

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

source("scripts/PlotMCMC.R")

plot_running_mean(chain1[-(1:1000),], parameters = params, actual_params = actual_values)

traceplot(chain1[-(1:1000),], parameters = params, actual_params = actual_values)

plot_density(chain1[-(1:1000), ], parameters = params, actual_params = actual_values)

####################
# Poisson Filtered #
####################

poissonFiltered = read_csv("data/PoissonModelFiltered.csv",
                           col_names = c("time", "y", "pred_eta", "eta_lower", "eta_upper", "pred_state", "state_upper", "state_lower"))

p1 = poissonFiltered %>%
  select(-y) %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("eta")) %>%
  gather(key, value, -time, -eta_upper, -eta_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = eta_lower, ymax = eta_upper), alpha = 0.5, colour = NA)

p2 = poissonFiltered %>%
  select(-y) %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("state")) %>%
  gather(key, value, -time, -state_upper, -state_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = NA)

png("FilteringPoisson.png")
grid.arrange(p1, p2, ncol = 1)
dev.off()

#####################
# One-step Forecast #
#####################

poisson_one_step = read_csv("data/PoissonOneStepForecast.csv", col_names = c("time", "pred_y", "y_lower", "y_upper",
                                                                             "pred_eta", "eta_lower", "eta_upper", 
                                                                             "pred_state", "state_upper", "state_lower"))

poisson_one_step %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("y")) %>%
  gather(key, value, -time, -y_lower, -y_upper) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.5, colour = NA, fill = "#1f5081") +
  theme(text = element_text(family = "Georgia"))


####################
# Poisson Forecast #
####################

poissonForecast = read_csv("data/PoissonLongForecast.csv", 
                           col_names = c("time", "pred_y", "y_lower", "y_upper",
                                         "pred_eta", "eta_lower", "eta_upper", 
                                         "pred_state", "state_upper", "state_lower"))

poissonForecast %>%
  inner_join(poissonSims, by = "time") %>%
  select(time, contains("y")) %>%
  gather(key, value, -time, -y_lower, -y_upper) %>%
  ggplot(aes(x = time, y = value, linetype = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.5)