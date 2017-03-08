library(tidyverse); library(ggthemes); library(extrafont)

theme_set(theme_solarized_2(light = FALSE))

linearSims = read_csv("data/LinearModelSims.csv", 
                      col_names = c("time", "y", "eta", "gamma", "state"))

#####################
# Plot linear Sims #
#####################

linearSims %>%
  select(time, y, eta) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  theme(legend.position = "none", text = element_text(family = "Georgia"))
  
ggsave("Figures/LinearSims.png")

####################
# Linear Pilot Run #
####################

pilot_run = read_csv("data/LinearPilotRun.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line(colour = "#666666") +
  geom_point(colour = "#999999") +
  ggtitle("Variance of the estimate of the\npseudo-marginal log-likelihood") +
  theme(legend.position = "none", text = element_text(family = "Georgia"))

ggsave("Figures/LinearPilotRun.png")

###########################
# Linear Model Parameters #
###########################

params = c("v", "m0", "c0", "theta", "alpha", "sigma")

actual_values = data_frame(parameter = params, actual_value = c(1.0, 0.5, 0.12, 3.0, 0.2, 0.5))

chain1 = read_csv("data/LinearModelParams-1.csv", col_names = c(params, "accepted")) %>%
  mutate(chain = 1, iteration = seq_len(n()))

chain2 = read_csv("data/LinearModelParams-2.csv", col_names = c(params, "accepted")) %>%
  mutate(chain = 2, iteration = seq_len(n()))

chains = bind_rows(chain1[-(1:1000),], chain2[-(1:1000),])

source("scripts/PlotMCMC.R")

plot_params = c("v", "alpha", "theta", "sigma")
plot_running_mean(chains, parameters = plot_params, actual_params = actual_values)
traceplot(chains, parameters = plot_params, actual_params = actual_values)
plot_density(chains, plot_params, actual_params = actual_values)

############################
# One Step linear Forecast #
############################

# One step forecast for the linear model

linear_forecast = read_csv("data/LinearModelForecast.csv", 
                           col_names = c("time", "y_hat", "y_upper", "y_lower", 
                                         "eta_hat", "eta_upper", "eta_lower",
                                          "state_hat", "state_upper", "state_lower"))

linear_forecast %>%
  inner_join(linearSims, by = "time") %>%
  select(contains("y"), time) %>%
  gather(key, value, -time, -y_upper, -y_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.5, colour = NA, fill = "#1f5081")

#################
# Online Filter #
#################

online_filter = read_csv("data/LinearOnlineFilter.csv",
                                col_names = c("time", "observation",
                                              "eta_hat", "eta_lower", "eta_upper",
                                              "state_hat", "state_lower", "state_upper"))

online_filter %>%
  select(contains("state"), time, observation) %>%
  gather(key, value, -time, -state_upper, -state_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = "NA", fill = "#1f5081") +
  theme(legend.position = "bottom")

######################
# Long Term Forecast #
######################

linear_long_forecast = read_csv("data/LinearLongForecast.csv", 
                                col_names = c("time", "y_hat", "y_upper", "y_lower", 
                                              "eta_hat", "eta_upper", "eta_lower",
                                              "state_hat", "state_upper", "state_lower"))

linear_long_forecast %>%
  inner_join(linearSims, by = "time") %>%
  select(time, contains("y")) %>%
  gather(key, value, -time, -y_upper, -y_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.5, colour = "NA", fill = "#1f5081") +
  theme(legend.position = "bottom")