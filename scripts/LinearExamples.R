library(tidyverse); library(ggthemes); library(extrafont); library(coda); library(ggmcmc); library(jsonlite)

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

params = c("v", "alpha", "sigma", "theta", "m0", "c0")

actual_values = data_frame(parameter = params, actual_value = c(1.0, 0.5, 0.12, 3.0, 0.2, 0.5))

read_chain = function(file, params) {
  chain = lapply(readLines(file), function(x) fromJSON(x)$params) %>% 
    unlist() %>%
    matrix(ncol = 6, byrow = T) %>%
    as_data_frame()
  
  colnames(chain) = params
  
  chain %>%
    mutate(v = exp(v), alpha = exp(alpha), sigma = exp(sigma))
}

chain = read_chain("data/LinearPosterior.json", params)

# ggmcmc(ggs(mcmc(chain)), file = "linear_params.pdf")

ggs_traceplot(ggs(mcmc(chain)))

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