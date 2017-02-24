library(tidyverse)

theme_set(theme_minimal())

linearSims = read_csv("data/LinearModelSims.csv", 
                      col_names = c("time", "observation", "eta", "gamma", "state"),
                      n_max = 200)

#####################
# Plot linear Sims #
#####################

linearSims %>%
  select(time, observation, eta) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y")

ggsave("Figures/LinearSims.png")

####################
# linear Filtered #
####################

linearFiltered = read_csv("data/LinearModelFiltered.csv", 
                           col_names = c("time", "observation", "pred_eta", "eta_lower", "eta_upper", "pred_state", "state_upper", "state_lower"))

linearFiltered %>%
  select(-observation) %>%
  inner_join(linearSims, by = "time") %>%
  select(time, contains("state")) %>%
  gather(key, value, -time, -state_upper, -state_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = NA)

ggsave("Figures/LinearFiltered.png")

####################
# Linear Pilot Run #
####################

pilot_run = read_csv("data/LinearPilotRun.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line() +
  geom_point() +
  ggtitle("Variance of the estimate of the pseudo-marginal log-likelihood with \n different particle amounts")

###################
# linear Forecast #
###################


