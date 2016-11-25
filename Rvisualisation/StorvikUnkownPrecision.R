library(tidyverse); library(coda); library(ggmcmc)

## Linear Gaussian Model

sims = read_csv("~/Documents/ComposableStateSpaceModels/LinearModelSims.csv", col_names = c("time", "observation", "eta", "gamma", "state"))

sims %>%
  gather(key, value, - time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line()

storvik_out = read_csv("Documents/ComposableStateSpaceModels/StorvikGaussianUnknownPrecision.csv", 
                       col_names = c("time", "state_pred", "upper_state", "lower_state", "m0", "c0", "v", "mu", "sigma", "ess"))

storvik_out %>%
  inner_join(sims, by = "time") %>%
  select(time, contains("state"), eta) %>%
  gather(key, value, -time, -upper_state, -lower_state) %>%
  ggplot() + 
  geom_line(aes(x = time, y = value, colour = key)) +
  geom_ribbon(aes(x = time, ymin = lower_state, ymax = upper_state), alpha = 0.3)

storvik_out %>%
  select(time, v) %>%
  ggplot(aes(x = time, y = v)) + 
  geom_line() +
  geom_hline(aes(yintercept = 1.0), linetype = "dashed")
