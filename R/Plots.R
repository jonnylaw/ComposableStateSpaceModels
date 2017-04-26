library(tidyverse); library(jsonlite); library(coda); library(ggmcmc); library(ggthemes); 
library(jsonlite); library(magrittr); library(gridExtra)

theme_set(theme_few())

######################
# Ornstein Uhlenbeck #
######################

ornstein_uhlenbeck = read_csv("data/ornsteinUhlenbeck.csv", col_names = c("time", sapply(1:2, function(i) paste("x", i, sep = "_"))))

ornstein_uhlenbeck %>%
  select(time, x_1, x_2) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  theme(legend.position = "none")

ggsave("src/main/resources/site/figures/ouProcess.png")

################
# NegBin Model #
################

single_sims = read_csv("data/NegBinModelSims.csv", col_names = c("time", "y", "eta", "gamma", "state"))

single_sims %>%
  select(time, y, gamma) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  theme(legend.position = "none")

ggsave("src/main/resources/site/figures/NegBinSims.png")

#####################
# Plot Neg Bin Sims #
#####################

negbin_sims = read_csv("data/NegBin/NegativeBinomial.csv",
                       col_names = c("time", "y", "eta", "gamma", sapply(1:9, function(i) paste("state", i, sep = "_"))))

negbin_sims %>%
  select(time, y, gamma) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  theme(legend.position = "none")

ggsave("src/main/resources/site/figures/ComposedNegBinSims.png")

###################################
# Filtering the Negative Binomial #
###################################

negbin_filtered = read_csv("data/NegBin/NegativeBinomialFiltered.csv",
                         col_names = c("time", "observation",
                                       "eta_hat", "eta_lower", "eta_upper",
                                       sapply(1:9, function(i) paste("state", i, "hat", sep = "_")), 
                                       sapply(1:9, function(i) c(paste("state", i, "lower", sep = "_"), paste("state", i, "upper", sep = "_")))))

negbin_filtered %>%
  inner_join(negbin_sims, by = "time") %>%
  select(contains("state_1"), time) %>%
  gather(key, value, -time, -state_1_upper, -state_1_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_1_lower, ymax = state_1_upper), alpha = 0.5, colour = "NA", fill = "#1f5081") +
  theme(legend.position = "bottom")

ggsave("src/main/resources/site/figures/NegBinFiltered.png")

###############################
# Negative Binomial Pilot Run #
###############################

negbin_pilot = read_csv("data/NegativeBinomialPilotRun.csv", col_names = c("particles", "mll_variance"))

negbin_pilot %>%
  ggplot(aes(x = particles, y = mll_variance)) +
  geom_line() +
  scale_x_log10() +
  geom_point() + 
  ggtitle("Variance of Pseudo log-likelihood")

##########################################
# Negative Binomial Parameter Estimation #
##########################################

params = c("size", "m0", "c0", "sigma", "m0_1", "c0_1", "sigma_1", "alpha", 
           sapply(1:8, function(i) paste("theta", i, sep = "_")))

chain1 = read_csv("data/NegativeBinomialPosterior-1.csv", col_names = c(params, "accepted")) %>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma), sigma_1 = exp(sigma_1), alpha = exp(alpha))
chain2 = read_csv("data/NegativeBinomialPosterior-0.csv", col_names = c(params, "accepted")) %>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma), sigma_1 = exp(sigma_1), alpha = exp(alpha))

n = max(nrow(chain1), nrow(chain2))

mcmc.list(mcmc(chain1[1:n,]), mcmc(chain2[1:n,])) %>% 
  ggs() %>% 
  ggmcmc(file = "negative_binomial.pdf")

####################
# Online Filtering #
####################

filtered = read_csv("data/NegBin/NegativeBinomialOnlineFilter.csv", 
                    col_names = c("time", "observation",
                                  "eta_hat", "eta_lower", "eta_upper",
                                  sapply(1:9, function(i) paste("state", i, "hat", sep = "_")), 
                                  sapply(1:9, function(i) c(paste("state", i, "lower", sep = "_"), paste("state", i, "upper", sep = "_")))))

filtered %>%
  inner_join(negbin_sims, by = "time") %>%
  select(contains("state_2"), time) %>%
  gather(key, value, -time, -state_2_upper, -state_2_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_2_lower, ymax = state_2_upper), alpha = 0.5, colour = "NA", fill = "#1f5081") +
  theme(legend.position = "bottom")

#################
# Interpolation #
#################

## Remove some observations of the process systematically, predict the state at that time
negbin_full = read_csv("data/NegativeBinomial.csv",
                       col_names = c("time", "observation", "eta", "gamma", sapply(1:9, function(i) paste("state", i, sep = "_"))))

negbin_full %>%
  filter(or(time < 420, time > 450)) %>%
  ggplot(aes(x = time, y = observation)) +
  geom_point() +
  ggtitle("Seasonal Negative Binomial Model with \nmissing values between t = 420 and t = 450")

ggsave("src/main/resources/site/figures/missing_values.png")

interpolated = read_csv("data/NegativeBinomialInterpolated.csv",
                        col_names = c("time", "observation",
                                      "eta_hat", "eta_lower", "eta_upper",
                                      sapply(1:9, function(i) paste("state", i, "hat", sep = "_")), 
                                      sapply(1:9, function(i) c(paste("state", i, "lower", sep = "_"), paste("state", i, "upper", sep = "_")))))

interpolated %>%
  # inner_join(negbin_full %>% select(time, y), by = "time") %>%
  select(time, observation, contains("eta")) %>%
  gather(key, value, -time, -eta_lower, -eta_upper) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = eta_lower, ymax = eta_upper), alpha = 0.5, colour = "NA") +
  theme(legend.position = "bottom") +
  ggtitle("Negative Binomial Model Interpolated, with 95% credible intervals")

ggsave("src/main/resources/site/figures/NegBinInterpolated.png")

##################
# Gaussian Model #
##################

## Simulation

gaussian_sims = read_csv("data/gaussian_sims.csv", col_names = c("time", "observation", "eta", "gamma", "state"))

gaussian_sims %>%
  select(time, observation, state) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  theme(legend.position = "bottom")

ggsave("src/main/resources/site/figures/GaussianSims.png")

## Filtering

gaussian_filtered = read_csv("data/gaussian_filtered.csv", col_names = c("time", "observation", "eta_hat", "eta_lower", "eta_upper", 
                                                                         "state_hat", "state_lower", "state_upper"))

gaussian_filtered %>%
  inner_join(gaussian_sims, by = "time") %>%
  select(time, contains("eta")) %>%
  gather(key, value, -eta_upper, -eta_lower, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = eta_lower, ymax = eta_upper), alpha = 0.5, colour = "NA") +
  theme(legend.position = "bottom")

ggsave("src/main/resources/site/figures/GaussianFiltered.png")