library(tidyverse); library(jsonlite); library(coda); library(ggmcmc); library(ggthemes); 
library(jsonlite); library(magrittr)

theme_set(theme_solarized_2(light = FALSE))

negbin_sims = read_csv("data/NegativeBinomial.csv", 
                      col_names = c("time", "y", "eta", "gamma", "state"))

#####################
# Plot Neg Bin Sims #
#####################

negbin_sims %>%
  select(time, y, eta, state) %>%
  gather(key, value, -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  theme(legend.position = "none", text = element_text(family = "Georgia"))

###################################
# Filtering the Negative Binomial #
###################################

negbin_filtered = read_csv("data/NegativebinomialFiltered.csv",
                         col_names = c("time", "observation",
                                       "eta_hat", "eta_lower", "eta_upper",
                                       "state_hat", "state_lower", "state_upper"))

negbin_filtered %>%
  inner_join(negbin_sims, by = "time") %>%
  select(contains("state"), time) %>%
  gather(key, value, -time, -state_upper, -state_lower) %>%
  ggplot(aes(x = time, y = value, colour = key)) +
  geom_line() +
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = "NA", fill = "#1f5081") +
  theme(legend.position = "bottom")

###############################
# Negative Binomial Pilot Run #
###############################

negbin_pilot = read_csv("data/NegBinPilotRun.csv", col_names = c("particles", "mll_variance"))

negbin_pilot %>%
  ggplot(aes(x = particles, y = mll_variance)) +
  geom_line() +
  geom_point() + 
  ggtitle("Variance of Pseudo log-likelihood")


##########################################
# Negative Binomial Parameter Estimation #
##########################################

params = c("size", "m0", "c0", "mu", "sigma")

actual_values = data_frame(params, value = c(3.0,0.0, 1.0, 0.3, 0.5))

chain1 = lapply(readLines("data/NegBinPosterior-1.json"), function(x) fromJSON(x)$params) %>% 
  unlist() %>%
  matrix(ncol = 5, byrow = T) %>%
  as_data_frame()

chain2 = lapply(readLines("data/NegBinPosterior-2.json"), function(x) fromJSON(x)$params) %>% 
  unlist() %>%
  matrix(ncol = 5, byrow = T) %>%
  as_data_frame()

colnames(chain1) = params
colnames(chain2) = params

chain1 %<>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma))

chain2 %<>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma))

mcmc.list(mcmc(chain1), mcmc(chain2)) %>% 
  ggs() %>% 
  ggmcmc(file = "negative_binomial.pdf")

##############################
# Tuned Parameter Estimation #
##############################

chain3 = lapply(readLines("data/NegBinPosterior-1-0.json"), function(x) fromJSON(x)$params) %>% 
  unlist() %>%
  matrix(ncol = 5, byrow = T) %>%
  as_data_frame()

chain4 = lapply(readLines("data/NegBinPosterior-1-1.json"), function(x) fromJSON(x)$params) %>% 
  unlist() %>%
  matrix(ncol = 5, byrow = T) %>%
  as_data_frame()

colnames(chain3) = params
colnames(chain4) = params

chain3 %<>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma))
chain4 %<>%
  mutate(size = exp(size), c0 = exp(c0), sigma = exp(sigma))

mcmc.list(mcmc(chain3), mcmc(chain4)) %>% 
  ggs() %>% 
  ggmcmc(file = "negative_binomial_tuned.pdf")

#########################################################
# Experimenting with the Negative Binomial Distribution #
#########################################################

qplot(x = rnbinom(1000, mu = 6, size = 10), geom = "bar")
mean = 48
size = 3
qplot(x = rnbinom(1000, size = size, prob = size / (size + mean)), geom = "bar")
