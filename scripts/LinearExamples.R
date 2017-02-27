library(tidyverse); library(ggthemes); library(extrafont)

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
  facet_wrap(~key, ncol = 1, scales = "free_y") +
  ggthemes::theme_few() +
  theme(legend.position = "none", text = element_text(family = "Georgia"))
  
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
  geom_ribbon(aes(ymin = state_lower, ymax = state_upper), alpha = 0.5, colour = NA) +
  ggthemes::theme_few() +
  theme(legend.position = "none", text = element_text(family = "Georgia"))


ggsave("Figures/LinearFiltered.png")

####################
# Linear Pilot Run #
####################

pilot_run = read_csv("data/LinearPilotRun.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line() +
  geom_point() +
  ggtitle("Variance of the estimate of the\npseudo-marginal log-likelihood") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(family = "Georgia"))

###########################
# Linear Model Parameters #
###########################

params = c("v", "m0", "c0", "mu", "sigma")

actual_values = data_frame(parameter = params, actual_value = c(1.0, 0.5, 0.12, 0.01, 0.5))

chain1 = read_csv("data/LinearModelParams-1.csv", col_names = c(params, "accepted")) %>%
  mutate(chain = 1, iteration = seq_len(n()))

chain2 = read_csv("data/LinearModelParams-2.csv", col_names = c(params, "accepted")) %>%
  mutate(chain = 2, iteration = seq_len(n()))

chains = bind_rows(chain1, chain2)

plot_running_mean = function(chains, parameters = c("mu", "sigma")) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_values, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    mutate(m = mean(value), rm = cumsum(value)/iteration) %>%
    ggplot(aes(x = iteration, y = rm, colour = chain)) +
      geom_line() +
      geom_hline(aes(yintercept = m)) +
      facet_wrap(~parameter, ncol = 1, scales = "free_y") +
      geom_hline(aes(yintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
      xlab("Iteration") +
      ylab("Running Mean") +
      ggtitle("Parameters of Brownian Motion Drift and Diffusion") +
      theme_bw() +
      theme(legend.position = "none", text = element_text(family = "Georgia"))
}

plot_running_mean(chains)

traceplot = function(chains, parameters = c("mu", "sigma")) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_values, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    ggplot(aes(x = iteration, y = value, colour = chain)) + 
      geom_line() +
      facet_wrap(~parameter, scales = "free_y", ncol = 1) +
      theme_few() +
      geom_hline(aes(yintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
      theme(legend.position = "none", text = element_text(family = "Georgia"))
}

traceplot(chains)

plot_density = function(chains, parameters = c("mu", "sigma")) {
  chains %>%
    mutate(chain = as.factor(chain)) %>%
    select(-accepted) %>%
    gather(key = parameter, value, -iteration, -chain) %>%
    inner_join(actual_values, by = "parameter") %>%
    filter(parameter %in% parameters) %>%
    arrange(parameter, iteration) %>%
    drop_na() %>%
    ggplot(aes(x = value)) + 
    geom_histogram(binwidth = 0.05) +
    facet_wrap(~parameter, scales = "free", ncol = 1) +
    theme_few() +
    geom_vline(aes(xintercept = actual_value), linetype = "dashed", colour = "#ff0000") +
    theme(legend.position = "none", text = element_text(family = "Georgia"))
}

plot_density(chains)

###################
# linear Forecast #
###################

