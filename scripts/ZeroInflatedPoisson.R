library(tidyverse); library(extrafont); library(ggthemes); library(gridExtra); library(tidyjson); library(jsonlite)

theme_set(theme_solarized_2(light = FALSE))

#######################
# Parse JSON and Plot #
#######################

data = lapply(readLines("data/ZipModel.json"), fromJSON) %>% 
  bind_rows() %>%
  select(t, observation, eta) %>%
  gather(key, value, -t)

ggplot(data, aes(x = t, y = value, colour = key)) + 
  facet_wrap(~key, ncol = 1) +
  theme(legend.position = "none") +
  geom_point(data = subset(data, key == "observation")) + 
  geom_line(data = subset(data, key == "eta"))
  
#############
# Pilot Run #
#############

pilot_run = read_csv("data/PilotRunZip.csv", col_names = c("particles", "variance"))

pilot_run %>%
  ggplot(aes(x = particles, y = variance)) +
  geom_line() + geom_point()

##################
# Zip Parameters #
##################

params = c("scale", "alpha", "sigma", "theta", "m0", "c0")
actual_values = data_frame(params, actual_value = c(log(0.2), log(0.0), log(1.0), 2.0, 0.25, 0.5))

chain1 = read_csv("data/ZiPoissonPosterior.csv", col_names = c(params, "accepted"))

source("scripts/PlotMCMC.R")

chain1 %>%
  select(-accepted) %>%
  coda::mcmc() %>% plot()
