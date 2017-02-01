## install and load packages
library(tidyverse); library(gridExtra); library(ggmcmc); library(coda);

theme_set(theme_minimal())

#############################
# Simulate Seasonal Poisson #
#############################

system("cd ../ && sbt \"run-main com.github.jonnylaw.examples.SimulateSeasonalPoisson\"")
seasPois = read_csv("../data/seasonalPoissonSims.csv", col_names = c("Time", "Value", "Eta", "Gamma", sapply(1:7, function(i) paste("State", i, sep = ""))))

png("Figures/SeasonalPoisson.png")
p1 = seasPois %>%
  ggplot(aes(x = Time, y = Value)) + geom_step() + 
  ggtitle("Poisson Observations")

p2 = seasPois %>%
  dplyr::select(Time, Eta, Gamma) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") + theme(legend.position="none")

p3 = seasPois %>%
  dplyr::select(-Value, -Eta, -Gamma) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + theme(legend.position = "none")

grid.arrange(p1, p2, p3, heights = c(1,2,1))
dev.off()

##############################
# Filtering Seasonal Poisson #
##############################

system("cd ../ && sbt \"run-main com.github.jonnylaw.examples.FilterSeasonalPoisson\"")
filteredPoisson = read_csv("../data/seasonalPoissonFiltered.csv", 
                           col_names c("Time", "Observation", "PredictedEta", "lowerEta", "upperEta", sapply(1:7, function(i) paste0("PredictedState", i)), sapply(1:7, function(i) c(paste0("LowerState", i), paste0("UpperState", i))))

pdf("Figures/FilteredPoisson.pdf")

p1 = filteredPoisson %>%
  inner_join(seasPois, by = "Time") %>%
  dplyr::select(PredictedState1, State1, LowerState1, UpperState1, Time) %>%
  gather(key = "key", value = "value", -Time, -LowerState1, -UpperState1) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  geom_ribbon(aes(ymax = UpperState1, ymin = LowerState1), alpha = 0.1) + 
  ggtitle("Generalised Brownian Motion State for Local Level") + theme(legend.position = "bottom")

p3 = filteredPoisson %>%
  inner_join(seasPois, by = "Time") %>%
  dplyr::select(PredictedEta, lowerEta, upperEta, Eta, Time) %>%
  gather(key = "key", value = "value", -Time, -lowerEta, -upperEta) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  geom_ribbon(aes(ymax = upperEta, ymin = lowerEta), alpha = 0.2) + 
  theme(legend.position = "bottom") + ggtitle("Filtered rate of Poisson Process")

grid.arrange(p3, p1)

dev.off()

##########################
# Determining Parameters #
##########################

plotIters = function(iters, variable, thin = 10, burning = nrow(iters)*0.1) {
  mcmcObject = mcmc(iters[seq(from = burnin, to = nrow(iters), by = thin), variable]) %>% ggs()
  
  p1 = ggs_histogram(mcmcObject)
  p2 = ggs_traceplot(mcmcObject)
  p3 = ggs_autocorrelation(mcmcObject)
  p4 = ggs_running(mcmcObject)
  
  grid.arrange(p1, p2, p3, p4)
}

system("cd ../ && sbt \"run-main com.github.jonnylaw.examples.DetermineComposedParams\"")
iters = read.csv("../data/SeasonalPoissonParams.csv", header = F,
                 col.names = c("m0", "c0", "mu0", "sigma0",
                               sapply(2:7, function(i) paste0("m", i)),
                               sapply(2:7, function(i) paste0("c", i)),
                               sapply(1:6, function(i) paste0("theta", i)),
                               sapply(1:6, function(i) paste0("alpha", i)),
                               sapply(1:6, function(i) paste0("sigma", i)),
                               "accepted"))

pdf("Figures/SeasonalPoissonParams.pdf")
plotIters(iters, 3:4)
dev.off()

