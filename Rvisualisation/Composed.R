## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

#############################
# Simulate Seasonal Poisson #
#############################

system("sbt \"run-main examples.SimulateSeasonalPoisson\"")
seasPois = read.csv("seasonalPoissonSims.csv", header = F,
                    col.names = c("Time", "Value", "Eta", "Gamma", sapply(1:7, function(i) paste("State", i, sep = ""))))

# png("Figures/SeasonalPoisson.png")
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
# dev.off()

##############################
# Filtering Seasonal Poisson #
##############################

system("sbt \"run-main examples.FilteringSeasonalPoisson\"")
filteredPoisson = read.csv("seasonalPoissonFiltered.csv", header = F)
colnames(filteredPoisson) = c("Time", "Observation", "PredictedEta", "lowerEta", "upperEta", sapply(1:7, function(i) paste0("PredictedState", i)), sapply(1:7, function(i) c(paste0("LowerState", i), paste0("UpperState", i))))

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

# system("sbt \"run-main examples.DetermineComposedParams\"")
# iters = read.csv("../poisson-10000-200-1.csv", header = F, col.names = c("m0", "c0", ""))