## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

#############################
# Simulate Seasonal Poisson #
#############################

system("cd ~/Desktop/ComposableModels/ && sbt \"run-main SimulateSeasonalPoisson\"")
seasPois = read.csv("~/Desktop/ComposableModels/seasonalPoissonSims.csv", header = F,
                    col.names = c("Time", "Value", "Eta", "Gamma", sapply(1:7, function(i) paste("State", i, sep = ""))))

png("~/Desktop/ComposableModels/Figures/SeasonalPoisson.png")
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

#####################
# Online Simulation #
#####################

seasPois = read.csv("~/Desktop/ComposableModels/OnlineModel.csv", header = F,
                    col.names = c("Time", "Value", "Eta", "Gamma", sapply(1:7, function(i) paste("State", i, sep = ""))))

seasPois %>%
  ggplot(aes(x = Time, y = Value)) + geom_step()
