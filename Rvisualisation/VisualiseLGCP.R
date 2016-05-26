## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

#############################
# Simulate Log-Gaussian Cox #
#############################

system("cd ~/Desktop/ComposableModels/ && sbt \"run-main SimulateLGCP\"")
lgcp = read.csv("~/Desktop/ComposableModels/lgcpsims.csv", header = F,
                col.names = c("Time", "Value", "Eta", "Gamma", "State"))

png("~/Desktop/ComposableModels/Figures/LgcpSims.png")
p1 = lgcp %>%
  ggplot(aes(x = Time, y = Value)) + geom_point() + 
  ggtitle("Observed Event Times")

p2 = lgcp %>%
  dplyr::select(Time, State) %>%
  mutate(Hazard = exp(State)) %>%
  dplyr::select(Time, Hazard, State) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") + theme(legend.position="none")

grid.arrange(p1, p2)
dev.off()

######################
# Filtering for LGCP #
######################

system("cd ~/Desktop/ComposableModels/ && sbt \"run-main FilterLgcp\"")
lgcpFiltered = read.csv("~/Desktop/ComposableModels/LgcpFiltered.csv", header = F)
colnames(lgcpFiltered) <- c("Time", "Value", "PredState", "Lower", "Upper")

png("~/Desktop/ComposableModels/Figures/LgcpFiltered.png")
lgcpFiltered[,-2] %>%
  left_join(lgcp  %>% dplyr::select(-Gamma, -Eta, -Value), by = "Time") %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line()
dev.off()

#####################
# MCMC for the LGCP #
#####################

