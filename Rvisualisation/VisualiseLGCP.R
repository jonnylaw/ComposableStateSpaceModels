## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

#############################
# Simulate Log-Gaussian Cox #
#############################

system("sbt \"run-main examples.SimulateLGCP\"")
lgcp = read.csv("lgcpsims.csv", header = F,
                col.names = c("Time", "Value", "Eta", "Gamma", "State"))

png("Figures/LgcpSims.png")
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

system("sbt \"run-main examples.FilterLgcp\"")
lgcpFiltered = read.csv("LgcpFiltered.csv", header = F)
colnames(lgcpFiltered) <- c("Time", "Value", "PredEta", "LowerEta", "UpperEta", "PredState", "Lower", "Upper")

png("Figures/LgcpFiltered.png")
p1 = lgcpFiltered[,-2] %>% dplyr::select(-LowerEta, -UpperEta, -PredEta) %>%
  left_join(lgcp %>% dplyr::select(-Gamma, -Eta, -Value), by = "Time") %>%
  gather(key = "key", value = "value", -Time, -Lower, -Upper) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2)

p2 = lgcpFiltered[,-2] %>% dplyr::select(-Lower, -Upper, -PredState) %>%
  left_join(lgcp  %>% dplyr::select(-Gamma, -State, -Value), by = "Time") %>%
  gather(key = "key", value = "value", -Time, -LowerEta, -UpperEta) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() +
  geom_ribbon(aes(ymin = LowerEta, ymax = UpperEta), alpha = 0.2) +
  scale_y_continuous()

grid.arrange(p1, p2)
dev.off()

#####################
# MCMC for the LGCP #
#####################

# system("sbt \"run-main examples.GetLgcpParams\"")
# iters = read.csv("~/Desktop/LgcpMCMC.csv", header = F, col.names = c("m0", "c0", "theta", "alpha", "sigma"))

## Actual values m0 = 1.0, c0 = 1.0, theta = 1.0, alpha = 0.1, sigma = 0.4

# mcmc(iters) %>% summary()
# mcmc(iters) %>% plot()
