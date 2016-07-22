## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

#####################
# Simulate Ornstein #
#####################
system("sbt \"run-main SimulateOrnstein\"")
orn = read.csv("OrnsteinSims.csv", header = F,
               col.names = c("Time", "Value"))

png("Figures/OrnsteinSims.png")
orn %>%
  ggplot(aes(x = Time, y = Value)) + geom_line()
dev.off()

#####################
# Simulate Binomial #
#####################

system("sbt \"run-main SimulateBernoulli\"")
bern = read.csv("BernoulliSims.csv", header = F,
                col.names = c("Time", "Value", "Eta", "Gamma", "State"))

png("Figures/BernoulliSims.png")
p1 = bern %>%
  ggplot(aes(x = Time, y = Value)) + geom_step() + 
  ggtitle("Bernoulli Observations")

p2 = bern %>%
  dplyr::select(Time, Eta, State) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") + theme(legend.position="none")

grid.arrange(p1, p2)
dev.off()

#####################
# Seasonal Binomial #
#####################

system("sbt \"run-main SeasonalBernoulli\"")
bernSeas = read.csv("seasonalBernoulliSims.csv", header = F,
                    col.names = c("Time", "Value", "Eta", "Gamma", sapply(1:7, function(i) paste("State", i, sep = ""))))

png("Figures/seasonalBernoulliSims.png")
p1 = bernSeas %>%
  ggplot(aes(x = Time, y = Value)) + geom_step() + 
  ggtitle("Bernoulli Observations")

p2 = bernSeas %>%
  dplyr::select(Time, Eta, Gamma) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + 
  facet_wrap(~key, ncol = 1, scales = "free_y") + theme(legend.position="none")

p3 = bernSeas %>%
  dplyr::select(-Value, -Eta, -Gamma) %>%
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + theme(legend.position = "none")

grid.arrange(p1, p2, p3, heights = c(1,2,1))
dev.off()

####################
# Filter Bernoulli #
####################

system("sbt \"run-main FilterBernoulli\"")
bernFiltered = read.csv("BernoulliFiltered.csv", header = F,
                        col.names = c("Time", "Value", "PredEta", "lowerEta", "upperEta", "PredState", "Lower", "Upper"))

png("~/Desktop/ComposableModels/Figures/BernoulliFiltered.png")
p1 = bern %>%
  dplyr::select(-Gamma, -Eta, -Value) %>%
  inner_join(bernFiltered[,-2], by = "Time") %>%
<<<<<<< HEAD
  gather(key = "key", value = "value", -Time) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line()
dev.off()

########################
# Determine Parameters #
########################

plotIters = function(iters, variable, burnin, thin) {
  mcmcObject = mcmc(iters[seq(from = burnin, to = nrow(iters), by = thin), variable]) %>% ggs()
  
  p1 = ggs_histogram(mcmcObject)
  p2 = ggs_traceplot(mcmcObject)
  p3 = ggs_autocorrelation(mcmcObject)
  p4 = ggs_running(mcmcObject)
  
  grid.arrange(p1, p2, p3, p4)
}

system("sbt \"run-main DetermineBernoulliParameters\"")
bernMcmc = read.csv("BernoulliMCMC.csv", header = F)
colnames(bernMcmc) <- c("m0", "c0", "mu", "sigma")

## Actual Parameters m0 = 6.0, c0 = 1.0, theta = 1.0, alpha = 0.05, sigma = 1.0
=======
  gather(key = "key", value = "value", -Time, -PredEta, -lowerEta, -upperEta, -Upper, -Lower) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2)
>>>>>>> 497efe45cb6a5187b708203aa9f5ce8ba0432c8f

p2 = bern %>%
  dplyr::select(-Gamma, -State, -Value) %>%
  inner_join(bernFiltered[,-2], by = "Time") %>%
  gather(key = "key", value = "value", -Time, -PredState, -lowerEta, -upperEta, -Upper, -Lower) %>%
  ggplot(aes(x = Time, y = value, colour = key)) + geom_line() +
  geom_ribbon(aes(ymin = lowerEta, ymax = upperEta), alpha = 0.2)

<<<<<<< HEAD
# png("Figures/BernoulliMCMC.png")
plotIters(bernMcmc, 3:4, 0, 10)
# dev.off()
=======
grid.arrange(p1, p2)
dev.off()
>>>>>>> 497efe45cb6a5187b708203aa9f5ce8ba0432c8f

##############################
# Visualise Online Filtering #
##############################

# bernOnline = read.csv("OnlineBern.csv", header = F,
                      # col.names = c("Time", "Value", "Eta", "Gamma", "State"))
# bernOnlineFiltered = read.csv("~/Desktop/ComposableModels/filteredBernoulliOnline.csv")
# colnames(bernOnlineFiltered) <- c("Time", "Value", "PredState", "Lower", "Upper")

# png("~/Desktop/ComposableModels/Figures/BernoulliFilteredOnline.png")
# bernOnline %>%
#   dplyr::select(-Gamma, -Eta, -Value) %>%
#   inner_join(bernOnlineFiltered[,-2], by = "Time") %>%
#   gather(key = "key", value = "value", -Time) %>%
#   ggplot(aes(x = Time, y = value, colour = key)) + geom_line()
# dev.off()
