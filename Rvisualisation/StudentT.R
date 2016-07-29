## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

######################
# Seasonal Student T #
######################

system("sbt \"run-main examples.SeasStudentT\"")
seas = read.csv("seastdistSims.csv", header = F,
                col.names = c("Time", "Observation", "Eta", "Gamma", sapply(1:7, function(i) paste0("State", i))))

seas %>%
  gather(key = "key", value = "value", -Time) %>%
    ggplot(aes(x = Time, y = value, colour = key)) + geom_line() + facet_wrap(~key)
ggsave("Figures/SeasonalTSims.pdf")

##################################
# Seasonal Students T Parameters #
##################################

system("sbt \"run-main examples.GetSeasTParams\"")

iters = read.csv("seastMCMC.csv", header = F,
                 col.names = c("m0", "c0", "V", "theta1", "alpha1", "sigma1",
                               sapply(2:7, function(i) paste0("m", i)),
                               sapply(2:7, function(i) paste0("c", i)),
                               sapply(2:7, function(i) paste0("theta", i)),
                               sapply(2:7, function(i) paste0("alpha", i)),
                               sapply(2:7, function(i) paste0("sigma", i))))

pdf("Figures/SeasonalTParameters.pdf")
plot(mcmc(iters))
dev.off()
