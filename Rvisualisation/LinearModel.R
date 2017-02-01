## install and load packages
packages = c("dplyr","ggplot2","gridExtra", "ggmcmc", "coda", "tidyr", "plyr")
newPackages = packages[!(packages %in% as.character(installed.packages()[,"Package"]))]
if(length(newPackages)) install.packages(newPackages)
lapply(packages, require, character.only = T)

theme_set(theme_minimal())

## Simulate the simplest linear model
## m0 = 0.1 c0 = 1.0 V = 1.0, mu = -0.2, sigma = 1.0

system("sbt \"run-main examples.SimLinear\"")

sims = read.csv("./LinearModelSims.csv", header = F,
                col.names = c("time", "y", "eta", "gamma", "x"))

sims[,-(3:4)] %>%
  gather(key = "key", value = "value", -time) %>%
  ggplot(aes(x = time, y = value, colour = key)) + geom_line()

system("scp maths:/home/a9169110/LinearModel* ~/Desktop/ComposableModels")

iters = read.csv("LinearModelGraph-0.1-200.csv", header = F,
                 col.names = c("m0", "c0", "V", "mu", "sigma", "accepted"))

mcmcObj = mcmc(iters[,-6])
